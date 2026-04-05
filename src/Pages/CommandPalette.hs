module Pages.CommandPalette (paletteShell_, commandPaletteItemsH, commandPaletteRecentPostH, RecentForm (..)) where

import Data.Text qualified as T
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (FromRow, Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful (Eff)
import Effectful.PostgreSQL qualified as PG
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (DB)
import Relude
import Servant (NoContent (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (faSprite_)
import Web.FormUrlEncoded (FromForm)


data PaletteIssue = PaletteIssue {id :: UUID, title :: Text, seqNum :: Int}
  deriving stock (Generic, Show)
  deriving anyclass (FromRow)


data PaletteItem = PaletteItem {id :: UUID, title :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromRow)


data PaletteRecent = PaletteRecent {itemType :: Text, label :: Text, url :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromRow)


data RecentForm = RecentForm {label :: Text, url :: Text, itemType :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | Returns dynamic palette items (recents, dashboards, issues, monitors) loaded lazily.
commandPaletteItemsH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
commandPaletteItemsH pid = do
  sess <- Projects.getSession
  let userId = sess.user.id
  (recents, issues, monitors, dashboards) <- fetchPaletteData pid userId
  addRespHeaders $ renderDynamicItems pid recents issues monitors dashboards


commandPaletteRecentPostH :: Projects.ProjectId -> RecentForm -> ATAuthCtx (RespHeaders NoContent)
commandPaletteRecentPostH pid form = do
  sess <- Projects.getSession
  let userId = sess.user.id
  recordRecent pid userId form
  addRespHeaders NoContent


-- DB operations

fetchPaletteData :: DB es => Projects.ProjectId -> Projects.UserId -> Eff es ([PaletteRecent], [PaletteIssue], [PaletteItem], [PaletteItem])
fetchPaletteData pid userId = do
  recents <-
    PG.query
      [sql|
    SELECT item_type, label, url FROM apis.command_palette_recents
    WHERE project_id = ? AND user_id = ? ORDER BY created_at DESC LIMIT 10 |]
      (pid, userId)
  issues <-
    PG.query
      [sql|
    SELECT id, title, seq_num FROM apis.issues
    WHERE project_id = ? AND archived_at IS NULL ORDER BY created_at DESC LIMIT 50 |]
      (Only pid)
  monitors <-
    PG.query
      [sql|
    SELECT id, alert_config->>'title' as title FROM monitors.query_monitors
    WHERE project_id = ? AND deleted_at IS NULL ORDER BY created_at DESC LIMIT 50 |]
      (Only pid)
  dashboards <-
    PG.query
      [sql|
    SELECT id, title FROM projects.dashboards
    WHERE project_id = ? ORDER BY title |]
      (Only pid)
  pure (recents, issues, monitors, dashboards)


recordRecent :: DB es => Projects.ProjectId -> Projects.UserId -> RecentForm -> Eff es ()
recordRecent pid userId form = do
  void
    $ PG.execute
      [sql|
    INSERT INTO apis.command_palette_recents (project_id, user_id, item_type, label, url)
    VALUES (?, ?, ?, ?, ?)
    ON CONFLICT (project_id, user_id, url) DO UPDATE SET created_at = now(), label = EXCLUDED.label |]
      (pid, userId, form.itemType, form.label, form.url)
  void
    $ PG.execute
      [sql|
    DELETE FROM apis.command_palette_recents
    WHERE project_id = ? AND user_id = ? AND id = ANY(
      SELECT id FROM apis.command_palette_recents
      WHERE project_id = ? AND user_id = ? ORDER BY created_at DESC OFFSET 20
    ) |]
      (pid, userId, pid, userId)


-- Rendering

-- | Palette shell rendered inline in the page (hidden). Shown via Cmd+K or search button click.
paletteShell_ :: Projects.ProjectId -> Html ()
paletteShell_ pid = do
  let pidTxt = pid.toText
      directPages = [("log_explorer", "explore", "Log Explorer"), ("api_catalog", "swap", "API Catalog"), ("reports", "chart-simple", "Reports"), ("settings", "gear", "Settings")] :: [(Text, Text, Text)]
  -- Global palette functions + Cmd+K listener (must be on a visible element)
  span_ [id_ "cmd-palette-global", paletteGlobalScript] ""
  -- Backdrop (hidden by default, shown on Cmd+K / search click)
  div_
    [ id_ "cmd-palette-backdrop"
    , class_ "cmd-palette-backdrop hidden fixed inset-0 flex flex-col items-center pt-[15vh] bg-black/40"
    , style_ "z-index:99999"
    , [__|on click if event.target is me add .hidden to me end
          on htmx:beforeRequest from <body/> add .hidden to me end|]
    ]
    do
      -- Header
      div_ [class_ "w-full max-w-lg flex items-center justify-between mb-2 px-3"] do
        span_ [class_ "text-2xs font-medium text-white dark:text-white/70 uppercase tracking-wider"] "Quick Search"
        span_ [class_ "cmd-palette-count text-xs text-white/80 dark:text-white/50"] ""
      -- Panel
      div_ [class_ "cmd-palette w-full max-w-lg bg-base-100 rounded-lg shadow-2xl border border-base-300 overflow-hidden", data_ "pid" pidTxt, data_ "current-category" "", drillInitScript] do
        -- Search input
        div_ [class_ "px-3 border-b border-base-300"] do
          input_
            [ type_ "text"
            , id_ "cmd-palette-input"
            , class_ "w-full py-2.5 bg-transparent outline-none text-sm"
            , placeholder_ "Search pages, issues, actions\x2026"
            , filterScript
            ]
        -- Back breadcrumb (hidden by default)
        div_ [class_ "cmd-breadcrumb hidden items-center gap-1.5 px-3 py-1.5 border-b border-base-300 text-xs text-base-content/60 cursor-pointer hover:text-base-content/80", [__|on click call goBackToRoot(me.closest('.cmd-palette')) then set #cmd-palette-input.placeholder to 'Search pages, issues, actions…'|]] do
          faSprite_ "chevron-left" "regular" "w-3 h-3"
          span_ [class_ "cmd-breadcrumb-label"] ""
        -- Results container
        div_ [id_ "cmd-palette-results", class_ "max-h-80 overflow-y-auto p-1"] do
          -- Lazy-loaded dynamic items placeholder
          div_ [id_ "cmd-palette-dynamic", hxGet_ ("/p/" <> pidTxt <> "/command-palette"), hxTrigger_ "palette:open from:body", hxSwap_ "outerHTML", class_ "text-center text-xs text-textWeak py-2"] "Loading..."
          -- Direct page links (static, always present)
          forM_ directPages \(path, icon, label) ->
            cmdItem pidTxt "direct" path [] [] icon label "Page"
          -- Logs shortcut
          a_
            [ class_ "cmd-item flex items-center gap-2 px-3 py-2 rounded text-sm cursor-pointer transition-colors"
            , data_ "search" "search logs"
            , data_ "cmd-type" "direct"
            , href_ $ "/p/" <> pidTxt <> "/log_explorer"
            , data_ "log-shortcut" "true"
            ]
            do
              faSprite_ "explore" "regular" "w-3.5 h-3.5 text-textWeak shrink-0"
              span_ [class_ "cmd-log-label truncate flex-1"] "Search logs for: \"\""
              badge_ "Logs"
          -- Actions
          cmdItem pidTxt "direct" "log_explorer#create-alert-toggle" [] [] "plus" "Create monitor" "Action"
          cmdItem pidTxt "direct" "dashboards?new=true" [] [] "plus" "Create dashboard" "Action"
          a_
            [ class_ "cmd-item flex items-center gap-2 px-3 py-2 rounded text-sm cursor-pointer transition-colors"
            , data_ "search" "switch project"
            , data_ "cmd-type" "direct"
            , href_ "/"
            ]
            do
              faSprite_ "grid" "regular" "w-3.5 h-3.5 text-textWeak shrink-0"
              span_ [class_ "truncate flex-1"] "Switch project"
              badge_ "Action"
          a_
            [ class_ "cmd-item flex items-center gap-2 px-3 py-2 rounded text-sm cursor-pointer transition-colors"
            , data_ "search" "copy current url"
            , data_ "cmd-type" "direct"
            , data_ "action" "copy-url"
            , [__|on click js navigator.clipboard.writeText(window.location.href); end then add .hidden to #cmd-palette-backdrop|]
            ]
            do
              faSprite_ "copy" "regular" "w-3.5 h-3.5 text-textWeak shrink-0"
              span_ [class_ "truncate flex-1"] "Copy current URL"
              badge_ "Action"
          -- AI row (hidden initially)
          div_ [class_ "cmd-ai-row", style_ "display:none"]
            $ a_
              [ class_ "cmd-item flex items-center gap-2 px-3 py-2 rounded text-sm cursor-pointer transition-colors"
              , data_ "search" ""
              , data_ "cmd-type" "direct"
              , data_ "ai-action" "true"
              , [__|on click
                  set :q to #cmd-palette-input.value
                  fetch `/p/${me.closest('.cmd-palette').dataset.pid}/log_explorer/ai_search` {method:'POST', headers:{'Content-Type':'application/json'}, body: JSON.stringify({input: :q})}
                  then set :r to it.json()
                  then set window.location to `/p/${me.closest('.cmd-palette').dataset.pid}/log_explorer?query=${encodeURIComponent(:r.query)}`
               |]
              ]
              do
                faSprite_ "sparkles" "regular" "w-3.5 h-3.5 text-textWeak shrink-0"
                span_ [class_ "cmd-ai-label truncate flex-1"] "Ask AI: \"\""
                badge_ "AI"
          -- Empty state
          div_
            [class_ "cmd-palette-empty px-3 py-8 text-center text-sm text-base-content/40", style_ "display:none"]
            "No matching results"
      -- Keyboard hints
      div_ [class_ "cmd-palette-hints flex items-center gap-6 mt-3 text-xs text-white dark:text-white/80 drop-shadow"] do
        div_ [class_ "flex items-center gap-1.5"] $ "Navigate" >> kbd_ [class_ "kbd kbd-xs"] "\x2191" >> kbd_ [class_ "kbd kbd-xs"] "\x2193"
        div_ [class_ "flex items-center gap-1.5"] $ "Open" >> kbd_ [class_ "kbd kbd-xs"] "\x21B5" >> kbd_ [class_ "kbd kbd-xs"] "\x2192"
        div_ [class_ "flex items-center gap-1.5"] $ "Back" >> kbd_ [class_ "kbd kbd-xs"] "\x2190"
        div_ [class_ "flex items-center gap-1.5"] $ "Close" >> kbd_ [class_ "kbd kbd-xs"] "esc"


-- | Dynamic items fragment — loaded lazily into the shell
renderDynamicItems :: Projects.ProjectId -> [PaletteRecent] -> [PaletteIssue] -> [PaletteItem] -> [PaletteItem] -> Html ()
renderDynamicItems pid recents issues monitors dashboards = do
  let pidTxt = pid.toText
  div_
    [ id_ "cmd-palette-dynamic"
    , [__|init
      set :palette to closest .cmd-palette
      set :counter to the first <.cmd-palette-count/>
      if :counter then
        set :total to (<a.cmd-item/> in :palette).length
        set :counter.dataset.total to :total
        put `${:total} items` into :counter
      end
      for item in <a.cmd-item/> in :palette remove .active from item end
      set :f to the first <a.cmd-item:not([style*='display: none']):not([style*='display:none'])/> in :palette
      if :f then add .active to :f end
    |]
    ]
    do
      -- Recents
      unless (null recents)
        $ div_ [class_ "cmd-section cmd-palette-recents"]
        $ forM_ recents (recentItem pidTxt)
      -- Category: Dashboards
      categoryItem pidTxt "dashboards" "dashboard" "Dashboards" (length dashboards + 1)
      cmdItem pidTxt "child" "dashboards" [] [data_ "cmd-category" "dashboards", style_ "display:none"] "dashboard" "All Dashboards" "Page"
      forM_ dashboards \d ->
        cmdItem pidTxt "child" ("dashboards/" <> show d.id) [] [data_ "cmd-category" "dashboards", style_ "display:none"] "dashboard" (if T.null d.title then "Untitled Dashboard" else d.title) "Dashboard"
      -- Category: Issues
      categoryItem pidTxt "issues" "bug" "Issues" (length issues)
      forM_ issues \i ->
        cmdItem pidTxt "child" ("issues/" <> show i.id) [] [data_ "cmd-category" "issues", style_ "display:none"] "bug" i.title "Issue"
      -- Category: Monitors
      categoryItem pidTxt "monitors" "list-check" "Monitors" (length monitors)
      forM_ monitors \m ->
        cmdItem pidTxt "child" ("monitors?highlight=" <> show m.id) [] [data_ "cmd-category" "monitors", style_ "display:none"] "list-check" m.title "Monitor"


-- | Global Cmd+K listener + drill helpers — on a visible element so init runs
paletteGlobalScript :: Attribute
paletteGlobalScript =
  [__|on keydown[key=='k' and (metaKey or ctrlKey)] from window
      halt the event
      send paletteToggle to me
    end
    on paletteToggle
      if #cmd-palette-backdrop.classList.contains('hidden')
        remove .hidden from #cmd-palette-backdrop
        set #cmd-palette-input.value to ''
        call #cmd-palette-input.focus()
        send palette:open to <body/>
      else
        add .hidden to #cmd-palette-backdrop
      end
    end
    init js
      window.goBackToRoot = function(palette) {
        palette.dataset.currentCategory = '';
        palette.querySelectorAll('[data-cmd-type="direct"], [data-cmd-type="category"]').forEach(function(el) { el.style.display = ''; });
        palette.querySelectorAll('[data-cmd-type="child"]').forEach(function(el) { el.style.display = 'none'; });
        var recents = palette.querySelector('.cmd-palette-recents');
        if (recents) recents.style.display = '';
        var bc = palette.querySelector('.cmd-breadcrumb');
        if (bc) { bc.classList.add('hidden'); bc.classList.remove('flex'); }
        var input = palette.querySelector('#cmd-palette-input');
        if (input) { input.value = ''; input.focus(); }
        palette.querySelectorAll('a.cmd-item').forEach(function(el) { el.classList.remove('active'); });
        var first = palette.querySelector('a.cmd-item:not([style*="display: none"]):not([style*="display:none"])');
        if (first) first.classList.add('active');
      };
      window.drillIntoCategory = function(palette, category) {
        palette.dataset.currentCategory = category;
        palette.querySelectorAll('[data-cmd-type="direct"], [data-cmd-type="category"]').forEach(function(el) { el.style.display = 'none'; });
        var recents = palette.querySelector('.cmd-palette-recents');
        if (recents) recents.style.display = 'none';
        palette.querySelectorAll('[data-cmd-type="child"][data-cmd-category="' + category + '"]').forEach(function(el) { el.style.display = ''; });
        var bc = palette.querySelector('.cmd-breadcrumb');
        if (bc) {
          bc.classList.remove('hidden'); bc.classList.add('flex');
          var lbl = bc.querySelector('.cmd-breadcrumb-label');
          if (lbl) lbl.textContent = category.charAt(0).toUpperCase() + category.slice(1);
        }
        var input = palette.querySelector('#cmd-palette-input');
        if (input) { input.value = ''; input.placeholder = 'Search ' + category + '\u2026'; input.focus(); }
        palette.querySelectorAll('a.cmd-item').forEach(function(el) { el.classList.remove('active'); });
        var first = palette.querySelector('a.cmd-item[data-cmd-category="' + category + '"]:not([style*="display: none"]):not([style*="display:none"])');
        if (first) first.classList.add('active');
      };
    end
  |]


-- | Prevents click events from bubbling through the palette panel
drillInitScript :: Attribute
drillInitScript = [__|on click halt the event's bubbling end|]


-- Hyperscript for input filtering + keyboard nav
filterScript :: Attribute
filterScript =
  [__|on input
      set :q to my value.toLowerCase()
      set :palette to closest .cmd-palette
      set :cat to :palette.dataset.currentCategory
      -- Hide recents when typing, show when empty (only at root)
      if :cat === '' then
        if :q.length > 0 then hide <.cmd-palette-recents/> in :palette else show <.cmd-palette-recents/> in :palette end
      end
      -- Filter based on current view
      if :cat !== '' then
        -- Drilled in: filter only children of this category
        for item in <a.cmd-item[data-cmd-type='child']/> in :palette
          if item.dataset.cmdCategory === :cat then
            set :t to item.dataset.search.toLowerCase()
            if :q.length === 0 or :t.includes(:q) then show item else hide item end
          end
        end
      else
        -- Root: filter direct + category items
        for item in <a.cmd-item/> in :palette
          set :typ to item.dataset.cmdType
          if :typ === 'child' then
            -- Show matching children inline during global search
            if :q.length > 0 then
              set :t to item.dataset.search.toLowerCase()
              if :t.includes(:q) then show item else hide item end
            else
              hide item
            end
          else
            set :t to item.dataset.search.toLowerCase()
            set :fz to true
            if :q.length > 0 and not :t.includes(:q) then
              set :qi to 0
              set :ti to 0
              repeat while :qi < :q.length and :ti < :t.length
                if :q.charAt(:qi) === :t.charAt(:ti) then increment :qi end
                increment :ti
              end
              if :qi < :q.length then set :fz to false end
            end
            if :q.length === 0 or :fz then show item else hide item end
          end
        end
      end
      -- Reset active and highlight best match
      for item in <a.cmd-item/> in :palette remove .active from item end
      set :first to the first <a.cmd-item:not([style*='display: none']):not([style*='display:none'])/> in :palette
      if :first then add .active to :first end
      -- Update result count
      set :counter to the first <.cmd-palette-count/>
      set :total to :counter.dataset.total
      set :visCount to <a.cmd-item:not([style*='display: none']):not([style*='display:none'])/> in :palette
      if :q.length > 0 then put `${:visCount.length} of ${:total} items` into :counter
      else put `${:total} items` into :counter end
      -- AI row: show when query > 10 chars (root only)
      if :q.length > 10 and :cat === '' then show <.cmd-ai-row/> else hide <.cmd-ai-row/> end
      set :aiLabel to the first <.cmd-ai-label/>
      if :aiLabel then put `Ask AI: "${my value}"` into :aiLabel end
      -- Log shortcut: update label
      for el in <.cmd-log-label/> put `Search logs for: "${my value}"` into el end
      set :ell to the first <.cmd-empty-log-label/>
      if :ell then put `Search logs for: "${my value}"` into :ell end
      set :eal to the first <.cmd-empty-ai-label/>
      if :eal then put `Ask AI: "${my value}"` into :eal end
      if :q.length > 10 then show <.cmd-empty-ai/> else hide <.cmd-empty-ai/> end
      -- Update log shortcut hrefs
      for el in <[data-log-shortcut]/> set el.href to `/p/${el.closest('.cmd-palette').dataset.pid}/log_explorer?query=${encodeURIComponent(my value)}` end
      -- Empty state
      set :allVisible to <a.cmd-item:not([style*='display: none']):not([style*='display:none'])/> in :palette
      set :empty to the first <.cmd-palette-empty/> in :palette
      if :allVisible.length === 0 and :q.length > 0 then show :empty else hide :empty end
    end
    on keydown[key=='Escape']
      set :palette to closest .cmd-palette
      set :cat to :palette.dataset.currentCategory
      if :cat !== '' then
        call goBackToRoot(:palette)
        set me.placeholder to 'Search pages, issues, actions…'
      else
        add .hidden to #cmd-palette-backdrop
      end
    end
    on keydown[key=='Enter']
      set :a to the first <a.active/> in closest .cmd-palette
      if :a and :a.dataset.cmdType === 'category' then
        call drillIntoCategory(closest .cmd-palette, :a.dataset.cmdCategory)
      else if :a then
        call :a.click()
      end
    end
    on keydown[key=='Backspace']
      if my value === '' then
        set :palette to closest .cmd-palette
        set :cat to :palette.dataset.currentCategory
        if :cat !== '' then
          halt the event
          call goBackToRoot(:palette)
          set me.placeholder to 'Search pages, issues, actions…'
        end
      end
    end
    on keydown[key=='ArrowLeft']
      if my value === '' then
        set :palette to closest .cmd-palette
        set :cat to :palette.dataset.currentCategory
        if :cat !== '' then
          halt the event
          call goBackToRoot(:palette)
          set me.placeholder to 'Search pages, issues, actions…'
        end
      end
    end
    on keydown[key=='ArrowRight']
      if my value === '' then
        set :a to the first <a.active/> in closest .cmd-palette
        if :a and :a.dataset.cmdType === 'category' then
          halt the event
          call drillIntoCategory(closest .cmd-palette, :a.dataset.cmdCategory)
        end
      end
    end
    on keydown[key=='ArrowDown'] halt the event
      set :all to <a.cmd-item:not([style*='display: none']):not([style*='display:none'])/> in closest .cmd-palette
      set :a to the first <a.active/> in closest .cmd-palette
      if :a then
        set :idx to -1
        set :i to 0
        for el in :all
          if el === :a then set :idx to :i end
          increment :i
        end
        if :idx >= 0 and :idx < :all.length - 1 then
          remove .active from :a
          add .active to :all[:idx + 1]
          call :all[:idx + 1].scrollIntoView({block:'nearest'})
        end
      else
        if :all.length > 0 then add .active to :all[0] end
      end
    end
    on keydown[key=='ArrowUp'] halt the event
      set :all to <a.cmd-item:not([style*='display: none']):not([style*='display:none'])/> in closest .cmd-palette
      set :a to the first <a.active/> in closest .cmd-palette
      if :a then
        set :idx to -1
        set :i to 0
        for el in :all
          if el === :a then set :idx to :i end
          increment :i
        end
        if :idx > 0 then
          remove .active from :a
          add .active to :all[:idx - 1]
          call :all[:idx - 1].scrollIntoView({block:'nearest'})
        end
      end
    end
  |]


-- Item helpers

-- | Shared recent-tracking hyperscript for items that record recent clicks
recentClickScript :: Attribute
recentClickScript =
  [__|on click
        set :ru to my.dataset.recentUrl
        js(me) var p = new URLSearchParams(); p.set('label', me.dataset.recentLabel); p.set('url', me.getAttribute('href')); p.set('itemType', me.dataset.recentType); return p.toString(); end
        then set :rb to it
        fetch `${:ru}` {method:'POST', headers:{'Content-Type':'application/x-www-form-urlencoded'}, body: :rb}
     |]


-- | Base item renderer — unifies direct, child, and action items
cmdItem :: Text -> Text -> Text -> [Attribute] -> [Attribute] -> Text -> Text -> Text -> Html ()
cmdItem pidTxt cmdType path recentAttrs extraAttrs icon label badgeText =
  let url = "/p/" <> pidTxt <> "/" <> path
      recentUrl = "/p/" <> pidTxt <> "/command-palette/recents"
      baseAttrs =
        [ class_ "cmd-item flex items-center gap-2 px-3 py-2 rounded text-sm cursor-pointer transition-colors"
        , data_ "search" (T.toLower label)
        , data_ "cmd-type" cmdType
        , href_ url
        ]
      trackAttrs
        | null recentAttrs =
            [ data_ "recent-url" recentUrl
            , data_ "recent-label" label
            , data_ "recent-type" (T.toLower badgeText)
            , recentClickScript
            ]
        | otherwise = recentAttrs
   in a_ (baseAttrs <> trackAttrs <> extraAttrs) do
        faSprite_ icon "regular" "w-3.5 h-3.5 text-textWeak shrink-0"
        span_ [class_ "truncate flex-1"] $ toHtml label
        badge_ badgeText


categoryItem :: Text -> Text -> Text -> Text -> Int -> Html ()
categoryItem pidTxt category icon label count =
  a_
    [ class_ "cmd-item flex items-center gap-2 px-3 py-2 rounded text-sm cursor-pointer transition-colors"
    , data_ "search" (T.toLower label)
    , data_ "cmd-type" "category"
    , data_ "cmd-category" category
    , href_ "#"
    , [__|on click halt the event then call drillIntoCategory(closest .cmd-palette, my.dataset.cmdCategory)|]
    ]
    do
      faSprite_ icon "regular" "w-3.5 h-3.5 text-textWeak shrink-0"
      span_ [class_ "truncate flex-1"] $ toHtml label
      span_ [class_ "badge badge-ghost badge-xs text-2xs tabular-nums"] $ toHtml (show count)
      faSprite_ "chevron-right" "regular" "w-3 h-3 text-base-content/30 shrink-0"


recentItem :: Text -> PaletteRecent -> Html ()
recentItem pidTxt r =
  let icon = case r.itemType of
        "page" -> "file-lines"
        "issue" -> "bug"
        "monitor" -> "list-check"
        "dashboard" -> "dashboard"
        _ -> "clock"
      recentUrl = "/p/" <> pidTxt <> "/command-palette/recents"
   in a_
        [ class_ "cmd-item flex items-center gap-2 px-3 py-2 rounded text-sm cursor-pointer transition-colors"
        , data_ "search" (T.toLower r.label)
        , data_ "cmd-type" "direct"
        , href_ r.url
        , data_ "recent-url" recentUrl
        , data_ "recent-label" r.label
        , data_ "recent-type" r.itemType
        , recentClickScript
        ]
        do
          faSprite_ icon "regular" "w-3.5 h-3.5 text-textWeak shrink-0"
          span_ [class_ "truncate flex-1"] $ toHtml r.label
          badge_ $ T.toTitle r.itemType


badge_ :: Monad m => Text -> HtmlT m ()
badge_ t = span_ [class_ "badge badge-ghost badge-xs text-2xs"] $ toHtml t
