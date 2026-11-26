module Pages.CommandPalette (paletteShell_, commandPaletteItemsH, commandPaletteRecentPostH, RecentForm (..)) where

import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.List (lookup)
import Data.Text qualified as T
import Data.UUID (UUID)
import Hasql.Interpolate qualified as HI
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Pages.Components (PickerCfg (..), PickerItem (..), PickerList (..), logExplorerUrl, pickerBadge_, pickerCard_, pickerEmpty_, pickerIcon_, pickerItem_, pickerOption_)
import Pkg.DeriveUtils (UUIDId (..))
import Relude
import Servant (NoContent (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (faSprite_, fmtInt)
import Web.FormUrlEncoded (FromForm)
import Web.Links


data PaletteItem = PaletteItem {id :: UUID, title :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data PaletteRecent = PaletteRecent {itemType :: Text, label :: Text, url :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data RecentForm = RecentForm {label :: Text, url :: Text, itemType :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | Returns dynamic palette items (recents, dashboards, issues, monitors) loaded lazily.
commandPaletteItemsH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
commandPaletteItemsH pid = do
  (sess, _) <- Projects.sessionAndProject pid
  recents <-
    Hasql.interp
      [HI.sql|
    SELECT item_type, label, url FROM apis.command_palette_recents
    WHERE project_id = #{pid} AND user_id = #{sess.user.id} ORDER BY created_at DESC LIMIT 10 |]
  issues <-
    Hasql.interp
      [HI.sql|
    SELECT id, title FROM apis.issues
    WHERE project_id = #{pid} AND archived_at IS NULL ORDER BY created_at DESC LIMIT 50 |]
  monitors <-
    Hasql.interp
      [HI.sql|
    SELECT id, alert_config->>'title' as title FROM monitors.query_monitors
    WHERE project_id = #{pid} AND deleted_at IS NULL ORDER BY created_at DESC LIMIT 50 |]
  dashboards <-
    Hasql.interp
      [HI.sql|
    SELECT id, title FROM projects.dashboards
    WHERE project_id = #{pid} ORDER BY title |]
  addRespHeaders $ renderDynamicItems pid recents issues monitors dashboards


commandPaletteRecentPostH :: Projects.ProjectId -> RecentForm -> ATAuthCtx (RespHeaders NoContent)
commandPaletteRecentPostH pid form = do
  (sess, _) <- Projects.sessionAndProject pid
  void
    $ Hasql.interpExecute
      [HI.sql|
    INSERT INTO apis.command_palette_recents (project_id, user_id, item_type, label, url)
    VALUES (#{pid}, #{sess.user.id}, #{form.itemType}, #{form.label}, #{form.url})
    ON CONFLICT (project_id, user_id, url) DO UPDATE SET created_at = now(), label = EXCLUDED.label |]
  void
    $ Hasql.interpExecute
      [HI.sql|
    DELETE FROM apis.command_palette_recents
    WHERE project_id = #{pid} AND user_id = #{sess.user.id} AND id = ANY(
      SELECT id FROM apis.command_palette_recents
      WHERE project_id = #{pid} AND user_id = #{sess.user.id} ORDER BY created_at DESC OFFSET 20
    ) |]
  addRespHeaders NoContent


-- Rendering

-- | Palette shell rendered inline in the page (hidden). Shown via Cmd+K or search button click.
paletteShell_ :: Projects.ProjectId -> Html ()
paletteShell_ pid = do
  -- Global palette functions + Cmd+K listener (must be on a visible element)
  span_ [id_ "cmd-palette-global", paletteGlobalScript] ""
  -- Native modal <dialog> (top-layer, built-in Escape and backdrop dismissal)
  term
    "dialog"
    [ id_ "cmd-palette-backdrop"
    , Aria.label_ "Command palette"
    , class_ "cmd-palette-backdrop modal !items-start px-4 pt-[15vh] bg-black/40 dark:bg-black/65 backdrop:backdrop-blur-[2px] [&_.modal-backdrop]:backdrop-blur-[2px]"
    , [__|on cancel
            set p to the first .cmd-palette in me
            if p.dataset.currentCategory halt the event then send cmdNav(cat:'') to #cmd-palette-input end
          end
          on htmx:before:request from <body/> if my.open and event.target.id is not 'cmd-palette-dynamic' call me.close() end|]
    ]
    do
      div_ [class_ "modal-box !w-full !max-w-lg !overflow-visible !rounded-none !border-0 !bg-transparent !p-0 !shadow-none"]
        $ pickerCard_
          def
            { cardClass = "cmd-palette w-full max-w-lg"
            , cardAttrs = [data_ "pid" pid.toText, data_ "current-category" ""]
            , inputLabel = Just "Search or ask AI"
            , placeholder = "Ask AI, or search pages, issues, actions\x2026"
            , inputAttrs =
                [ id_ "cmd-palette-input"
                , term "hx-live:placeholder" "(c => c ? 'Search ' + c + '\x2026' : 'Ask AI, or search pages, issues, actions\x2026')(closest('.cmd-palette').data.currentCategory)"
                , inputScript
                ]
            , beforeList =
                div_ [hidden_ "", term "hx-live:hidden" "!closest('.cmd-palette').data.currentCategory", class_ "cmd-breadcrumb flex [&[hidden]]:hidden items-center gap-1.5 px-3 py-1.5 border-b border-strokeWeak text-xs text-textWeak cursor-pointer hover:text-textStrong", [__|on click send cmdNav(cat:'') to #cmd-palette-input|]] do
                  faSprite_ "chevron-left" "regular" "w-3 h-3"
                  span_ [term "hx-live:text" "(c => c.charAt(0).toUpperCase() + c.slice(1))(closest('.cmd-palette').data.currentCategory || '')"] ""
            , hints = [("Navigate", ["\x2191", "\x2193"]), ("Open", ["\x21B5", "\x2192"]), ("Back", ["\x2190"]), ("Close", ["esc"])]
            }
        $ PickerItems "max-h-80" [id_ "cmd-palette-results"] do
          -- The assistant leads the palette, so typing then Enter asks it; ↓ reaches logs and pages.
          cmdLink_
            ""
            CKAiAction
            [ hxPost_ $ urls.aiDockPost pid
            , hxTrigger_ "click[document.getElementById('cmd-palette-input').value.trim()]"
            , hxVals_ "js:{query: document.getElementById('cmd-palette-input').value, allowActions: true, path: location.pathname + location.search, title: document.title}"
            , hxTarget_ "#ai-dock"
            , hxSwap_ "outerMorph"
            , [__|on htmx:after:request set #ai-dock-open.checked to true then if #ai-dock-input exists call #ai-dock-input.focus() end|]
            ]
            do
              faSprite_ "sparkles" "regular" "w-3.5 h-3.5 shrink-0 text-iconBrand"
              span_ [class_ "truncate flex-1 font-medium text-textStrong", term "hx-live:text" "(v => v.trim() ? 'Ask AI: \"' + v + '\"' : 'Ask AI to find, create or change anything')(q('#cmd-palette-input').value)"] "Ask AI to find, create or change anything"
              span_ [class_ "badge badge-xs border-0 bg-fillBrand-weak text-2xs text-textBrand"] "AI"
          -- Logs shortcut
          cmdRow_ "search logs" CKLogShortcut [href_ $ logExplorerUrl pid, term "hx-live:href" "'/p/' + closest('.cmd-palette').dataset.pid + '/log_explorer?query=' + encodeURIComponent(q('#cmd-palette-input').value)"] "explore" [term "hx-live:text" "'Search logs for: \"' + q('#cmd-palette-input').value + '\"'"] "Search logs for: \"\"" "Logs"
          -- Lazy-loaded dynamic items placeholder
          div_ [id_ "cmd-palette-dynamic", hxGet_ (urls.commandPaletteGet pid), hxTrigger_ "palette:open from:body", hxSwap_ "outerHTML", class_ "text-center text-xs text-textWeak py-2"] "Loading..."
          -- Direct page links (static, always present)
          forM_ @[] @_ @(Text, Text, Text) [(logExplorerUrl pid, "explore", "Log Explorer"), (urls.apiCatalogGet pid Nothing Nothing Nothing Nothing Nothing Nothing Nothing, "swap", "API Catalog"), (urls.aiRoutinesGet pid, "clock", "Routines and reports"), (urls.repositoriesGet pid Nothing Nothing, "code-branch", "Repositories"), (urls.projects.settingsGet pid, "gear", "Settings")] \(href, icon, label) ->
            cmdItem pid CKDirect href [] icon label "Page"
          -- Actions
          cmdItem pid CKDirect (logExplorerUrl pid <> "#create-alert-toggle") [] "plus" "Create monitor" "Action"
          cmdItem pid CKDirect (urls.dashboardsGetList pid Nothing Nothing Nothing Nothing Nothing (Just "true") def) [] "plus" "Create dashboard" "Action"
          cmdRow_ "switch project" CKDirect [href_ "/"] "grid" [] "Switch project" "Action"
          cmdRow_
            "copy current url"
            CKDirect
            [ data_ "action" "copy-url"
            , [__|on click call navigator.clipboard.writeText(window.location.href) then call #cmd-palette-backdrop.close()|]
            ]
            "copy"
            []
            "Copy current URL"
            "Action"
          pickerEmpty_ "No matching results"
      form_ [method_ "dialog", class_ "modal-backdrop"] $ button_ [Aria.label_ "Close"] "close"
  where
    -- Global Cmd+K listener — on a visible element so it is processed. A letter typed where
    -- no field or player takes it opens the palette with that letter.
    paletteGlobalScript =
      [__|on keydown[key=='k' and (metaKey or ctrlKey)] from window
          halt the event
          send paletteToggle to me
        end
        on keydown[key.length == 1 and key.toLowerCase() != key.toUpperCase() and not (metaKey or ctrlKey or altKey or defaultPrevented or target.isContentEditable) and not target.closest('input, textarea, select, .monaco-editor') and not document.querySelector('dialog[open]')] from window
          halt the event
          send paletteToggle to me
          set #cmd-palette-input.value to event.key
          send input to #cmd-palette-input
        end
        on paletteToggle
          if not #cmd-palette-backdrop.open
            call #cmd-palette-backdrop.showModal()
            set #cmd-palette-input.value to ''
            call #cmd-palette-input.focus()
            send palette:open to <body/>
          else
            call #cmd-palette-backdrop.close()
          end
        end
      |]
    -- Category drill-in/back; filtering and arrow/Enter nav live on the items and listboxNavAttr_
    inputScript =
      [__|on cmdNav(cat) set p to closest .cmd-palette then set p.dataset.currentCategory to cat then set my value to '' then call me.focus() then send input to me end
        on keydown[(key=='Backspace' or key=='ArrowLeft') and not target.value and target.closest('.cmd-palette').dataset.currentCategory] halt the event then send cmdNav(cat:'') to me end
        on keydown[key=='ArrowRight' and not target.value]
          set a to the first <a.active[data-cmd-type=category]/> in closest .cmd-palette
          if a halt the event then call a.click() end
        end
      |]


-- | Dynamic items fragment — loaded lazily into the shell
renderDynamicItems :: Projects.ProjectId -> [PaletteRecent] -> [PaletteItem] -> [PaletteItem] -> [PaletteItem] -> Html ()
renderDynamicItems pid recents issues monitors dashboards =
  div_
    [ id_ "cmd-palette-dynamic"
    , [__|init send input to #cmd-palette-input|]
    ]
    do
      unless (null recents)
        $ div_ [class_ "cmd-palette-recents"]
        $ forM_ recents recentItem
      section
        "dashboards"
        "dashboard"
        "Dashboards"
        "Dashboard"
        [cmdItem pid CKChild (urls.dashboardsGetList pid Nothing Nothing Nothing Nothing Nothing Nothing def) [data_ "cmd-category" "dashboards", hidden_ ""] "dashboard" "All Dashboards" "Page"]
        [(urls.dashboardsGet pid (UUIDId d.id) Nothing Nothing Nothing Nothing [], if T.null d.title then "Untitled Dashboard" else d.title) | d <- dashboards]
      section "issues" "bug" "Issues" "Issue" [] [((urls.issues pid).detailGet (UUIDId i.id) Nothing Nothing Nothing Nothing Nothing, i.title) | i <- issues]
      section "monitors" "list-check" "Monitors" "Monitor" [] [((urls.monitors pid).listGet Nothing Nothing <> "?highlight=" <> show m.id, m.title) | m <- monitors]
  where
    section category icon label badge extras items = do
      categoryItem category icon label (length items + length extras)
      sequence_ extras
      forM_ items \(href, title) ->
        cmdItem pid CKChild href [data_ "cmd-category" category, hidden_ ""] icon title badge

    categoryItem :: Text -> Text -> Text -> Int -> Html ()
    categoryItem category icon label count =
      cmdLink_
        (T.toLower label)
        CKCategory
        [ data_ "cmd-category" category
        , href_ "#"
        , [__|on click halt the event then send cmdNav(cat: my.dataset.cmdCategory) to #cmd-palette-input|]
        ]
        do
          pickerIcon_ icon
          span_ [class_ "truncate flex-1"] $ toHtml label
          span_ [class_ "badge badge-ghost badge-xs text-2xs tabular-nums"] $ toHtml $ fmtInt count
          faSprite_ "chevron-right" "regular" "w-3 h-3 text-iconNeutral shrink-0"

    recentItem :: PaletteRecent -> Html ()
    recentItem r =
      cmdRow_ (T.toLower r.label) CKRecent (href_ r.url : recentAttrs pid r.label r.itemType) icon [] r.label (T.toTitle r.itemType)
      where
        icon = fromMaybe "clock" $ lookup r.itemType [("page", "file-lines"), ("issue", "bug"), ("monitor", "list-check"), ("dashboard", "dashboard")]


-- Item helpers

-- | Records the click as a recent entry (fetch, not hx-post: htmx would cancel the anchor navigation)
recentAttrs :: Projects.ProjectId -> Text -> Text -> [Attribute]
recentAttrs pid label itemType =
  [ data_ "recent-url" $ urls.commandPaletteRecentPost pid
  , data_ "recent-label" label
  , data_ "recent-type" itemType
  , [__|on click
          fetch `${my.dataset.recentUrl}` {method:'POST', headers:{'Content-Type':'application/x-www-form-urlencoded'}, body: `label=${encodeURIComponent(my.dataset.recentLabel)}&url=${encodeURIComponent(my.getAttribute('href'))}&itemType=${encodeURIComponent(my.dataset.recentType)}`}
       |]
  ]


data CmdKind = CKDirect | CKCategory | CKChild | CKRecent | CKLogShortcut | CKAiAction


cmdAttrs :: Text -> CmdKind -> [Attribute]
cmdAttrs search kind =
  [ data_ "search" search
  , term "hx-live:hidden" $ "((c, s, t, d) => " <> hiddenWhen <> ")(closest('.cmd-palette').data.currentCategory, q('#cmd-palette-input').value.toLowerCase(), this.dataset.search, this.dataset)"
  ]
    <> [data_ "cmd-type" "category" | isCategory]
  where
    fuzzyMiss = "!!c || !(i => [...s].every(ch => (i = t.indexOf(ch, i) + 1)))(0)"
    (hiddenWhen, isCategory) = case kind of
      CKDirect -> (fuzzyMiss, False)
      CKCategory -> (fuzzyMiss, True)
      CKChild -> ("(c ? d.cmdCategory != c : !s) || !t.includes(s)", False)
      CKRecent -> ("!!(c || s)", False)
      CKLogShortcut -> ("!!c", False)
      CKAiAction -> ("!!c", False)


-- | Every palette row; its visibility is derived from the current category (@c@) and query (@s@),
-- with each kind emitting only its own rule.
cmdLink_ :: Text -> CmdKind -> [Attribute] -> Html () -> Html ()
cmdLink_ search kind attrs = pickerOption_ a_ "cmd-item" (cmdAttrs search kind <> attrs)


cmdRow_ :: Text -> CmdKind -> [Attribute] -> Text -> [Attribute] -> Text -> Text -> Html ()
cmdRow_ search kind attrs icon labelAttrs label badgeText =
  pickerItem_
    a_
    def{extraClass = "cmd-item", leading = pickerIcon_ icon, labelAttrs, trailing = pickerBadge_ badgeText, filterable = False}
    label
    (cmdAttrs search kind <> attrs)


-- | Base item renderer — unifies direct, child, and action items
cmdItem :: Projects.ProjectId -> CmdKind -> Text -> [Attribute] -> Text -> Text -> Text -> Html ()
cmdItem pid kind href extraAttrs icon label badgeText =
  cmdRow_ (T.toLower label) kind (href_ href : recentAttrs pid label (T.toLower badgeText) <> extraAttrs) icon [] label badgeText
