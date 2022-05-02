module Pages.Log (apiLog, apiLogItem) where

import Config
import Data.Aeson qualified as AE
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector, iforM_)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.HTMX
import Lucid.Hyperscript (__)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Optics.Core ((^.))
import Pages.BodyWrapper (BWConfig, bodyWrapper, currProject, pageTitle, sessM)
import Relude
import Servant (Headers, addHeader)
import Servant.Htmx (HXPush)

-- $setup
-- >>> import Relude
-- >>> import Data.Vector qualified as Vector
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import Data.Aeson

apiLog :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> DashboardM (Headers '[HXPush] (Html ()))
apiLog sess pid queryM cols' hxRequestM hxBoostedM = do
  let cols = T.splitOn "," (fromMaybe "" cols')
  let query = fromMaybe "" queryM
  pool <- asks pool
  (project, requests) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      requests <- RequestDumps.selectRequestDumpByProject pid query
      pure (project, requests)

  let currentUrlPath = RequestDumps.requestDumpLogUrlPath pid queryM cols'
  case (hxRequestM, hxBoostedM) of
    (Just "true", Nothing) -> pure $ addHeader currentUrlPath $ logItemRows pid requests cols
    _ -> do
      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess,
                currProject = project,
                pageTitle = "API Log Explorer"
              }
      pure $ addHeader currentUrlPath $ bodyWrapper bwconf $ apiLogsPage pid requests cols

apiLogItem :: Sessions.PersistentSession -> Projects.ProjectId -> UUID.UUID -> DashboardM (Html ())
apiLogItem sess pid rdId = do
  pool <- asks pool
  logItemM <- liftIO $ withPool pool $ RequestDumps.selectRequestDumpByProjectAndId pid rdId
  case logItemM of
    Just logItem -> pure $ apiLogItemView logItem
    Nothing -> pure $ div_ "invalid log request ID"

apiLogsPage :: Projects.ProjectId -> Vector RequestDumps.RequestDumpLogItem -> [Text] -> Html ()
apiLogsPage pid requests cols =
  section_ [class_ "container mx-auto  px-3 py-5 gap-4 flex flex-col h-full overflow-hidden "] $ do
    form_
      [ class_ "card-round",
        hxGet_ $ "/p/" <> Projects.projectIdText pid <> "/log_explorer",
        hxVals_ "js:{query:getQueryFromEditor(), cols:params().cols}",
        hxTarget_ "#log-item-table-body"
      ]
      $ do
        nav_ [class_ "flex flex-row p-2 content-end justify-between items-baseline border-slate-100"] $ do
          a_ [class_ "inline-block"] "Query"
          button_ [type_ "submit", class_ "inline-block btn-sm btn-indigo"] "Run query"
        div_ $ do
          div_ [id_ "queryEditor", class_ "h-24"] ""

    div_ [class_ "card-round grow divide-y flex flex-col mb-8 text-sm h-full"] $ do
      div_ [class_ "pl-3 py-2 space-x-5"] $ do
        strong_ "Query results"
        span_ "330 log entries"

      jsonTreeAuxillaryCode pid
      table_ [class_ "table-fixed grow w-full min-w-full h-full divide-y flex flex-col monospace"] $ do
        thead_ [class_ "text-xs bg-gray-100 gray-400"] $ do
          tr_ [class_ "flex flex-row text-left space-x-4"] $ do
            th_ [class_ "font-normal inline-block py-1.5 p-1 px-2 w-8"] ""
            th_ [class_ "font-normal inline-block py-1.5 p-1 px-2 w-32"] "TIMESTAMP"
            th_ [class_ "font-normal inline-block py-1.5 p-1 px-2 grow"] "SUMMARY"
        tbody_ [class_ " grow overflow-y-scroll h-full whitespace-nowrap text-sm divide-y overflow-x-hidden", id_ "log-item-table-body"] $ logItemRows pid requests cols

logItemRows :: Projects.ProjectId -> Vector RequestDumps.RequestDumpLogItem -> [Text] -> Html ()
logItemRows pid requests cols =
  requests & traverse_ \req -> do
    let logItemPath = RequestDumps.requestDumpLogItemUrlPath pid (req ^. #id)
    tr_
      [ class_ "border-l-4 border-l-transparent divide-x space-x-4 hover:bg-blue-50 cursor-pointer",
        term "data-log-item-path" logItemPath,
        term
          "_"
          [text|
            install LogItemExpandable
        |]
      ]
      $ do
        td_ [class_ "p-1 px-2 w-8 flex justify-center align-middle"] $ do
          img_ [src_ "/assets/svgs/cheveron-right.svg", class_ "w-1.5 log-chevron"]
        td_ [class_ "p-1 px-2 w-32 overflow-hidden"] $ toHtml @String $ formatTime defaultTimeLocale "%F %R" (req ^. #createdAt)
        td_ [class_ "p-1 px-2"] $ do
          let reqJSON = AE.toJSON req
          let colValues = concatMap (\col -> findValueByKeyInJSON (T.splitOn "." col) reqJSON) cols
          -- FIXME: probably inefficient implementation and should be optimized
          zip cols colValues
            & traverse_
              \(col, colValue) ->
                div_ [class_ "relative inline-block log-item-field-parent", term "data-field-path" col] $ do
                  a_
                    [ class_ "cursor-pointer mx-1 inline-block bg-blue-100 blue-800 hover:bg-blue-200 blue-900 px-3 rounded-xl monospace log-item-field-anchor log-item-field-value",
                      term "data-field-path" col,
                      [__|install LogItemMenuable|]
                    ]
                    $ toHtml colValue
          span_ [class_ "mx-1 inline-block bg-green-100 green-800 px-3 rounded-xl monospace"] $ toHtml $ req ^. #method
          span_ [class_ "mx-1 inline-block bg-stone-200 stone-900 px-3 rounded-xl monospace"] $ toHtml $ req ^. #urlPath
          let rawUrl = req ^. #rawUrl
          let reqBody = decodeUtf8 $ AE.encode $ req ^. #requestBody
          let respBody = decodeUtf8 $ AE.encode $ req ^. #responseBody
          let reqHeaders = decodeUtf8 $ AE.encode $ req ^. #requestHeaders
          let respHeaders = decodeUtf8 $ AE.encode $ req ^. #responseHeaders
          p_ [class_ "inline-block"] $ toHtml $ T.take 300 [text| raw_url=$rawUrl request_body=$reqBody response_body=$respBody request_headers=$reqHeaders response_headers=$respHeaders|]

apiLogItemView :: RequestDumps.RequestDumpLogItem -> Html ()
apiLogItemView req =
  tr_ [class_ "log-item-info border-l-blue-200 border-l-4"] $
    td_ [class_ "pl-4 py-1 ", colspan_ "3"] $ do
      jsonValueToHtmlTree $ AE.toJSON req

-- | jsonValueToHtmlTree takes an aeson json object and renders it as a collapsible html tree, with hyperscript for interactivity.
jsonValueToHtmlTree :: AE.Value -> Html ()
jsonValueToHtmlTree val = jsonValueToHtmlTree' ("", "", val)
  where
    jsonValueToHtmlTree' :: (Text, Text, AE.Value) -> Html ()
    jsonValueToHtmlTree' (path, key, AE.Object v) = renderParentType "{" "}" key (length v) (HM.toList v & mapM_ (\(kk, vv) -> jsonValueToHtmlTree' (path <> "." <> key, kk, vv)))
    jsonValueToHtmlTree' (path, key, AE.Array v) = renderParentType "[" "]" key (length v) (iforM_ v \i item -> jsonValueToHtmlTree' (path <> "." <> key <> "." <> "[]", show i, item))
    jsonValueToHtmlTree' (path, key, value) = do
      let fullFieldPath = if T.isSuffixOf ".[]" path then path else path <> "." <> key
      let fullFieldPath' = fromMaybe fullFieldPath $ T.stripPrefix ".." fullFieldPath
      div_
        [ class_ "relative log-item-field-parent",
          term "data-field-path" fullFieldPath'
        ]
        $ a_ [class_ "block hover:bg-blue-50 cursor-pointer pl-6 relative log-item-field-anchor ", [__|install LogItemMenuable|]] $ do
          span_ $ toHtml key
          span_ [class_ "text-blue-800"] ":"
          span_ [class_ "text-blue-800 ml-2.5 log-item-field-value"] $ toHtml $ unwrapJsonPrimValue value

    renderParentType :: Text -> Text -> Text -> Int -> Html () -> Html ()
    renderParentType opening closing key count child = div_ [class_ (if key == "" then "" else "collapsed")] $ do
      a_
        [ class_ "inline-block cursor-pointer",
          [__|on click toggle .collapsed on the closest parent <div/>|]
        ]
        $ do
          span_ [class_ "log-item-tree-chevron "] "▾"
          span_ [] $ toHtml $ if key == "" then opening else key <> ": " <> opening
      div_ [class_ "pl-5 children "] $ do
        span_ [class_ "tree-children-count"] $ show count
        div_ [class_ "tree-children"] child
      span_ [class_ "pl-5 closing-token"] $ toHtml closing

-- >>> findValueByKeyInJSON ["key1", "key2"] [aesonQQ|{"kx":0, "key1":{"key2":"k2val"}}|]
-- ["\"k2val\""]
-- >>> findValueByKeyInJSON ["key1", "[]", "key2"] [aesonQQ|{"kx":0, "key1":[{"key2":"k2val"}]}|]
-- ["\"k2val\""]
findValueByKeyInJSON :: [Text] -> AE.Value -> [Text]
findValueByKeyInJSON (x : path) (AE.Object obj) = concatMap (\(_, v) -> findValueByKeyInJSON path v) (HM.toList obj & filter (\(k, _) -> k == x))
findValueByKeyInJSON ("[]" : path) (AE.Array vals) = concatMap (findValueByKeyInJSON path) (Vector.toList vals)
findValueByKeyInJSON [] value = [unwrapJsonPrimValue value]
findValueByKeyInJSON _ _ = error "findValueByKeyInJSON: case should be unreachable"

-- TODO:
jsonTreeAuxillaryCode :: Projects.ProjectId -> Html ()
jsonTreeAuxillaryCode pid = do
  template_ [id_ "log-item-context-menu-tmpl"] $ do
    div_ [id_ "log-item-context-menu", class_ "log-item-context-menu text-sm origin-top-right absolute left-0 mt-2 w-56 rounded-md shadow-md shadow-slate-300 bg-white ring-1 ring-black ring-opacity-5 divide-y divide-gray-100 focus:outline-none z-10", role_ "menu", tabindex_ "-1"] $ do
      div_ [class_ "py-1", role_ "none"] $ do
        a_
          [ class_ "cursor-pointer text-gray-700 block px-4 py-1 text-sm hover:bg-gray-100 hover:text-gray-900",
            role_ "menuitem",
            tabindex_ "-1",
            id_ "menu-item-0",
            hxGet_ $ "/p/" <> Projects.projectIdText pid <> "/log_explorer",
            hxVals_ "js:{query:params().query,cols:toggleColumnToSummary(event)}",
            hxTarget_ "#log-item-table-body",
            [__|init 
                  set fp to (closest <.log-item-field-parent/>)'s @data-field-path then 
                  if isFieldInSummary(fp) then set my innerHTML to 'Remove field from summary' end|]
          ]
          "Add field to Summary"
        a_
          [ class_ "cursor-pointer text-gray-700 block px-4 py-1 text-sm hover:bg-gray-100 hover:text-gray-900",
            role_ "menuitem",
            tabindex_ "-1",
            id_ "menu-item-1",
            [__|on click 
                  if 'clipboard' in window.navigator then 
                    call navigator.clipboard.writeText((previous <.log-item-field-value/>)'s innerText)
                    send successToast(value:['Value has been added to the Clipboard']) to <body/>
                    halt
                  end|]
          ]
          "Copy field value"

  script_
    [type_ "text/hyperscript"]
    [text|
      behavior LogItemMenuable
        on click
          if I match <.with-context-menu/> then
            remove <.log-item-context-menu /> then remove .with-context-menu from <.with-context-menu />
          else
            remove <.log-item-context-menu /> then remove .with-context-menu from <.with-context-menu /> then
            get #log-item-context-menu-tmpl.innerHTML then put it after me then add .with-context-menu to me then 
            _hyperscript.processNode(document.querySelector('.log-item-context-menu'))
            htmx.process(document.querySelector('.log-item-context-menu'))
          end
          halt
        end
      end

      behavior LogItemExpandable
        on click 
          if I match <.expanded-log/> then 
            remove next <.log-item-info/> then 
            remove .expanded-log from me
          else
            fetch `$${@data-log-item-path}` as html then put it after me then
            _hyperscript.processNode(next <.log-item-info />) then
            add .expanded-log to me
          end 
      end
    |]

  script_ [src_ "/assets/js/monaco/vs/loader.js", defer_ "true"] ("" :: Text)
  script_
    [text|
    var params = () => new Proxy(new URLSearchParams(window.location.search), {
      get: (searchParams, prop) => searchParams.get(prop)??"",
    });
    var toggleColumnToSummary = (e)=>{
      const cols = (params().cols??"").split(",").filter(x=>x!="");
      const subject = e.target.closest('.log-item-field-parent').dataset.fieldPath; 
      if (cols.includes(subject)) {
        return [...new Set(cols.filter(x=>x!=subject))].join(",");
      } 
      return [...new Set(cols.concat(subject))].join(",");
    }
    var isFieldInSummary = field=>params().cols.split(",").includes(field);
    var getQueryFromEditor = ()=>window.editor.getValue();

    var execd = false
    document.addEventListener('DOMContentLoaded', function(){
      // Configuration for the monaco editor which the query editor is built on.
      require.config({ paths: { vs: '/assets/js/monaco/vs' } });
			require(['vs/editor/editor.main'], function () {
        monaco.editor.defineTheme('apitoolkit', {
          base: 'vs',
          inherit: true,
          rules: [{ background: 'EDF9FA' }],
          colors: {
            'editor.foreground': '#000000',
            'editor.background': '#f5f5f5',
            'editorGutter.background': '#e8e8e8',
          }
        });
        monaco.editor.setTheme('apitoolkit');
				window.editor = monaco.editor.create(document.getElementById('queryEditor'), {
          value: params().query,
					language:'hcl',
          minimap:{enabled:false}
				});
			});
      // Monaco code suggestions https://github.com/microsoft/monaco-editor/issues/1850
    })
    |]

  style_
    [text|
    .tree-children {
      display: block;
    }
    .tree-children-count { display: none; }
    .collapsed .tree-children {
      display: none !important; 
    }
    .collapsed .tree-children-count {display: inline !important;}
    .collapsed .children {display: inline-block; padding-left:0}
    .collapsed .closing-token {padding-left:0}
  |]

unwrapJsonPrimValue :: AE.Value -> Text
unwrapJsonPrimValue (AE.Bool True) = "true"
unwrapJsonPrimValue (AE.Bool False) = "true"
unwrapJsonPrimValue (AE.String v) = "\"" <> toText v <> "\""
unwrapJsonPrimValue (AE.Number v) = toText @String $ show v
unwrapJsonPrimValue AE.Null = "null"
unwrapJsonPrimValue (AE.Object _) = error "Impossible. unwrapJsonPrimValue should be for primitive types only" -- should never be reached
unwrapJsonPrimValue (AE.Array _) = error "Impossible. unwrapJsonPrimValue should be for primitive types only" -- should never be reached
