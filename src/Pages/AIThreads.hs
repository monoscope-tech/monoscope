module Pages.AIThreads (AIChatForm (..), RoutineForm (..), TitleForm (..), threadsGetH, threadGetH, startThreadPostH, threadPostH, threadTitlePostH, threadDeleteH, routinePostH, routinePausePostH) where

import BackgroundJobs qualified
import Data.Aeson qualified as AE
import Data.Effectful.UUID qualified as UUID
import Data.Text qualified as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effectful.Concurrent.Async (concurrently)
import Effectful.Reader.Static (ask)
import Lucid hiding (for_)
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxIndicator_, hxPost_, hxSwap_, hxTarget_)
import Lucid.Hyperscript (__)
import Models.Apis.Issues qualified as Issues
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkAIPageCtx)
import Pages.Bots.Utils qualified as Bots
import Pages.Issues qualified as IssuePage
import Pkg.AI qualified as AI
import Pkg.DeriveUtils (UUIDId (..))
import Relude hiding (ask)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, redirectCS)
import Utils (faSprite_)
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (FromHttpApiData (..))


data AIChatForm = AIChatForm
  { query :: Text
  , mode :: ComposerMode
  , intervalMinutes :: Maybe Issues.RoutineInterval
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


newtype RoutineForm = RoutineForm {intervalMinutes :: Issues.RoutineInterval}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data ComposerMode = ChatMode | RoutineMode
  deriving stock (Generic, Show)


instance FromHttpApiData ComposerMode where
  parseUrlPiece = \case
    "chat" -> Right ChatMode
    "routine" -> Right RoutineMode
    _ -> Left "Unknown conversation mode."


data NewThread = NewChat Text | NewRoutine Text Issues.RoutineInterval


newtype TitleForm = TitleForm {title :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


threadsGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
threadsGetH pid = page pid Nothing


threadGetH :: Projects.ProjectId -> UUIDId "conversation" -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
threadGetH pid convId = page pid (Just convId)


page :: Projects.ProjectId -> Maybe (UUIDId "conversation") -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
page pid convIdM = do
  (_, _, bw) <- mkAIPageCtx pid
  (conversations, (conversationM, selectedMessages)) <-
    concurrently
      (Issues.listConversations pid)
      $ maybe
        (pure (Nothing, []))
        (\convId -> concurrently (Issues.getConversation pid convId) (Issues.selectChatHistory pid convId))
        convIdM
  let activeId = (.conversationId) <$> conversationM
      messages = bool [] selectedMessages (isJust conversationM)
      routineM = conversationM >>= \conversation -> (conversation,) <$> conversation.routineInterval
      pageConfig = bw{pageTitle = maybe "AI" (.title) conversationM, menuItem = Just "AI", activeConversationId = activeId, conversations, hideNavbar = True}
  addRespHeaders $ PageCtx pageConfig $ threadPage_ pid ((.title) <$> conversationM) ((.conversationType) <$> conversationM) activeId routineM messages
  where
    threadPage_ :: Projects.ProjectId -> Maybe Text -> Maybe Issues.ConversationType -> Maybe (UUIDId "conversation") -> Maybe (Issues.ConversationSummary, Issues.RoutineInterval) -> [Issues.AIChatMessage] -> Html ()
    threadPage_ projectId titleM conversationTypeM activeConversationIdM routineM messages = div_ [class_ "h-full flex flex-col bg-bgBase"] do
      header_ [class_ "h-12 shrink-0 border-b border-strokeWeak px-4 flex items-center gap-2"] do
        faSprite_ "sparkles" "regular" "w-4 h-4 text-textBrand"
        span_ [class_ "font-medium truncate"] $ toHtml $ fromMaybe "Monoscope AI" titleM
        span_ [class_ "grow"] ""
        when (conversationTypeM == Just Issues.CTWeb) $ for_ activeConversationIdM \convId -> details_ [class_ "relative group/routine"] do
          summary_ [class_ "list-none cursor-pointer rounded-lg px-2.5 py-2 text-xs text-textWeak hover:bg-fillWeak hover:text-textStrong focus-visible:outline-2 focus-visible:outline-offset-2 transition-colors"] do
            faSprite_ "clock" "regular" "w-3.5 h-3.5 inline-block mr-1.5"
            toHtml $ maybe "Schedule" (\(conversation, interval) -> bool "Paused" ("Every " <> show (Issues.routineIntervalMinutes interval) <> " min") conversation.routineActive) routineM
          div_ [class_ "absolute right-0 top-full mt-2 z-20 w-64 rounded-xl bg-bgRaised p-3 shadow-lg border border-strokeWeak"] do
            form_ [class_ "space-y-3", hxPost_ $ "/p/" <> projectId.toText <> "/ai/" <> convId.toText <> "/routine", hxSwap_ "none"] do
              label_ [class_ "block text-xs font-medium"] "Run every"
              div_ [class_ "flex items-center gap-2"] do
                input_ [type_ "number", name_ "intervalMinutes", min_ "5", max_ "10080", value_ $ maybe "1440" (show . Issues.routineIntervalMinutes . snd) routineM, class_ "input input-sm grow min-w-0", Aria.label_ "Routine interval in minutes"]
                span_ [class_ "text-xs text-textWeak"] "minutes"
              button_ [type_ "submit", class_ "w-full rounded-lg bg-fillBrand-strong text-white px-3 py-2 text-xs font-medium cursor-pointer active:scale-[0.96] transition-transform"] "Save routine"
              when (maybe False ((.routineActive) . fst) routineM) $ form_ [class_ "mt-2 pt-2 border-t border-strokeWeak", hxPost_ $ "/p/" <> projectId.toText <> "/ai/" <> convId.toText <> "/routine/pause", hxSwap_ "none"] $ button_ [type_ "submit", class_ "w-full rounded-lg px-3 py-2 text-xs text-textWeak hover:bg-fillWeak hover:text-textStrong cursor-pointer"] "Pause routine"
      div_ [id_ "ai-thread-messages", class_ "min-h-0 grow overflow-y-auto"] do
        div_ [class_ "max-w-3xl mx-auto px-5 py-8"] do
          for_ routineM routineOverview_
          if hasVisibleHistory messages
            then do
              when (isJust routineM) $ h2_ [class_ "mb-2 mt-6 text-xs font-medium uppercase tracking-wide text-textWeak"] "Recent results"
              IssuePage.aiChatHistoryView_ projectId messages
            else maybe emptyThread_ routineEmpty_ routineM
      form_
        [ class_ "shrink-0 border-t border-strokeWeak bg-bgBase p-4"
        , hxPost_ $ "/p/" <> projectId.toText <> "/ai" <> foldMap (("/" <>) . (.toText)) activeConversationIdM
        , hxTarget_ "this"
        , hxSwap_ "none"
        , hxIndicator_ "#ai-thread-loader"
        ]
        do
          div_ [class_ "max-w-3xl mx-auto flex items-end gap-2 rounded-xl bg-bgRaised p-2 shadow-[0_0_0_1px_var(--color-strokeWeak),0_4px_12px_oklch(0_0_0/0.05)] focus-within:shadow-[0_0_0_2px_var(--color-strokeFocus)]"] do
            textarea_
              [ name_ "query"
              , id_ "ai-thread-input"
              , rows_ "1"
              , maxlength_ "4000"
              , required_ "required"
              , autofocus_
              , placeholder_ "Ask about your telemetry or delegate a task…"
              , Aria.label_ "Message Monoscope AI"
              , class_ "min-h-9 max-h-40 grow resize-none bg-transparent border-0 px-2 py-2 text-sm leading-5 focus:outline-none"
              , [__|on keydown[key=='Enter' and not shiftKey]
                      halt the event then call my form.requestSubmit()
                    end|]
              ]
              ""
            input_ [type_ "hidden", name_ "mode", value_ "chat"]
            button_ [type_ "submit", Aria.label_ "Send message", class_ "group/send w-9 h-9 shrink-0 rounded-lg bg-fillBrand-strong text-white flex items-center justify-center cursor-pointer active:scale-[0.96] transition-transform"] do
              span_ [id_ "ai-thread-loader", class_ "htmx-indicator"] $ faSprite_ "spinner" "regular" "w-3.5 h-3.5 animate-spin"
              span_ [class_ "inline-flex group-has-[.htmx-request]/send:hidden"] $ faSprite_ "arrow-up" "regular" "w-3.5 h-3.5"
          p_ [class_ "max-w-3xl mx-auto mt-2 text-xs text-textWeak text-center"] "AI can use your project data and connected integrations. Review actions before relying on them."
      where
        hasVisibleHistory :: [Issues.AIChatMessage] -> Bool
        hasVisibleHistory history = any ((== Issues.ChatExecutionEvent) . (.role)) history || hasExchange history
          where
            hasExchange (user : assistant : rest) = (user.role == Issues.ChatUser && assistant.role == Issues.ChatAssistant) || hasExchange (assistant : rest)
            hasExchange _ = False

        routineOverview_ :: (Issues.ConversationSummary, Issues.RoutineInterval) -> Html ()
        routineOverview_ (conversation, interval) =
          let (status, statusClass) =
                if
                  | isJust conversation.routineRunningSince -> ("Running now", "bg-fillBrand-weak text-textBrand")
                  | conversation.routineActive -> ("Active", "bg-fillSuccess-weak text-textSuccess")
                  | otherwise -> ("Paused", "bg-fillWeak text-textWeak")
              nextRunLabel = maybe "No upcoming run" (toText . formatTime defaultTimeLocale "Next run %-d %b, %H:%M UTC") conversation.routineNextRunAt
           in section_ [class_ "rounded-xl border border-strokeWeak bg-fillWeaker p-4", Aria.label_ "Routine status"] do
                div_ [class_ "flex items-start gap-3"] do
                  span_ [class_ "flex h-9 w-9 shrink-0 items-center justify-center rounded-lg bg-fillBrand-weak text-textBrand"]
                    $ faSprite_ "clock" "regular" "h-4 w-4"
                  div_ [class_ "min-w-0 grow"] do
                    div_ [class_ "flex flex-wrap items-center gap-2"] do
                      h2_ [class_ "font-medium text-textStrong"] "Routine schedule"
                      span_ [class_ $ "rounded-full px-2 py-0.5 text-[0.6875rem] font-medium " <> statusClass] $ toHtml status
                    p_ [class_ "mt-1 text-sm text-textWeak"] $ toHtml $ Issues.routineCadence interval <> " · " <> nextRunLabel

        routineEmpty_ :: (Issues.ConversationSummary, Issues.RoutineInterval) -> Html ()
        routineEmpty_ (conversation, _) = div_ [class_ "flex min-h-[18rem] flex-col items-center justify-center text-center"] do
          span_ [class_ "flex h-10 w-10 items-center justify-center rounded-xl bg-fillWeak text-textWeak"]
            $ faSprite_ "clock" "regular" "h-4 w-4"
          h2_ [class_ "mt-4 font-medium text-textStrong"] "No completed run yet"
          p_ [class_ "mt-1 max-w-sm text-sm text-textWeak"]
            $ if conversation.routineActive
              then "This routine is scheduled. Its first result will appear here after the next run."
              else "This routine is paused. Resume its schedule to produce a new result."

        emptyThread_ :: Html ()
        emptyThread_ = div_ [class_ "min-h-[50vh] flex flex-col justify-center"] do
          h1_ [class_ "text-xl font-semibold tracking-tight"] "What should we investigate?"
          p_ [class_ "mt-2 text-sm text-textWeak max-w-xl"] "Ask a question about production, build a chart, or describe work you want to repeat."
          div_ [class_ "mt-6 flex flex-wrap gap-2"] $ forM_ prompts \prompt ->
            button_ [type_ "button", class_ "rounded-lg border border-strokeWeak px-3 py-2 text-sm text-left text-textWeak hover:text-textStrong hover:bg-fillWeak active:scale-[0.96] transition-[color,background-color,scale]", data_ "prompt" prompt, [__|on click set #ai-thread-input.value to my @data-prompt then call #ai-thread-input.focus()|]] $ toHtml prompt
          where
            prompts = ["What changed in production today?", "Find the services with the most errors", "Create a daily reliability briefing"]


startThreadPostH :: Projects.ProjectId -> AIChatForm -> ATAuthCtx (RespHeaders (Html ()))
startThreadPostH pid form = do
  authorizeProject pid
  case normalizeNewThread form of
    Left err -> reject err
    Right request -> do
      convId <- UUIDId <$> UUID.genUUID
      void $ Issues.getOrCreateConversation pid convId Issues.CTWeb (AE.object [])
      case request of
        NewChat prompt -> submitTurn pid convId prompt
        NewRoutine prompt interval -> submitTurn pid convId prompt >> schedule pid convId interval
      redirectToThread pid convId


threadPostH :: Projects.ProjectId -> UUIDId "conversation" -> AIChatForm -> ATAuthCtx (RespHeaders (Html ()))
threadPostH pid convId form = do
  authorizeProject pid
  case normalizeQuery form.query of
    Left err -> reject err
    Right prompt ->
      Issues.getConversation pid convId >>= \case
        Nothing -> addRespHeaders mempty
        Just conversation -> do
          case Issues.conversationIssueId conversation of
            Just issueId -> void $ IssuePage.aiChatPostH pid issueId (IssuePage.AIChatForm prompt)
            Nothing -> submitTurn pid convId prompt
          redirectToThread pid convId


threadTitlePostH :: Projects.ProjectId -> UUIDId "conversation" -> TitleForm -> ATAuthCtx (RespHeaders (Html ()))
threadTitlePostH pid convId form = do
  authorizeProject pid
  let title = T.strip form.title
  if T.null title
    then reject "Enter a title."
    else Issues.renameConversation pid convId title >> redirectToThread pid convId


threadDeleteH :: Projects.ProjectId -> UUIDId "conversation" -> ATAuthCtx (RespHeaders (Html ()))
threadDeleteH pid convId = do
  authorizeProject pid
  Issues.deleteConversation pid convId
  redirectCS $ "/p/" <> pid.toText <> "/ai"
  addRespHeaders mempty


submitTurn :: Projects.ProjectId -> UUIDId "conversation" -> Text -> ATAuthCtx ()
submitTurn pid convId prompt = do
  appCtx <- ask @AuthContext
  result <- Bots.processActionableAIQuery (Just appCtx.config) appCtx.env.enableTimefusionReads AI.ServiceAccess pid prompt (Just convId) appCtx.config.openaiModel appCtx.config.openaiApiKey
  case result of
    Left err -> Issues.insertChatMessage pid convId Issues.ChatAssistant ("I couldn't complete that request: " <> err) Nothing Nothing
    Right _ -> pass


normalizeQuery :: Text -> Either Text Text
normalizeQuery = (\prompt -> if T.null prompt then Left "Enter a prompt to start." else if T.length prompt > 4000 then Left "Prompt is limited to 4,000 characters." else Right prompt) . T.strip


normalizeNewThread :: AIChatForm -> Either Text NewThread
normalizeNewThread form = do
  prompt <- normalizeQuery form.query
  case (form.mode, form.intervalMinutes) of
    (ChatMode, Nothing) -> Right $ NewChat prompt
    (ChatMode, Just _) -> Left "Chat mode does not accept a routine interval."
    (RoutineMode, Nothing) -> Left "Choose a routine interval."
    (RoutineMode, Just interval) -> Right $ NewRoutine prompt interval


reject :: Text -> ATAuthCtx (RespHeaders (Html ()))
reject message = addErrorToast message Nothing >> addRespHeaders mempty


authorizeProject :: Projects.ProjectId -> ATAuthCtx ()
authorizeProject = void . Projects.sessionAndProject


redirectToThread :: Projects.ProjectId -> UUIDId "conversation" -> ATAuthCtx (RespHeaders (Html ()))
redirectToThread pid convId = do
  redirectCS $ "/p/" <> pid.toText <> "/ai/" <> convId.toText
  addRespHeaders mempty


routinePostH :: Projects.ProjectId -> UUIDId "conversation" -> RoutineForm -> ATAuthCtx (RespHeaders (Html ()))
routinePostH pid convId form = do
  authorizeProject pid
  whenJustM (Issues.getConversation pid convId) \conversation ->
    when (conversation.conversationType == Issues.CTWeb) $ schedule pid convId form.intervalMinutes
  redirectToThread pid convId


schedule :: Projects.ProjectId -> UUIDId "conversation" -> Issues.RoutineInterval -> ATAuthCtx ()
schedule pid convId interval = do
  appCtx <- ask @AuthContext
  whenJustM (Issues.upsertRoutine pid convId interval) \routine ->
    BackgroundJobs.enqueueAIRoutine appCtx routine.id routine.scheduledAt


routinePausePostH :: Projects.ProjectId -> UUIDId "conversation" -> ATAuthCtx (RespHeaders (Html ()))
routinePausePostH pid convId = do
  authorizeProject pid
  Issues.pauseRoutine pid convId
  redirectToThread pid convId
