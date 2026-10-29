module Pages.AIThreads (AIChatForm (..), RoutineForm (..), RoutineTemplateForm (..), RoutineDestinationForm (..), TitleForm (..), threadsGetH, threadGetH, routinesGetH, routineInstallPostH, startThreadPostH, threadPostH, threadTitlePostH, threadDeleteH, routinePostH, routinePausePostH, routineCancelPostH, routineResumePostH, routineDestinationPostH, routineDeleteH) where

import BackgroundJobs qualified
import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Effectful.UUID qualified as UUID
import Data.Text qualified as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effectful.Concurrent.Async (concurrently)
import Effectful.Reader.Static (ask)
import Lucid hiding (for_)
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxConfirm_, hxDelete_, hxGet_, hxIndicator_, hxPost_, hxSelect_, hxSwap_, hxTarget_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Apis.Issues qualified as Issues
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkAIPageCtx, navTabAttrs)
import Pages.Bots.Utils qualified as Bots
import Pages.Components (BadgeColor (..), EmptyStateCfg (..), EmptyStateSize (..), IconBadgeSize (..), emptyState_, iconBadge_, localTimeFmt_, tonePill_)
import Pages.Issues qualified as IssuePage
import Pkg.AI qualified as AI
import Pkg.Components.Table (Config (..), EmptyStateAction (..), Features (..), Table (..), ZeroState (ZeroState), col, withAttrs)
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..))
import Relude hiding (ask)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, redirectCS)
import Utils (faSprite_, renderMarkdownToc)
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


newtype RoutineTemplateForm = RoutineTemplateForm {templateKey :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


newtype RoutineDestinationForm = RoutineDestinationForm {destination :: Issues.RoutineDestination}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data ComposerMode = ModeChat | ModeRoutine
  deriving stock (Generic, Read, Show)
  deriving (FromHttpApiData) via WrappedEnumSC 'Nothing "Mode" ComposerMode


data NewThread = NewChat Text | NewRoutine Text Issues.RoutineInterval


newtype TitleForm = TitleForm {conversationTitle :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


threadsGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
threadsGetH pid = do
  (_, _, bw) <- mkAIPageCtx pid
  allConversations <- Issues.listConversations pid
  let pageConfig =
        bw
          { pageTitle = "Conversations"
          , menuItem = Just "AI"
          , conversations = allConversations
          , pageActions = Just $ button_ [type_ "button", class_ "btn btn-sm btn-primary gap-2 active:scale-[0.96] transition-transform", [__|on click set #ai-composer-input.value to '' then set #ai-composer-chat.checked to true then call #ai-composer-modal.showModal()|]] do
              faSprite_ "plus" "regular" "h-3.5 w-3.5"
              span_ [class_ "max-md:hidden"] "New chat"
          }
  addRespHeaders $ PageCtx pageConfig $ conversationsPage_ $ filter (isNothing . (.routineInterval)) allConversations
  where
    conversationsPage_ :: [Issues.ConversationSummary] -> Html ()
    conversationsPage_ items = div_ [class_ "h-full overflow-y-auto bg-bgBase"] do
      main_ [class_ "mx-auto max-w-5xl px-4 py-6 sm:px-6 sm:py-8"] do
        p_ [class_ "mb-5 max-w-2xl text-sm text-textWeak"] "Open and manage your project’s AI conversations."
        toHtml
          Table
            { config = def{elemID = "ai-conversations", renderAsTable = True}
            , columns =
                [ col "Conversation" \c -> a_ [href_ $ threadUrl pid c.conversationId, class_ "block rounded-sm font-medium text-textStrong hover:text-textBrand focus-visible:outline-2 focus-visible:outline-offset-2"] $ toHtml c.title
                , col "Last updated" (\c -> time_ [datetime_ $ show c.updatedAt, class_ "text-sm tabular-nums text-textWeak"] $ toHtml $ formatTime defaultTimeLocale "%e %b %Y, %H:%M UTC" c.updatedAt) & withAttrs [class_ "w-48 max-md:hidden"]
                , col "" (\c -> a_ [href_ $ threadUrl pid c.conversationId, Aria.label_ $ "Open " <> c.title, class_ "btn btn-sm btn-ghost btn-square active:scale-[0.96] transition-transform"] $ faSprite_ "arrow-right" "regular" "h-3.5 w-3.5 rtl:-scale-x-100") & withAttrs [class_ "w-16 text-end", Aria.label_ "Actions"]
                ]
            , rows = fromList items
            , features = def{zeroState = Just $ ZeroState "comment" "No conversations yet" "Start a chat to investigate your project’s telemetry." ESNone}
            }


routinesGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
routinesGetH pid = do
  (_, _, bw) <- mkAIPageCtx pid
  (conversations, runs) <- concurrently (Issues.listConversations pid) (Issues.listRoutineRuns pid 12)
  let installed = filter (isJust . (.routineInterval)) conversations
      pageConfig =
        bw
          { pageTitle = "Routines"
          , menuItem = Just "AI"
          , conversations
          , pageActions = Just $ button_ [type_ "button", class_ "btn btn-sm btn-primary gap-2 active:scale-[0.96] transition-transform", [__|on click set #ai-composer-input.value to '' then set #ai-composer-routine.checked to true then call #ai-composer-modal.showModal()|]] do
              faSprite_ "plus" "regular" "h-3.5 w-3.5"
              span_ [class_ "max-md:hidden"] "New custom routine"
          }
  addRespHeaders $ PageCtx pageConfig $ routinesPage_ installed runs
  where
    routinesPage_ :: [Issues.ConversationSummary] -> [Issues.RoutineRun] -> Html ()
    routinesPage_ installedRoutines recentRuns = div_ [class_ "h-full overflow-y-auto bg-bgBase"] do
      main_ [class_ "mx-auto max-w-6xl space-y-10 px-4 py-6 sm:px-6 sm:py-8"] do
        p_ [class_ "max-w-2xl text-sm text-textWeak"] "Scheduled operational reviews for your project."
        section_ [Aria.label_ "Installed routines"] do
          div_ [class_ "mb-3 flex items-baseline justify-between"] do
            h2_ [class_ "text-sm font-semibold text-textStrong"] "Installed"
            span_ [class_ "text-xs text-textWeak tabular-nums"] $ toHtml $ show (length installedRoutines) <> " routine" <> bool "s" "" (length installedRoutines == 1)
          if null installedRoutines
            then emptyState_ def{icon = Just "clock", size = ESCompact} "No routines installed" "Add a built-in routine below or create one from a conversation."
            else div_ [class_ "divide-y divide-strokeWeak rounded-xl border border-strokeWeak bg-bgRaised"] $ for_ installedRoutines \routine ->
              div_ [class_ "flex flex-wrap items-center gap-3 px-4 py-3 sm:gap-4"] do
                span_ [class_ $ "h-2 w-2 rounded-full " <> bool "bg-fillWeak" "bg-fillSuccess-strong" routine.routineActive, Aria.hidden_ "true"] mempty
                a_ [href_ $ threadUrl pid routine.conversationId, class_ "min-w-0 grow rounded-sm focus-visible:outline-2 focus-visible:outline-offset-2"] do
                  div_ [class_ "truncate text-sm font-medium text-textStrong"] $ toHtml routine.title
                  div_ [class_ "mt-0.5 text-xs text-textWeak"] $ toHtml $ maybe "Custom schedule" Issues.routineCadence routine.routineInterval <> " · " <> bool "Paused" "Active" routine.routineActive
                let (toggle, toggleLabel) = bool ("/resume", "Resume") ("/pause", "Pause") routine.routineActive
                form_ [hxPost_ $ routineUrl routine <> toggle, hxSwap_ "none"] $ button_ [class_ "btn btn-xs btn-ghost active:scale-[0.96] transition-transform", type_ "submit"] $ toHtml @Text toggleLabel
                when (isJust routine.routineRunningSince) $ form_ [hxPost_ $ routineUrl routine <> "/cancel", hxSwap_ "none"] $ button_ [class_ "btn btn-xs btn-ghost text-textWarning active:scale-[0.96] transition-transform", type_ "submit"] "Cancel run"
                form_ [hxPost_ $ routineUrl routine <> "/destination", hxSwap_ "none"] do
                  let slackEnabled = routine.routineDestination == Just Issues.DestinationSlack
                  input_ [type_ "hidden", name_ "destination", value_ $ bool "slack" "conversation" slackEnabled]
                  button_ [class_ "btn btn-xs btn-ghost active:scale-[0.96] transition-transform", type_ "submit"] $ bool "Send results to Slack" "Stop Slack delivery" slackEnabled
                button_ [class_ "btn btn-xs btn-ghost text-textError active:scale-[0.96] transition-transform", hxDelete_ $ routineUrl routine, hxConfirm_ "Delete this schedule? Its conversation and past results will be kept.", hxSwap_ "none"] "Delete"
        unless (null recentRuns) $ section_ [Aria.label_ "Recent routine runs"] do
          h2_ [class_ "mb-3 text-sm font-semibold text-textStrong"] "Recent runs"
          div_ [class_ "divide-y divide-strokeWeak rounded-xl border border-strokeWeak bg-bgRaised"] $ for_ recentRuns \run ->
            div_ [class_ "grid gap-1 px-4 py-3 sm:grid-cols-[minmax(0,1fr)_auto_auto] sm:items-center sm:gap-4"] do
              div_ [class_ "min-w-0"] do
                case run.title of
                  Just title -> a_ [href_ $ threadUrl pid run.conversationId, class_ "truncate text-sm font-medium text-textBrand hover:underline"] $ toHtml title
                  Nothing -> span_ [class_ "text-sm font-medium text-textWeak"] "Deleted routine"
                whenJust run.error $ p_ [class_ "mt-0.5 truncate text-xs text-textError"] . toHtml
              uncurry tonePill_ $ runStatus run.status
              time_ [datetime_ $ show run.scheduledAt, class_ "text-xs tabular-nums text-textWeak sm:text-right"] $ toHtml $ formatTime defaultTimeLocale "%e %b, %H:%M UTC" run.scheduledAt
        section_ [Aria.label_ "Routine catalog"] do
          h2_ [class_ "text-sm font-semibold text-textStrong"] "Built-in routines"
          p_ [class_ "mt-1 text-sm text-textWeak"] "Read-only by default. Times are in the project’s time zone. Results stay in Monoscope; AI usage is tracked and billed at your project rate."
          let categories = [minBound .. maxBound]
          nav_ [Aria.label_ "Routine categories", class_ "mt-4 flex gap-2 overflow-x-auto pb-1"] $ for_ categories \category ->
            a_ [href_ $ "#" <> categoryId category, class_ "shrink-0 rounded-md border border-strokeWeak bg-bgRaised px-3 py-1.5 text-xs font-medium text-textWeak hover:bg-fillHover hover:text-textStrong"] $ toHtml $ Issues.routineCategoryLabel category
          div_ [class_ "mt-6 space-y-8"] $ for_ categories \category -> section_ [id_ $ categoryId category, class_ "scroll-mt-16"] do
            h3_ [class_ "mb-3 text-sm font-semibold text-textStrong"] $ toHtml $ Issues.routineCategoryLabel category
            div_ [class_ "grid gap-4 md:grid-cols-2 xl:grid-cols-3"] $ for_ (filter ((== category) . (.category)) Issues.routineTemplates) templateCard_
      where
        routineUrl routine = threadUrl pid routine.conversationId <> "/routine"
        categoryId = ("routine-category-" <>) . T.toLower . T.replace " " "-" . Issues.routineCategoryLabel
        templateCard_ template = do
          let added = template.key `elem` mapMaybe (.templateKey) installedRoutines
          article_ [class_ "flex flex-col rounded-xl border border-strokeWeak bg-bgRaised p-5 transition-[border-color,box-shadow] duration-150 hover:border-strokeStrong hover:shadow-md"] do
            div_ [class_ "flex min-h-8 items-center justify-between gap-3"] do
              span_ [class_ "whitespace-nowrap rounded-md bg-fillWeak px-2 py-0.5 font-mono text-xs text-textStrong"] $ toHtml $ Issues.routineScheduleLabel template.schedule
              if added
                then span_ [class_ "inline-flex shrink-0 items-center gap-1.5 text-xs font-medium text-textSuccess"] $ faSprite_ "check" "solid" "h-3 w-3" >> "Added"
                else form_ [hxPost_ $ "/p/" <> pid.toText <> "/ai/routines/install", hxSwap_ "none", class_ "shrink-0"] do
                  input_ [type_ "hidden", name_ "templateKey", value_ template.key]
                  button_ [type_ "submit", class_ "btn btn-sm gap-1.5 hover:border-strokeBrand-strong hover:text-textBrand active:scale-[0.96] transition-[color,border-color,scale]"] $ faSprite_ "plus" "regular" "h-3 w-3" >> "Add routine"
            div_ [class_ "mt-4 flex items-center gap-2"] do
              let (tone, icon) = categoryBadge template.category
              iconBadge_ IconXs tone icon
              h4_ [class_ "text-sm font-medium text-textStrong"] $ toHtml template.title
            p_ [class_ "mt-3 -indent-[0.4em] text-[0.9375rem] leading-6 text-textStrong text-pretty"] $ toHtml $ "“" <> template.ask <> "”"
            p_ [class_ "mt-2 mb-5 text-sm text-textWeak"] case template.reportWhen of
              Issues.ReportAlways -> "Then posts a report every run"
              Issues.ReportFindings -> "Then reports only when it finds something"
            ul_ [Aria.label_ "Uses", class_ "mt-auto flex flex-wrap gap-1.5 border-t border-strokeWeak pt-4"] $ for_ template.signals \signal -> do
              let (icon, label, color) = signalChip signal
              li_ [class_ "inline-flex items-center gap-1.5 rounded-md bg-fillWeak px-2 py-1 text-xs text-textWeak"] $ faSprite_ icon "regular" ("h-3 w-3 shrink-0 " <> color) >> label
        signalChip = \case
          Issues.SignalLogs -> ("file-lines", "Logs", "text-iconBrand")
          Issues.SignalTraces -> ("list-tree", "Traces", "text-iconBrand")
          Issues.SignalMetrics -> ("chart-line", "Metrics", "text-iconBrand")
          Issues.SignalEndpoints -> ("diagram-project", "Endpoints", "text-iconBrand")
          Issues.SignalVersions -> ("code-branch", "Versions", "text-iconBrand")
          Issues.SignalIssues -> ("bug", "Issues", "text-iconError")
          Issues.SignalIncidents -> ("triangle-exclamation", "Incidents", "text-iconError")
          Issues.SignalSecurity -> ("lock", "Security logs", "text-iconError")
          Issues.SignalMonitors -> ("bell", "Monitors", "text-iconWarning")
        categoryBadge = \case
          Issues.CategoryReliability -> (BrandBadge, "shield-check")
          Issues.CategoryTelemetryQuality -> (SuccessBadge, "server")
          Issues.CategoryDelivery -> (BrandBadge, "rocket")
          Issues.CategoryIncidents -> (ErrorBadge, "triangle-exclamation")
          Issues.CategoryCost -> (WarningBadge, "chart-line")
          Issues.CategorySecurity -> (ErrorBadge, "lock")
        runStatus = \case
          Issues.RunRunning -> (BrandBadge, "Running")
          Issues.RunSucceeded -> (SuccessBadge, "Succeeded")
          Issues.RunNoFindings -> (NeutralBadge, "No findings")
          Issues.RunFailed -> (ErrorBadge, "Failed")
          Issues.RunTimedOut -> (WarningBadge, "Timed out")
          Issues.RunCancelled -> (NeutralBadge, "Cancelled")


threadGetH :: Projects.ProjectId -> UUIDId "conversation" -> Maybe Issues.ArtifactKey -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
threadGetH pid convId docM = do
  (_, _, bw) <- mkAIPageCtx pid
  (conversations, (conversationM, (selectedMessages, artifacts))) <-
    concurrently
      (Issues.listConversations pid)
      $ concurrently (Issues.getConversation pid convId)
      $ concurrently (Issues.selectChatHistory pid convId) (Issues.listArtifacts pid convId docM)
  let activeId = (.conversationId) <$> conversationM
      messages = bool [] selectedMessages (isJust conversationM)
      docs = bool [] artifacts (isJust conversationM)
      openDoc = find (isJust . (.content)) docs
      routineM = conversationM >>= \conversation -> (conversation,) <$> conversation.routineInterval
      pageConfig =
        bw
          { pageTitle = maybe "AI" (.title) conversationM
          , menuItem = Just "AI"
          , activeConversationId = activeId
          , conversations
          , pageActions =
              conversationM >>= \conversation -> case Issues.conversationIssueId conversation of
                Just issueId -> Just $ a_ ([href_ $ "/p/" <> pid.toText <> "/issues/" <> issueId.toText, class_ "btn btn-sm btn-ghost gap-2 active:scale-[0.96] transition-transform", Aria.label_ "View issue"] <> navTabAttrs) do
                  faSprite_ "bug" "regular" "h-3.5 w-3.5"
                  span_ [class_ "max-md:hidden"] "View issue"
                Nothing -> scheduleControl_ conversation.conversationId routineM <$ guard (conversation.conversationType == Issues.CTWeb)
          }
  addRespHeaders $ PageCtx pageConfig $ threadPage_ pid activeId routineM messages docs openDoc
  where
    scheduleControl_ :: UUIDId "conversation" -> Maybe (Issues.ConversationSummary, Issues.RoutineInterval) -> Html ()
    scheduleControl_ conversationId routineState = details_ [class_ "relative group/routine"] do
      summary_ [class_ "btn btn-sm btn-ghost list-none gap-2 cursor-pointer"] do
        faSprite_ "clock" "regular" "h-3.5 w-3.5"
        span_ [class_ "max-md:hidden"] $ toHtml $ maybe "Schedule" (\(conversation, interval) -> bool "Paused" (Issues.routineCadence interval) conversation.routineActive) routineState
      div_ [class_ "absolute end-0 top-full z-30 mt-2 w-64 rounded-xl border border-strokeWeak bg-bgRaised p-3 shadow-lg"] do
        form_ [class_ "space-y-3", hxPost_ $ threadUrl pid conversationId <> "/routine", hxSwap_ "none"] do
          label_ [class_ "block text-xs font-medium text-textStrong"] "Run every"
          div_ [class_ "flex items-center gap-2"] do
            input_ [type_ "number", name_ "intervalMinutes", min_ "5", max_ "10080", value_ $ maybe "1440" (show . Issues.routineIntervalMinutes . snd) routineState, class_ "input input-sm min-w-0 grow", Aria.label_ "Routine interval in minutes"]
            span_ [class_ "text-xs text-textWeak"] "minutes"
          button_ [type_ "submit", class_ "btn btn-sm btn-primary w-full active:scale-[0.96] transition-transform"] "Save routine"
        when (any ((.routineActive) . fst) routineState) $ form_ [class_ "mt-3 border-t border-strokeWeak pt-2", hxPost_ $ threadUrl pid conversationId <> "/routine/pause", hxSwap_ "none"] $ button_ [type_ "submit", class_ "btn btn-sm btn-ghost w-full active:scale-[0.96] transition-transform"] "Pause routine"

    threadPage_ :: Projects.ProjectId -> Maybe (UUIDId "conversation") -> Maybe (Issues.ConversationSummary, Issues.RoutineInterval) -> [Issues.AIChatMessage] -> [Issues.AIArtifact] -> Maybe Issues.AIArtifact -> Html ()
    threadPage_ projectId activeConversationIdM routineM messages docs openDoc = div_ [class_ "h-full flex bg-bgBase"] do
      div_ [class_ "min-w-0 grow flex flex-col"] do
        div_ [id_ "ai-thread-messages", class_ "min-h-0 grow overflow-y-auto"] do
          div_ [class_ "max-w-3xl mx-auto px-5 py-8"] do
            unless (null docs) $ details_ [class_ "lg:hidden mb-6 rounded-xl border border-strokeWeak bg-bgRaised"] do
              summary_ [class_ "flex cursor-pointer list-none items-center gap-2 px-4 py-3 text-sm font-medium text-textStrong"] do
                faSprite_ "file-lines" "regular" "h-3.5 w-3.5 text-iconBrand"
                toHtml $ "Documents (" <> show @Text (length docs) <> ")"
              div_ [class_ "border-t border-strokeWeak p-2"] documentsNav_
            case openDoc of
              Just doc -> documentView_ doc
              Nothing -> do
                for_ routineM routineOverview_
                if hasVisibleHistory messages
                  then do
                    when (isJust routineM) $ h2_ [class_ "mb-2 mt-6 text-xs font-medium uppercase tracking-wide text-textWeak"] "Recent results"
                    IssuePage.aiChatHistoryView_ projectId messages
                  else foldMap routineEmpty_ routineM
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
      unless (null docs) $ aside_ [class_ "hidden lg:block w-64 shrink-0 overflow-y-auto border-l border-strokeWeak p-3"] documentsNav_
      where
        threadHref = foldMap (threadUrl projectId) activeConversationIdM
        docHref doc = threadHref <> "?doc=" <> toText doc.key
        isOpen doc = ((.key) <$> openDoc) == Just doc.key
        (openBody, openToc) = maybe (mempty, []) renderMarkdownToc $ openDoc >>= (.content)
        navLink_ active href = a_ ([href_ href, class_ $ "flex items-center gap-2 rounded-md px-2 py-1.5 text-sm hover:bg-fillHover hover:text-textStrong " <> bool "text-textWeak" "bg-fillWeak font-medium text-textStrong" active] <> [term "aria-current" "page" | active] <> navTabAttrs)

        documentsNav_ :: Html ()
        documentsNav_ = nav_ [Aria.label_ "Documents", class_ "flex flex-col gap-0.5"] do
          h2_ [class_ "px-2 pb-1 pt-1 text-xs font-medium uppercase tracking-wide text-textWeak"] "Documents"
          navLink_ (isNothing openDoc) threadHref do
            faSprite_ "comment" "regular" "h-3.5 w-3.5 shrink-0"
            "Conversation"
          for_ docs \doc -> do
            navLink_ (isOpen doc) (docHref doc) do
              faSprite_ "file-lines" "regular" "h-3.5 w-3.5 shrink-0 text-iconBrand"
              span_ [class_ "min-w-0 grow truncate"] $ toHtml doc.title
            when (isOpen doc) $ ul_ [class_ "mb-1 ms-4 border-s border-strokeWeak ps-2"] $ for_ openToc \(anchor, heading) ->
              li_ $ a_ [href_ $ "#" <> anchor, class_ "block truncate rounded px-2 py-1 text-xs text-textWeak hover:bg-fillHover hover:text-textStrong"] $ toHtml heading

        documentView_ :: Issues.AIArtifact -> Html ()
        documentView_ doc =
          article_
            ( [id_ "ai-document", Aria.label_ doc.title]
                -- Routines keep appending to today's document; keep an open one current.
                <> concat [[hxGet_ $ docHref doc, hxTrigger_ "every 60s", hxSelect_ "#ai-document", hxTarget_ "this", hxSwap_ "outerMorph"] | any ((.routineActive) . fst) routineM]
            )
            do
              p_ [class_ "font-mono text-xs text-textWeak"] $ toHtml $ toText doc.key
              h1_ [class_ "mt-1 text-xl font-semibold text-textStrong text-balance"] $ toHtml doc.title
              p_ [class_ "mt-1 text-xs text-textWeak"] $ "Updated " >> localTimeFmt_ "MMM d, HH:mm" doc.updatedAt
              div_ [class_ "prose prose-sm mt-6 max-w-none [&_h2]:scroll-mt-4"] openBody

        hasVisibleHistory :: [Issues.AIChatMessage] -> Bool
        hasVisibleHistory = any ((`elem` [Issues.ChatAssistant, Issues.ChatExecutionEvent]) . (.role))

        routineOverview_ :: (Issues.ConversationSummary, Issues.RoutineInterval) -> Html ()
        routineOverview_ (conversation, interval) =
          let (statusTone, status) =
                if
                  | isJust conversation.routineRunningSince -> (BrandBadge, "Running now")
                  | conversation.routineActive -> (SuccessBadge, "Active")
                  | otherwise -> (NeutralBadge, "Paused")
              nextRunLabel = maybe "No upcoming run" (toText . formatTime defaultTimeLocale "Next run %-d %b, %H:%M UTC") conversation.routineNextRunAt
           in section_ [class_ "rounded-xl border border-strokeWeak bg-fillWeaker p-4", Aria.label_ "Routine status"] do
                div_ [class_ "flex items-start gap-3"] do
                  span_ [class_ "flex h-9 w-9 shrink-0 items-center justify-center rounded-lg bg-fillWeak text-iconNeutral"]
                    $ faSprite_ "clock" "regular" "h-4 w-4"
                  div_ [class_ "min-w-0 grow"] do
                    div_ [class_ "flex flex-wrap items-center gap-2"] do
                      h2_ [class_ "font-medium text-textStrong"] "Routine schedule"
                      tonePill_ statusTone status
                    p_ [class_ "mt-1 text-sm text-textWeak"] $ toHtml $ Issues.routineCadence interval <> " · " <> nextRunLabel

        routineEmpty_ :: (Issues.ConversationSummary, Issues.RoutineInterval) -> Html ()
        routineEmpty_ (conversation, _) =
          emptyState_ def{icon = Just "clock"} "No completed run yet"
            $ if conversation.routineActive
              then "This routine is scheduled. Its first result will appear here after the next run."
              else "This routine is paused. Resume its schedule to produce a new result."


routineInstallPostH :: Projects.ProjectId -> RoutineTemplateForm -> ATAuthCtx (RespHeaders (Html ()))
routineInstallPostH pid form = do
  (_, project, _) <- mkAIPageCtx pid
  case find ((== form.templateKey) . (.key)) Issues.routineTemplates of
    Nothing -> reject "Unknown routine template."
    Just template -> do
      existing <- find ((== Just template.key) . (.templateKey)) <$> Issues.listConversations pid
      case existing of
        Just conversation -> redirectToThread pid conversation.conversationId
        Nothing -> do
          convId <- UUIDId <$> UUID.genUUID
          void $ Issues.getOrCreateConversation pid convId Issues.CTWeb (AE.object ["routine_template" AE..= template.key])
          Issues.renameConversation pid convId template.title
          Issues.insertChatMessage pid convId Issues.ChatUser template.prompt Nothing Nothing
          enqueue $ Issues.installRoutineTemplate pid convId project.timeZone template
          redirectToThread pid convId


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
        NewRoutine prompt interval -> submitTurn pid convId prompt >> enqueue (Issues.upsertRoutine pid convId interval)
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
  let title = T.strip form.conversationTitle
  if T.null title
    then reject "Enter a title."
    else Issues.renameConversation pid convId title >> redirectToThread pid convId


threadDeleteH :: Projects.ProjectId -> UUIDId "conversation" -> ATAuthCtx (RespHeaders (Html ()))
threadDeleteH pid convId = authorizeProject pid >> Issues.deleteConversation pid convId >> redirect pid ""


submitTurn :: Projects.ProjectId -> UUIDId "conversation" -> Text -> ATAuthCtx ()
submitTurn pid convId prompt = do
  appCtx <- ask @AuthContext
  result <- Bots.processAIQueryWithMode (Just appCtx.config) appCtx.env.enableTimefusionReads AI.InteractiveWithActions AI.ServiceAccess pid prompt (Just convId) appCtx.config.openaiModel appCtx.config.openaiApiKey
  whenLeft_ result \err -> Issues.insertChatMessage pid convId Issues.ChatAssistant ("I couldn't complete that request: " <> err) Nothing Nothing


normalizeQuery :: Text -> Either Text Text
normalizeQuery = (\prompt -> if T.null prompt then Left "Enter a prompt to start." else if T.length prompt > 4000 then Left "Prompt is limited to 4,000 characters." else Right prompt) . T.strip


normalizeNewThread :: AIChatForm -> Either Text NewThread
normalizeNewThread form = do
  prompt <- normalizeQuery form.query
  case (form.mode, form.intervalMinutes) of
    (ModeChat, Nothing) -> Right $ NewChat prompt
    (ModeChat, Just _) -> Left "Chat mode does not accept a routine interval."
    (ModeRoutine, Nothing) -> Left "Choose a routine interval."
    (ModeRoutine, Just interval) -> Right $ NewRoutine prompt interval


reject :: Text -> ATAuthCtx (RespHeaders (Html ()))
reject message = addErrorToast message Nothing >> addRespHeaders mempty


authorizeProject :: Projects.ProjectId -> ATAuthCtx ()
authorizeProject = void . Projects.sessionAndProject


threadUrl :: Projects.ProjectId -> UUIDId "conversation" -> Text
threadUrl pid convId = "/p/" <> pid.toText <> "/ai/" <> convId.toText


-- | HTMX redirect to @path@ under the project's AI section.
redirect :: Projects.ProjectId -> Text -> ATAuthCtx (RespHeaders (Html ()))
redirect pid path = redirectCS ("/p/" <> pid.toText <> "/ai" <> path) >> addRespHeaders mempty


redirectToThread :: Projects.ProjectId -> UUIDId "conversation" -> ATAuthCtx (RespHeaders (Html ()))
redirectToThread pid convId = redirect pid ("/" <> convId.toText)


-- | Schedule the routine's next run, if the write produced one.
enqueue :: ATAuthCtx (Maybe Issues.ScheduledRoutine) -> ATAuthCtx ()
enqueue write = do
  appCtx <- ask @AuthContext
  whenJustM write \routine -> BackgroundJobs.enqueueAIRoutine appCtx routine.id routine.scheduledAt


routinePostH :: Projects.ProjectId -> UUIDId "conversation" -> RoutineForm -> ATAuthCtx (RespHeaders (Html ()))
routinePostH pid convId form = do
  authorizeProject pid
  whenJustM (Issues.getConversation pid convId) \conversation ->
    when (conversation.conversationType == Issues.CTWeb) $ enqueue (Issues.upsertRoutine pid convId form.intervalMinutes)
  redirectToThread pid convId


routinePausePostH :: Projects.ProjectId -> UUIDId "conversation" -> ATAuthCtx (RespHeaders (Html ()))
routinePausePostH pid convId = authorizeProject pid >> Issues.pauseRoutine pid convId >> redirectToThread pid convId


routineCancelPostH, routineResumePostH, routineDeleteH :: Projects.ProjectId -> UUIDId "conversation" -> ATAuthCtx (RespHeaders (Html ()))
routineCancelPostH pid convId = authorizeProject pid >> Issues.cancelRoutineRun pid convId >> redirect pid "/routines"
routineResumePostH pid convId = authorizeProject pid >> enqueue (Issues.resumeRoutine pid convId) >> redirect pid "/routines"
routineDeleteH pid convId = authorizeProject pid >> Issues.deleteRoutine pid convId >> redirect pid "/routines"


routineDestinationPostH :: Projects.ProjectId -> UUIDId "conversation" -> RoutineDestinationForm -> ATAuthCtx (RespHeaders (Html ()))
routineDestinationPostH pid convId form = authorizeProject pid >> Issues.setRoutineDestination pid convId form.destination >> redirect pid "/routines"
