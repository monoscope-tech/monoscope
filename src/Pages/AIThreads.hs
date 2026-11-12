{-# LANGUAGE OrPatterns #-}

module Pages.AIThreads (AIPage (..), PolicySaved (..), PromptError (..), NewThreadError (..), TurnError (..), EmptyTitle (..), UnknownTemplate (..), NoReviewClasses (..), OperationReviewed (..), AIChatForm (..), RoutineForm (..), RoutineTemplateForm (..), RoutineDestinationForm (..), RoutinePolicyForm (..), OperationReviewForm (..), operationReviewPostH, operationGetH, RoutineInstructionsForm (..), TitleForm (..), threadsGetH, threadGetH, routinesGetH, routineRunGetH, routineInstructionsPostH, routineInstallPostH, startThreadPostHWithTools, threadPostHWithTools, threadTitlePostH, threadDeleteH, routinePostH, routinePausePostH, routineCancelPostH, routineResumePostH, routineDestinationPostH, routinePolicyPostH, routineDeleteH, aiSearchH) where

import BackgroundJobs qualified
import BackgroundJobs.Types qualified as Jobs
import Data.Aeson qualified as AE
import Data.Aeson.Encode.Pretty qualified as AEP
import Data.Aeson.Types qualified as AET
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.Effectful.UUID qualified as UUID
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.UUID.V5 qualified as UUID5
import Effectful.Concurrent.Async (concurrently)
import Effectful.Error.Static (throwError)
import Effectful.Reader.Static (ask)
import Lucid hiding (for_)
import Lucid qualified
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxConfirm_, hxDelete_, hxGet_, hxIndicator_, hxPost_, hxSelect_, hxSwap_, hxTarget_, hxTrigger_, hxVals_)
import Lucid.Hyperscript (__)
import Models.Apis.Investigations qualified as Investigations
import Models.Apis.Issues qualified as Issues
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkAIPageCtx, navTabAttrs)
import Pages.Bots.Utils qualified as Bots
import Pages.CodeContext qualified as CodeContext
import Pages.Components (BadgeColor (..), EmptyStateCfg (..), EmptyStateSize (..), IconBadgeSize (..), emptyState_, iconBadge_, localTimeFmt_, tonePill_)
import Pages.Issues qualified as IssuePage
import Pkg.AI qualified as AI
import Pkg.AgentCatalog qualified as Catalog
import Pkg.Components.Table (Config (..), EmptyStateAction (..), Features (..), Table (..), ZeroState (ZeroState), col, withAttrs)
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..), encodeEnumSC)
import Relude hiding (ask)
import Servant qualified
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, ActionResult, Done, Outcome (..), Refusal (..), RespHeaders, Says (..), act, addErrorToast, addRespHeaders, done)
import Utils (faSprite_, popoverPanel_, popoverTrigger_, renderMarkdownTocWithPrefix)
import Web.FormUrlEncoded (FromForm (..), parseAll, parseUnique)
import Web.HttpApiData (FromHttpApiData (..))
import Web.Links
import Web.MCP qualified as MCP


-- | Refusals of the JSON search endpoint; status and body are what the query box reads.
data AISearchError = MalformedSearch | EmptySearch | SearchFailed Text


aiSearchH :: Projects.ProjectId -> AE.Value -> ATAuthCtx (RespHeaders AE.Value)
aiSearchH pid requestBody = do
  _ <- Projects.sessionAndProject pid
  authCtx <- ask @AuthContext
  let parsed = AET.parseMaybe (AE.withObject "request" \o -> liftA2 (,) (o AE..: "input") (o AE..:? "timezone")) requestBody
  (inputText, timezoneM) <- parsed `whenNothing` refuse MalformedSearch
  when (T.null (T.strip inputText)) $ refuse EmptySearch
  either (refuse . SearchFailed) addRespHeaders =<< AI.runNlSearch pid authCtx.env timezoneM inputText
  where
    refuse :: AISearchError -> ATAuthCtx a
    refuse e = addErrorToast toast detail >> throwError status{Servant.errBody = body}
      where
        (toast, detail, status, body) = case e of
          MalformedSearch -> ("Invalid AI search input", Nothing, Servant.err400, "Invalid input format")
          EmptySearch -> ("Please enter a search query", Nothing, Servant.err400, "Empty input")
          SearchFailed msg -> ("AI search failed", Just msg, Servant.err502, encodeUtf8 msg)


data AIChatForm = AIChatForm
  { query :: Text
  , mode :: ComposerMode
  , intervalMinutes :: Maybe Issues.RoutineInterval
  , allowActions :: Maybe Bool
  , requestId :: Maybe (UUIDId "ai_routine_run")
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


newtype RoutineForm = RoutineForm {intervalMinutes :: Issues.RoutineInterval}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


newtype RoutineTemplateForm = RoutineTemplateForm {templateKey :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


newtype RoutineInstructionsForm = RoutineInstructionsForm {instructions :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


newtype RoutineDestinationForm = RoutineDestinationForm {destination :: Issues.RoutineDestination}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data RoutinePolicyForm = RoutinePolicyForm Issues.ExecutionPolicy | MissingReviewClasses
  deriving stock (Show)


-- $setup
-- >>> import Relude
-- >>> import Data.Either (isLeft)
-- >>> import Web.FormUrlEncoded (urlDecodeAsForm)
-- >>> import Models.Apis.Issues qualified as Issues
-- >>> import Pkg.AgentCatalog qualified as Catalog


-- | Parse selected review into a nonempty policy before it reaches the handler.
--
-- >>> let decode = urlDecodeAsForm @RoutinePolicyForm
-- >>> decode "policy=autonomous"
-- Right (RoutinePolicyForm Autonomous)
-- >>> case decode "policy=review&reviewClass=project_changes&reviewApprover=editors" of Right (RoutinePolicyForm value) -> value == Issues.ReviewSelected (Catalog.ReviewSelection (one Catalog.ProjectChanges) Catalog.ReviewEditors); _ -> False
-- True
-- >>> decode "policy=review&reviewApprover=editors"
-- Right MissingReviewClasses
-- >>> isLeft (decode "policy=review&reviewClass=unknown&reviewApprover=editors")
-- True
-- >>> isLeft (decode "policy=review&reviewClass=project_changes&reviewApprover=unknown")
-- True
-- >>> isLeft (decode "policy=observe&policy=autonomous")
-- True
instance FromForm RoutinePolicyForm where
  fromForm form =
    parseUnique @Text "policy" form >>= \case
      "review" -> do
        classes <- parseAll "reviewClass" form
        approver <- parseUnique "reviewApprover" form
        case classes of
          [] -> Right MissingReviewClasses
          actionClass : others -> pure $ RoutinePolicyForm $ Issues.ReviewSelected $ Catalog.ReviewSelection (actionClass :| others) approver
      value -> RoutinePolicyForm <$> parseUrlPiece value


newtype OperationReviewForm = OperationReviewForm {decision :: Investigations.ReviewDecision}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data ComposerMode = ModeChat | ModeRoutine
  deriving stock (Generic, Read, Show)
  deriving (FromHttpApiData) via WrappedEnumSC 'Nothing "Mode" ComposerMode


data NewThread = NewChat Text | NewRoutine Text Issues.RoutineInterval


-- | The AI page an action leaves the browser on.
data AIPage = ThreadPage Projects.ProjectId (UUIDId "conversation") | RoutinesPage Projects.ProjectId | ThreadsPage Projects.ProjectId
  deriving stock (Eq, Show)


instance Outcome AIPage where
  outcomeRedirect =
    Just . \case
      ThreadPage pid convId -> threadUrl pid convId
      RoutinesPage pid -> urls.aiRoutinesGet pid
      ThreadsPage pid -> urls.aiThreadsGet pid


data PromptError = EmptyPrompt | PromptTooLong
  deriving stock (Eq, Show)


instance Refusal PromptError where
  refusalMessage = \case
    EmptyPrompt -> "Enter a prompt to start."
    PromptTooLong -> "Prompt is limited to 4,000 characters."


data NewThreadError = NewThreadPrompt PromptError | ChatWithInterval | RoutineWithoutInterval
  deriving stock (Eq, Show)


instance Refusal NewThreadError where
  refusalMessage = \case
    NewThreadPrompt e -> refusalMessage e
    ChatWithInterval -> "Chat mode does not accept a routine interval."
    RoutineWithoutInterval -> "Choose a routine interval."


data TurnError = TurnPrompt PromptError | ConversationGone
  deriving stock (Eq, Show)


instance Refusal TurnError where
  refusalMessage = \case
    TurnPrompt e -> refusalMessage e
    ConversationGone -> "This conversation no longer exists."


data EmptyTitle = EmptyTitle
  deriving stock (Eq, Generic, Show)
  deriving (Refusal) via Says "Enter a title." EmptyTitle


data UnknownTemplate = UnknownTemplate
  deriving stock (Eq, Generic, Show)
  deriving (Refusal) via Says "Unknown routine template." UnknownTemplate


data NoReviewClasses = NoReviewClasses
  deriving stock (Eq, Generic, Show)
  deriving (Refusal) via Says "Choose at least one action class for review." NoReviewClasses


newtype TitleForm = TitleForm {conversationTitle :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


threadsGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
threadsGetH pid = do
  (_, _, bw) <- mkAIPageCtx pid
  allConversations <- visibleConversations pid
  let pageConfig =
        bw
          { pageTitle = "Conversations"
          , menuItem = Just "AI"
          , conversations = allConversations
          , pageActions = Just $ button_ [type_ "button", Aria.label_ "New chat", class_ "btn btn-sm btn-primary gap-2 active:scale-[0.96] transition-transform", [__|on click set #ai-composer-input.value to '' then set #ai-composer-chat.checked to true then call #ai-composer-modal.showModal()|]] do
              faSprite_ "plus" "regular" "h-3.5 w-3.5"
              span_ [class_ "max-md:hidden"] "New chat"
          }
  addRespHeaders $ PageCtx pageConfig $ conversationsPage_ $ filter Issues.isPersonConversation allConversations
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
  canEdit <- canEditRoutines pid
  (session, _, bw) <- mkAIPageCtx pid
  role <- currentViewerPermission pid
  (conversations, runs) <- concurrently (visibleConversations pid) (Issues.listVisibleRoutineRuns pid role (Investigations.MemberViewer session.user.id) 12)
  let installed = filter (isJust . (.routineKind)) conversations
      pageConfig =
        bw
          { pageTitle = "Routines"
          , menuItem = Just "AI"
          , conversations
          , pageActions = Just $ button_ [type_ "button", Aria.label_ "New custom routine", class_ "btn btn-sm btn-primary gap-2 active:scale-[0.96] transition-transform", [__|on click set #ai-composer-input.value to '' then set #ai-composer-routine.checked to true then call #ai-composer-modal.showModal()|]] do
              faSprite_ "plus" "regular" "h-3.5 w-3.5"
              span_ [class_ "max-md:hidden"] "New custom routine"
          }
  addRespHeaders $ PageCtx pageConfig{pageActions = pageConfig.pageActions <* guard canEdit} $ routinesPage_ canEdit installed runs
  where
    routinesPage_ :: Bool -> [Issues.ConversationSummary] -> [Issues.RoutineRun] -> Html ()
    routinesPage_ canEdit installedRoutines recentRuns = div_ [class_ "h-full overflow-y-auto bg-bgBase"] do
      main_ [class_ "mx-auto max-w-6xl space-y-10 px-4 py-6 sm:px-6 sm:py-8"] do
        p_ [class_ "max-w-2xl text-sm text-textWeak"] "AI tasks that run on a schedule or when something happens in your project. Each run keeps its result here, and reports keep a dated copy."
        section_ [Aria.label_ "Installed routines"] do
          div_ [class_ "mb-2 flex items-baseline justify-between"] do
            h2_ [class_ "text-sm font-semibold text-textStrong"] "Installed"
            span_ [class_ "text-xs text-textWeak tabular-nums"] $ toHtml $ show (length installedRoutines) <> " routine" <> bool "s" "" (length installedRoutines == 1)
          if null installedRoutines
            then emptyState_ def{icon = Just "clock", size = ESCompact} "No routines installed" "Add a built-in routine below or create one from a conversation."
            else
              toHtml
                Table
                  { config = def{elemID = "ai-routines-installed", renderAsTable = True, tdClasses = "px-4 py-2.5"}
                  , columns =
                      [ col
                          "Routine"
                          ( \r -> a_ [href_ $ threadUrl pid r.conversationId, class_ "block min-w-0 rounded-sm focus-visible:outline-2 focus-visible:outline-offset-2"] do
                              div_ [class_ "truncate text-sm font-medium text-textStrong"] $ toHtml r.title
                              div_ [class_ "truncate text-xs text-textWeak"] $ toHtml $ maybe (foldMap Issues.routineKindLabel r.routineKind) Issues.routineCadence r.routineInterval
                          )
                          -- Takes the leftover width and truncates, so the narrow columns stay visible on phones.
                          & withAttrs [class_ "w-full max-w-0"]
                      , col "Status" (uncurry tonePill_ . routineStatus) & withAttrs [class_ "w-28 whitespace-nowrap"]
                      , col
                          "Last run"
                          ( \r -> span_ [class_ "flex items-center gap-2 whitespace-nowrap text-xs tabular-nums text-textWeak"] case (r.routineLastRunAt, r.routineLastRunStatus) of
                              (Just at, Just status) -> toHtml (formatTime defaultTimeLocale "%e %b, %H:%M" at) >> uncurry tonePill_ (runStatus status)
                              _ -> "Not run yet"
                          )
                          & withAttrs [class_ "w-56 max-md:hidden"]
                      , col "Next run" (\r -> span_ [class_ "whitespace-nowrap text-xs tabular-nums text-textWeak"] $ maybe "—" (toHtml . formatTime defaultTimeLocale "%e %b, %H:%M") (r.routineNextRunAt <* guard r.routineActive)) & withAttrs [class_ "w-36 max-lg:hidden"]
                      ]
                        <> [ col
                               ""
                               ( \r -> div_ [class_ "flex items-center justify-end gap-1"] do
                                   let (toggle, toggleLabel) = bool (\p c -> urls.aiRoutineResumePost p c False, "Resume") (urls.aiRoutinePausePost, "Pause") r.routineActive
                                       menuId = "routine-menu-" <> r.conversationId.toText
                                   form_ [hxPost_ $ toggle pid r.conversationId, hxSwap_ "none"] $ button_ [class_ "btn btn-xs btn-ghost active:scale-[0.96] transition-transform", type_ "submit"] $ toHtml @Text toggleLabel
                                   button_ ([type_ "button", class_ "btn btn-xs btn-ghost btn-square", Aria.label_ $ "More actions for " <> r.title] <> popoverTrigger_ menuId) $ faSprite_ "ellipsis" "regular" "h-3.5 w-3.5"
                                   ul_ ([class_ "dropdown dropdown-end menu menu-sm w-52 rounded-box border border-strokeWeak bg-bgRaised p-1 shadow-lg"] <> popoverPanel_ menuId) do
                                     when (isJust r.routineRunningSince) $ li_ $ button_ [hxPost_ $ urls.aiRoutineCancelPost pid r.conversationId, hxSwap_ "none", class_ "text-textWarning"] "Stop active runs"
                                     when (isJust r.routineInterval) do
                                       li_ [class_ "menu-title"] "Deliver results"
                                       for_ [Issues.DestinationConversation, Issues.DestinationEmail, Issues.DestinationSlack] \destination ->
                                         li_ $ button_ [hxPost_ $ urls.aiRoutineDestinationPost pid r.conversationId, hxSwap_ "none", hxVals_ $ "{\"destination\":\"" <> toText (encodeEnumSC @"Destination" destination) <> "\"}", term "aria-pressed" $ bool "false" "true" (r.routineDestination == Just destination)] do
                                           span_ [class_ "w-3"] $ when (r.routineDestination == Just destination) $ faSprite_ "check" "solid" "h-3 w-3"
                                           toHtml $ destinationLabel destination
                                     li_ $ button_ [hxDelete_ $ urls.aiRoutineDelete pid r.conversationId, hxConfirm_ "Delete this routine? Its conversation and past results will be kept.", hxSwap_ "none", class_ "text-textError"] "Delete"
                               )
                               & withAttrs [class_ "w-28 text-end", Aria.label_ "Actions"]
                           | canEdit
                           ]
                  , rows = fromList installedRoutines
                  , features = def
                  }
        unless (null recentRuns) $ section_ [Aria.label_ "Recent routine runs"] do
          h2_ [class_ "mb-2 text-sm font-semibold text-textStrong"] "Recent runs"
          toHtml
            Table
              { config = def{elemID = "ai-routines-runs", renderAsTable = True, tdClasses = "px-4 py-2.5"}
              , columns =
                  [ col
                      "Routine"
                      ( \run -> div_ [class_ "min-w-0"] do
                          a_ [href_ $ runUrl pid run.id, class_ "block truncate text-sm font-medium text-textStrong hover:text-textBrand"] $ toHtml $ fromMaybe "Deleted routine" run.title
                          whenJust (run.error <|> (summaryLine =<< run.summary)) \line -> p_ [class_ $ "mt-0.5 truncate text-xs " <> maybe "text-textWeak" (const $ runErrorTone run.status) run.error, title_ line] $ toHtml line
                      )
                      & withAttrs [class_ "w-full max-w-0"]
                  , col "Outcome" (uncurry tonePill_ . runStatus . (.status)) & withAttrs [class_ "w-32"]
                  , col "Started" (\run -> time_ [datetime_ $ show run.startedAt, class_ "whitespace-nowrap text-xs tabular-nums text-textWeak"] $ toHtml $ formatTime defaultTimeLocale "%e %b, %H:%M UTC" run.startedAt) & withAttrs [class_ "w-40 max-sm:hidden"]
                  ]
              , rows = fromList recentRuns
              , features = def{rowLink = Just $ runUrl pid . (.id)}
              }
        section_ [Aria.label_ "Routine catalog"] do
          h2_ [class_ "text-sm font-semibold text-textStrong"] "Built-in routines"
          p_ [class_ "mt-1 max-w-3xl text-sm text-textWeak"] "Times use the project’s time zone. New routines act within your role without per-action approval; change that under a routine’s Settings. AI usage is billed at your project rate."
          let categories = [minBound .. maxBound]
          nav_ [Aria.label_ "Routine categories", class_ "mt-4 flex gap-2 overflow-x-auto pb-1"] $ for_ categories \category ->
            a_ [href_ $ "#" <> categoryId category, class_ "shrink-0 rounded-md border border-strokeWeak bg-bgRaised px-3 py-1.5 text-xs font-medium text-textWeak hover:bg-fillHover hover:text-textStrong"] $ toHtml $ Issues.routineCategoryLabel category
          div_ [class_ "mt-6 space-y-8"] $ for_ categories \category -> section_ [id_ $ categoryId category, class_ "scroll-mt-16"] do
            h3_ [class_ "mb-3 text-sm font-semibold text-textStrong"] $ toHtml $ Issues.routineCategoryLabel category
            div_ [class_ "grid gap-4 md:grid-cols-2 xl:grid-cols-3"] $ for_ (filter ((== category) . (.category)) Issues.routineTemplates) templateCard_
      where
        categoryId = ("routine-category-" <>) . T.toLower . T.replace " " "-" . Issues.routineCategoryLabel
        templateCard_ template = do
          let added = template.key `elem` mapMaybe (.templateKey) installedRoutines
          article_ [id_ $ "routine-" <> template.key, class_ "scroll-mt-16 flex flex-col rounded-xl border border-strokeWeak bg-bgRaised p-5 transition-[border-color,box-shadow] duration-150 hover:border-strokeStrong hover:shadow-md"] do
            div_ [class_ "flex min-h-8 items-center justify-between gap-3"] do
              span_ [class_ "whitespace-nowrap rounded-md bg-fillWeak px-2 py-0.5 font-mono text-xs text-textStrong"] $ toHtml $ Issues.routineScheduleLabel template.schedule
              if added
                then span_ [class_ "inline-flex shrink-0 items-center gap-1.5 text-xs font-medium text-textSuccess"] $ faSprite_ "check" "solid" "h-3 w-3" >> "Added"
                else when canEdit $ form_ [hxPost_ $ urls.aiRoutineInstallPost pid, hxSwap_ "none", class_ "shrink-0"] do
                  input_ [type_ "hidden", name_ "templateKey", value_ template.key]
                  button_ [type_ "submit", Aria.label_ $ "Add " <> template.title, class_ "btn btn-sm gap-1.5 hover:border-strokeBrand-strong hover:text-textBrand active:scale-[0.96] transition-[color,border-color,scale]"] $ faSprite_ "plus" "regular" "h-3 w-3" >> "Add routine"
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


threadGetH :: Projects.ProjectId -> UUIDId "conversation" -> Maybe Issues.ArtifactKey -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
threadGetH pid convId docM = threadPageGetH pid convId docM Nothing


threadPageGetH :: Projects.ProjectId -> UUIDId "conversation" -> Maybe Issues.ArtifactKey -> Maybe Issues.RoutineRun -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
threadPageGetH pid convId docM runM hxTarget = do
  requestId <- UUIDId <$> UUID.genUUID
  canEdit <- canEditRoutines pid
  (_, _, bw) <- mkAIPageCtx pid
  viewerPermission <- currentViewerPermission pid
  for_ (convId : map (.conversationId) (maybeToList runM)) $ authorizeConversation pid
  -- The open document's refresh keeps only #ai-document (htmx 4 sends `tag#id`), so skip what it discards.
  let unlessRefresh :: Applicative f => f [a] -> f [a]
      unlessRefresh q = if any ("#ai-document" `T.isSuffixOf`) hxTarget then pure [] else q
  (conversations, (conversationM, (selectedMessages, artifacts))) <-
    concurrently
      (unlessRefresh $ visibleConversations pid)
      $ concurrently (Issues.getConversation pid convId)
      $ concurrently (unlessRefresh $ Issues.selectChatHistory pid convId) (Issues.listArtifacts pid convId docM)
  instructions <- Issues.getRoutineInstructions pid convId
  runs <- Issues.listConversationRuns pid convId 20
  -- A report routine's latest result reads as its digest, each highlight beside its own chart.
  reportM <- case (conversationM >>= (.templateKey) >>= Issues.reportWindowDays, find ((== Issues.RunSucceeded) . (.status)) runs) of
    (Just days, Just run) ->
      Issues.getRoutineRunEvidence pid run.id <&> \evidence ->
        (evidence >>= (.findings) >>= AET.parseMaybe AE.parseJSON) >>= fmap fst . AI.reportRunAnswer pid (addUTCTime (fromInteger $ negate days * 86400) run.startedAt, run.startedAt)
    _ -> pure Nothing
  runContent <- forM runM \run -> do
    evidence <- Issues.getRoutineRunEvidence pid run.id >>= maybe (throwError Servant.err404) pure
    requestedBy <- maybe (pure Nothing) Projects.userById evidence.requestedBy
    runArtifacts <- Issues.listRunArtifacts pid run.id
    operations <- Investigations.listOperations @AI.OperationPolicy pid run.id
    fixEvidence <- case run.source of
      Issues.FixRun identifier | run.status == Issues.RunSucceeded && viewerPermission >= ProjectMembers.PAdmin -> Just <$> CodeContext.issueFixEvidenceH pid identifier
      (Issues.ScheduledRun; Issues.TriggeredRun{}; Issues.InteractiveRun{}; Issues.ConversationRun{}; Issues.FixRun{}) -> pure Nothing
    pure $ runView_ (isNothing conversationM) run evidence requestedBy runArtifacts viewerPermission operations fixEvidence
  let activeId = (.conversationId) <$> conversationM
      messages = bool [] selectedMessages (isJust conversationM)
      -- 'latest-result' mirrors the newest run, already shown as the latest result and under Runs.
      docs = filter ((/= "latest-result") . toText . (.key)) $ bool [] artifacts (isJust conversationM)
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
                Just issueId -> Just $ a_ ([href_ $ (urls.issues pid).detailGet issueId Nothing Nothing Nothing Nothing Nothing, class_ "btn btn-sm btn-ghost gap-2 active:scale-[0.96] transition-transform", Aria.label_ "View issue"] <> navTabAttrs) do
                  faSprite_ "bug" "regular" "h-3.5 w-3.5"
                  span_ [class_ "max-md:hidden"] "View issue"
                Nothing -> scheduleControl_ conversation.conversationId routineM <$ guard (canEdit && conversation.conversationType == Issues.CTWeb && (isNothing conversation.routineKind || isJust conversation.routineInterval))
          }
  addRespHeaders $ PageCtx pageConfig $ threadPage_ requestId canEdit instructions runs reportM runContent conversationM pid activeId routineM (maybe True (isNothing . Issues.conversationIssueId) conversationM) messages docs openDoc
  where
    runStatusId :: Issues.RoutineRun -> Text
    runStatusId run = "routine-run-status-" <> run.id.toText

    runView_ :: Bool -> Issues.RoutineRun -> Issues.RoutineRunEvidence -> Maybe Projects.User -> [Issues.AIArtifact] -> ProjectMembers.Permissions -> [Investigations.OperationEntry AI.OperationPolicy] -> Maybe (Html ()) -> Html ()
    runView_ retained run evidence requestedBy artifacts viewerPermission operations fixEvidence =
      section_
        ([id_ "routine-run", class_ "space-y-5"] <> [attribute | run.status == Issues.RunRunning, attribute <- [hxGet_ $ urls.aiRoutineRunGet pid run.id (Just convId), hxTrigger_ "every 5s", hxSelect_ "#routine-run", hxTarget_ "this", hxSwap_ "outerMorph", term "hx-select-oob" $ "#" <> runStatusId run <> ":outerHTML"]])
        do
          header_ [class_ "space-y-2"] do
            div_ [class_ "flex items-baseline justify-between gap-3"] do
              h2_ [class_ "min-w-0 text-base font-semibold text-textStrong break-words"] $ toHtml $ fromMaybe "Run" run.title
              when (run.conversationId /= convId) $ a_ [href_ $ threadUrl pid run.conversationId, class_ "shrink-0 text-xs font-medium text-textBrand hover:underline"] "Open thread"
            div_ [class_ "flex flex-wrap items-center gap-x-2 gap-y-1 text-xs text-textWeak [&>*+*]:before:me-2 [&>*+*]:before:text-textDisabled [&>*+*]:before:content-['·']"] do
              span_ [role_ "status"] $ uncurry tonePill_ $ runStatus run.status
              time_ [datetime_ $ show run.startedAt, class_ "tabular-nums"] $ toHtml $ formatTime defaultTimeLocale "%e %b %Y, %H:%M UTC" run.startedAt
              span_ $ toHtml $ case run.source of
                (Issues.InteractiveRun _; Issues.ConversationRun _) -> "Web request"
                Issues.FixRun{} -> "Code fix"
                Issues.ScheduledRun -> "Scheduled"
                Issues.TriggeredRun (Issues.SlackMessage _) -> "Slack request"
                Issues.TriggeredRun (Issues.SlackCommand _) -> "Slack command"
                Issues.TriggeredRun (Issues.PullRequestReview _) -> "Pull request"
                Issues.TriggeredRun (Issues.IssueEnrichment _) -> "New issue"
                Issues.TriggeredRun (Issues.IncidentEvent _ _) -> "Incident event"
              span_ $ toHtml $ "Attempt " <> show @Text run.attempt
              span_ [title_ "Role upgrades apply to new requests. Current access can restrict this run further."] $ toHtml $ "Role limit: " <> toText (encodeEnumSC @"P" evidence.permissionCeiling)
              whenJust requestedBy $ \user -> do
                let name = T.strip $ user.firstName <> " " <> user.lastName
                span_ [class_ "min-w-0 break-words"] $ toHtml $ "Requested by " <> if T.null name then CI.original user.email else name
          sequence_ fixEvidence
          whenJust run.error $ p_ [class_ $ "rounded-lg border border-strokeWeak p-4 text-sm break-words " <> runErrorTone run.status, role_ "status"] . toHtml
          whenJust evidence.input $ \input -> section_ [Aria.label_ "Run request"] do
            h2_ [class_ "mb-2 text-sm font-semibold text-textStrong"] "Request"
            p_ [class_ "whitespace-pre-wrap break-words text-sm text-textStrong"] $ toHtml input
          disclosure_ [term "hx-morph-skip" ""] (case run.source of Issues.FixRun{} -> "Fix workflow"; (Issues.ScheduledRun; Issues.TriggeredRun{}; Issues.InteractiveRun{}; Issues.ConversationRun{}) -> "Instructions used for this run") do
            p_ [class_ "whitespace-pre-wrap break-words text-sm text-textWeak"] $ toHtml $ fromMaybe (case run.source of Issues.FixRun{} -> "The fix must reproduce the issue, pass the retained regression, and pass independent review."; (Issues.ScheduledRun; Issues.TriggeredRun{}; Issues.InteractiveRun{}; Issues.ConversationRun{}) -> "This run predates instruction snapshots.") evidence.instructions
            whenJust evidence.templateVersion $ \version -> p_ [class_ "mt-2 text-xs text-textWeak"] $ toHtml $ "Template version " <> show @Text version
          unless (null operations) $ section_ [Aria.label_ "Action history", class_ "space-y-3"] do
            div_ [class_ "flex items-center justify-between gap-3"] do
              h2_ [class_ "text-sm font-semibold text-textStrong"] "Action history"
              let refresh = urls.aiRoutineRunGet pid run.id (Just convId)
              a_ [href_ refresh, hxGet_ refresh, hxSelect_ "#routine-run", hxTarget_ "#routine-run", hxSwap_ "outerMorph", term "hx-preload" "false", class_ "btn btn-xs btn-ghost"] "Refresh actions"
            p_ [class_ "text-xs text-textWeak"] "Most recent 200 actions for this request, including unresolved earlier actions. A saved response can contain a tool error."
            for_ operations \operation -> article_ [id_ $ "operation-" <> operation.id.toText, class_ "space-y-2 rounded-lg border border-strokeWeak p-3"] do
              div_ [class_ "flex flex-wrap items-center justify-between gap-2"] do
                code_ [class_ "min-w-0 break-all text-sm text-textStrong"] $ toHtml $ T.replace "_" " " $ Catalog.operationName operation.proposal.call
                uncurry tonePill_ $ case operation.outcome of
                  Investigations.OperationRefused Investigations.ReviewDeclined -> (NeutralBadge, "Declined")
                  Investigations.OperationRefused Investigations.ReviewRequestEnded -> (NeutralBadge, "Request ended")
                  Investigations.OperationRefused Investigations.ReviewAuthorityWithdrawn -> (WarningBadge, "Approval withdrawn")
                  Investigations.OperationRefused Investigations.ReviewTimedOut -> (WarningBadge, "Review expired")
                  Investigations.OperationUnstarted -> case operation.review of
                    Investigations.NoReview -> (NeutralBadge, "Not started")
                    Investigations.ReviewPending{} -> (BrandBadge, "Awaiting review")
                    Investigations.ReviewResolved _ receipt -> case receipt.decision of
                      Investigations.ReviewApproved -> (BrandBadge, "Approved")
                      Investigations.ReviewRejected -> (NeutralBadge, "Declined")
                  Investigations.OperationRunning -> (BrandBadge, "Awaiting outcome")
                  Investigations.OperationUnknown -> (WarningBadge, "Outcome uncertain")
                  Investigations.OperationReturned{} -> (NeutralBadge, "Response saved")
              case operation.outcome of
                Investigations.OperationRefused reason -> p_ [class_ "text-sm text-textWeak"] $ toHtml $ Investigations.reviewRefusalMessage reason
                (Investigations.OperationUnstarted; Investigations.OperationRunning; Investigations.OperationUnknown; Investigations.OperationReturned{}) -> pass
              when (case operation.outcome of Investigations.OperationUnknown -> True; (Investigations.OperationUnstarted; Investigations.OperationRunning; Investigations.OperationReturned{}; Investigations.OperationRefused{}) -> False)
                $ p_ [class_ "text-sm text-textWeak"] "No result was retained. This action was not repeated. It may still complete."
              let required = MCP.minimumPermission operation.proposal.policy.capability
              if viewerPermission >= required
                then details_ [class_ "text-sm text-textWeak"] do
                  summary_ [class_ "cursor-pointer font-medium text-textStrong"] "Proposal and outcome"
                  pre_ [class_ "mt-2 max-h-64 overflow-auto whitespace-pre-wrap break-words text-xs"] $ toHtml $ preview operation.proposal
                  pre_ [class_ "mt-2 max-h-64 overflow-auto whitespace-pre-wrap break-words text-xs"] $ toHtml $ preview operation.outcome
                  when (operation.review /= Investigations.NoReview) $ pre_ [class_ "mt-2 max-h-64 overflow-auto whitespace-pre-wrap break-words text-xs"] $ toHtml $ preview operation.review
                else p_ [class_ "text-xs text-textWeak"] $ toHtml $ "Action details require " <> toText (encodeEnumSC @"P" required) <> " permission."
              when (run.status == Issues.RunRunning) $ case operation.outcome of
                Investigations.OperationUnstarted -> case operation.review of
                  Investigations.ReviewPending request -> do
                    p_ [class_ "text-xs text-textWeak"] do
                      "Review deadline: "
                      localTimeFmt_ "MMM d, HH:mm" request.expiresAt
                    when (viewerPermission >= max required (Catalog.approverPermission request.approver))
                      $ form_
                        [ hxPost_ $ urls.aiOperationReviewPost pid operation.id
                        , hxTarget_ "closest #routine-run"
                        , hxSelect_ "#routine-run"
                        , hxSwap_ "outerMorph"
                        , term "hx-disable" $ "#operation-" <> operation.id.toText <> " button"
                        , [__|on htmx:response:error or htmx:error
                          send errorToast(value: ['Could not confirm the decision. Refresh actions to check its status.']) to <body/>|]
                        , class_ "flex flex-wrap gap-2"
                        ]
                        do
                          button_ [type_ "submit", name_ "decision", value_ "approved", class_ "btn btn-sm btn-primary"] "Approve action"
                          button_ [type_ "submit", name_ "decision", value_ "rejected", class_ "btn btn-sm btn-ghost"] "Decline action"
                  (Investigations.NoReview; Investigations.ReviewResolved{}) -> pass
                (Investigations.OperationRunning; Investigations.OperationUnknown; Investigations.OperationReturned{}; Investigations.OperationRefused{}) -> pass
              time_ [datetime_ $ show operation.createdAt, class_ "text-xs text-textWeak"] $ localTimeFmt_ "MMM d, HH:mm" operation.createdAt
              when (operation.runId /= run.id)
                $ a_ [href_ $ runUrl pid operation.runId, class_ "ms-3 text-xs text-textBrand hover:underline"] "View original attempt"
          for_ (evidence.findings >>= AET.parseMaybe (AE.parseJSON @AI.AgenticChatResult)) $ \answer ->
            for_ (rightToMaybe $ AI.parseAgenticResponse answer) $ \response -> do
              let widgets = take 10 $ AI.responseWidgets response
              unless (null widgets) $ section_ [Aria.label_ "Run visualizations", class_ "space-y-3"] do
                h2_ [class_ "text-sm font-semibold text-textStrong"] "Visualizations"
                IssuePage.aiChatWidgets_ pid ("ai-run-" <> run.id.toText <> "-") widgets (Just answer.toolCalls)
          section_ [Aria.label_ "Run artifacts", class_ "space-y-4"] do
            h2_ [class_ "text-sm font-semibold text-textStrong"] "Artifacts"
            if null artifacts
              then p_ [class_ "text-sm text-textWeak"] $ if run.status == Issues.RunRunning then "Artifacts will appear when this run finishes." else "This run did not produce an artifact."
              else for_ artifacts \artifact -> article_ [class_ "border-t border-strokeWeak pt-4"] do
                h3_ [class_ "font-medium text-textStrong break-words"] $ toHtml artifact.title
                div_ [class_ "prose prose-sm mt-4 max-w-none break-words overflow-x-auto"] $ fst $ renderMarkdownTocWithPrefix ("ai-run-" <> run.id.toText <> "-" <> toText artifact.key <> "-") $ fromMaybe "" artifact.content
          for_ ([("Findings", evidence.findings), ("Tool results", evidence.actions)] :: [(Text, Maybe AE.Value)]) \(title, value) -> whenJust value $ disclosure_ [] (toHtml title) . pre_ [class_ "max-h-96 overflow-auto whitespace-pre-wrap break-words text-xs text-textWeak"] . toHtml @Text . decodeUtf8 . AEP.encodePretty
          when retained $ p_ [class_ "text-sm text-textWeak"] "This conversation was deleted. Its run and artifacts are retained here."
      where
        preview value =
          let text = decodeUtf8 @Text $ AE.encode value
           in if T.length text <= 8192 then text else T.take 4000 text <> "\n[… preview truncated; full evidence is retained …]\n" <> T.takeEnd 4000 text

    scheduleControl_ :: UUIDId "conversation" -> Maybe (Issues.ConversationSummary, Issues.RoutineInterval) -> Html ()
    scheduleControl_ conversationId routineState = details_ [class_ "relative group/routine"] do
      summary_ [class_ "btn btn-sm btn-ghost list-none gap-2 cursor-pointer"] do
        faSprite_ "clock" "regular" "h-3.5 w-3.5"
        span_ [class_ "max-md:hidden"] $ toHtml $ maybe "Schedule" (\(conversation, interval) -> bool "Paused" (Issues.routineCadence interval) conversation.routineActive) routineState
      div_ [class_ "absolute end-0 top-full z-30 mt-2 w-64 rounded-xl border border-strokeWeak bg-bgRaised p-3 shadow-lg"] do
        form_ [class_ "space-y-3", hxPost_ $ urls.aiRoutinePost pid conversationId, hxSwap_ "none"] do
          label_ [class_ "block text-xs font-medium text-textStrong"] "Run every"
          div_ [class_ "flex items-center gap-2"] do
            input_ [type_ "number", name_ "intervalMinutes", min_ "5", max_ "10080", value_ $ maybe "1440" (show . Issues.routineIntervalMinutes . snd) routineState, class_ "input input-sm min-w-0 grow text-base sm:text-xs", Aria.label_ "Routine interval in minutes"]
            span_ [class_ "text-xs text-textWeak"] "minutes"
          p_ [class_ "text-xs text-textWeak"] "New routines act automatically within your current role. Existing routine policies stay unchanged."
          button_ [type_ "submit", class_ "btn btn-sm btn-primary w-full active:scale-[0.96] transition-transform"] "Save routine"
        when (any ((.routineActive) . fst) routineState) $ form_ [class_ "mt-3 border-t border-strokeWeak pt-2", hxPost_ $ urls.aiRoutinePausePost pid conversationId, hxSwap_ "none"] $ button_ [type_ "submit", class_ "btn btn-sm btn-ghost w-full active:scale-[0.96] transition-transform"] "Pause routine"

    threadPage_ :: UUIDId "ai_routine_run" -> Bool -> Maybe Text -> [Issues.RoutineRun] -> Maybe AI.ReportDigest -> Maybe (Html ()) -> Maybe Issues.ConversationSummary -> Projects.ProjectId -> Maybe (UUIDId "conversation") -> Maybe (Issues.ConversationSummary, Issues.RoutineInterval) -> Bool -> [Issues.AIChatMessage] -> [Issues.AIArtifact] -> Maybe Issues.AIArtifact -> Html ()
    threadPage_ requestId canEdit instructions runs reportM runContent conversationM projectId activeConversationIdM routineM allowActionUi messages docs openDoc = div_ [id_ "ai-conversation", class_ "group/conversation relative h-full min-h-0 flex bg-bgBase"] do
      div_ [class_ "min-w-0 grow flex flex-col max-lg:group-has-[[data-open]]/conversation:hidden"] do
        div_ [id_ "ai-thread-messages", class_ "min-h-0 grow overflow-y-auto"] do
          div_ [class_ "max-w-3xl mx-auto px-5 py-6 space-y-8"] do
            for_ conversationM \conversation -> when (isJust conversation.routineKind) $ routineOverview_ conversation
            unless (null docs) documentsNav_
            div_
              ([id_ "ai-thread-results", class_ "space-y-8"] <> if any ((== Issues.RunRunning) . (.status)) runs then [hxGet_ threadHref, hxTrigger_ "every 2s", hxSelect_ "#ai-thread-results", term "hx-select-oob" "#pageTitleText:outerHTML", hxSwap_ "outerMorph"] else [])
              do
                unless (null runs) $ section_ [Aria.label_ "Routine runs"] do
                  let (recent, older) = splitAt 5 runs
                      ownRun run = Just run.conversationId == activeConversationIdM
                      runRow run = a_ ([href_ $ runUrl projectId run.id, class_ "grid grid-cols-[minmax(0,1fr)_auto] items-center gap-x-3 gap-y-0.5 px-3 py-2.5 hover:bg-fillHover focus-visible:outline-2 focus-visible:-outline-offset-2"] <> inspectorAttrs (urls.aiRoutineRunGet projectId run.id activeConversationIdM) <> [term "hx-select-oob" $ "#" <> runStatusId run <> ":outerHTML"]) do
                        span_ [class_ "flex min-w-0 items-baseline gap-2 text-sm text-textStrong"] do
                          time_ [datetime_ $ show run.startedAt, class_ "shrink-0 tabular-nums"] $ toHtml $ formatTime defaultTimeLocale "%e %b, %H:%M UTC" run.startedAt
                          unless (ownRun run) $ span_ [class_ "truncate text-textWeak"] $ toHtml $ fromMaybe "Run" run.title
                          when (run.attempt > 1) $ span_ [class_ "shrink-0 text-xs text-textWeak"] $ toHtml $ "retry " <> show @Text run.attempt
                        span_ [id_ $ runStatusId run, class_ "row-span-2"] $ uncurry tonePill_ $ runStatus run.status
                        whenJust (run.error <|> (summaryLine =<< run.summary)) \line -> span_ [class_ $ "truncate text-xs " <> maybe "text-textWeak" (const $ runErrorTone run.status) run.error, title_ line] $ toHtml line
                      runList = div_ [class_ "divide-y divide-strokeWeak overflow-hidden rounded-lg border border-strokeWeak bg-bgRaised"] . mapM_ runRow
                  h2_ [class_ "mb-2 text-sm font-semibold text-textStrong"] "Runs"
                  runList recent
                  unless (null older) $ details_ [class_ "group/older mt-2", term "hx-morph-skip" ""] do
                    summary_ [class_ "cursor-pointer list-none rounded-md px-1 py-1 text-xs font-medium text-textBrand hover:underline [&::-webkit-details-marker]:hidden"] do
                      span_ [class_ "group-open/older:hidden"] $ toHtml $ "Show " <> show @Text (length older) <> " older"
                      span_ [class_ "hidden group-open/older:inline"] "Show fewer"
                    div_ [class_ "mt-2"] $ runList older
                -- A routine reads as its latest result (plus the question that prompted it); older ones live under Runs.
                let isRoutine = any (isJust . (.routineKind)) conversationM
                    definingPrompt m = m.role == Issues.ChatUser && Just (T.strip m.content) == (T.strip <$> instructions)
                    (afterLatest, fromLatest) = break ((== Issues.ChatAssistant) . (.role)) $ reverse $ filter (not . definingPrompt) messages
                    asked = takeWhile ((== Issues.ChatUser) . (.role)) $ drop 1 fromLatest
                    shown = if isRoutine then reverse (afterLatest <> take 1 fromLatest <> asked) else messages
                    -- A report replaces its own result message; questions asked after it still show.
                    followUps digest = reverse $ takeWhile ((/= AI.digestMarkdown digest []) . (.content)) $ reverse messages
                case reportM of
                  Just digest -> do
                    reportView_ digest
                    unless (null $ followUps digest) $ IssuePage.aiChatHistoryView_ artifactLinkAttrs projectId (followUps digest)
                  Nothing
                    | hasVisibleHistory shown -> section_ [Aria.label_ "Results"] do
                        when isRoutine $ h2_ [class_ "mb-2 text-sm font-semibold text-textStrong"] "Latest result"
                        IssuePage.aiChatHistoryView_ artifactLinkAttrs projectId shown
                    | otherwise -> foldMap routineEmpty_ routineM
            when (any (isJust . (.routineKind)) conversationM) do
              -- Setup, not daily reading: collapsed below the results.
              disclosure_ [Aria.label_ "Routine configuration"] ("Settings" >> whenJust (conversationM >>= (.routinePolicy)) (span_ [class_ "ms-auto text-xs font-normal text-textWeak"] . toHtml . policyLabel))
                $ div_ [class_ "space-y-5"] do
                  for_ (conversationM >>= (.routinePolicy)) \policy -> do
                    let selection = case policy of Issues.ReviewSelected selected -> Just selected; (Issues.Observe; Issues.Autonomous) -> Nothing
                    if canEdit
                      then form_ [hxPost_ $ foldMap (urls.aiRoutinePolicyPost projectId) activeConversationIdM, hxSwap_ "none", class_ "group/policy space-y-2"] do
                        label_ [Lucid.for_ "routine-policy", class_ "block text-sm font-medium text-textStrong"] "Execution policy"
                        select_ [id_ "routine-policy", name_ "policy", class_ "select select-sm w-full text-base sm:text-xs"] do
                          option_ ([value_ "observe"] <> [selected_ "selected" | policy == Issues.Observe]) "Read-only"
                          option_ ([value_ "autonomous"] <> [selected_ "selected" | policy == Issues.Autonomous]) "Can make changes"
                          option_ ([value_ "review"] <> [selected_ "selected" | isJust selection]) "Review selected actions"
                        fieldset_ [class_ "hidden space-y-3 group-has-[option[value=review]:checked]/policy:block"] do
                          legend_ [class_ "text-sm font-medium text-textStrong"] "Actions requiring review"
                          for_ [minBound @Catalog.ReviewClass .. maxBound] \actionClass -> label_ [class_ "flex items-center gap-2 text-sm text-textStrong"] do
                            input_ $ [type_ "checkbox", name_ "reviewClass", value_ $ toText $ encodeEnumSC @"" actionClass, class_ "checkbox checkbox-sm"] <> [checked_ | any (elem actionClass . (.classes)) selection]
                            toHtml $ reviewClassLabel actionClass
                          label_ [class_ "block space-y-1 text-sm text-textStrong"] do
                            span_ "Required reviewer"
                            select_ [name_ "reviewApprover", class_ "select select-sm w-full text-base sm:text-xs"]
                              $ for_ @[] [Catalog.ReviewEditors, Catalog.ReviewAdministrators] \approver ->
                                option_ ([value_ $ toText $ encodeEnumSC @"Review" approver] <> [selected_ "selected" | approver == maybe Catalog.ReviewEditors (.approver) selection]) $ toHtml $ reviewerLabel approver
                          p_ [class_ "text-xs text-textWeak"] "Only checked action classes require review. Unchecked actions stay automatic. Choose at least one class."

                        p_ [class_ "text-sm text-textWeak"] "Read-only reads evidence and saves results. Can make changes also changes project resources, updates project knowledge, and publishes requested results within the authorizing member's current role, without per-action approval."
                        p_ [class_ "text-xs text-textWeak"] "Restrictions apply immediately. Greater authority applies to future runs."
                        button_ [type_ "submit", class_ "btn btn-sm btn-primary"] "Save policy"
                      else p_ [class_ "text-sm text-textWeak"] $ toHtml $ case policy of
                        (Issues.Observe; Issues.Autonomous) -> "Execution policy: " <> policyLabel policy
                        Issues.ReviewSelected selected -> "Review selected actions: " <> T.intercalate ", " (map reviewClassLabel $ toList selected.classes) <> ". Reviewers: " <> reviewerLabel selected.approver <> "."
                  form_ [hxPost_ $ foldMap (urls.aiRoutineInstructionsPost projectId) activeConversationIdM, hxSwap_ "none", class_ "space-y-2", Aria.label_ "Routine instructions"] do
                    label_ [Lucid.for_ "routine-instructions", class_ "block text-sm font-medium text-textStrong"] "Instructions"
                    textarea_ ([id_ "routine-instructions", name_ "instructions", required_ "required", maxlength_ "4000", rows_ "8", class_ "textarea w-full text-base sm:text-sm"] <> [readonly_ "" | not canEdit]) $ toHtml $ fromMaybe "" instructions
                    p_ [class_ "text-xs text-textWeak"] "Changes apply to future runs. Past runs keep the instructions they used."
                    when canEdit $ button_ [type_ "submit", class_ "btn btn-sm btn-primary"] "Save instructions"
              when (null runs && isNothing routineM) $ p_ [class_ "text-sm text-textWeak"] "No runs yet. An active routine starts when the next matching event arrives."
        unless (isJust runContent && isNothing conversationM) $ form_
          [ class_ "shrink-0 border-t border-strokeWeak bg-bgBase p-4"
          , hxPost_ $ maybe (urls.aiThreadsPost projectId) (urls.aiThreadPost projectId) activeConversationIdM
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
                , placeholder_ $ bool "Ask or delegate a task…" "Ask about these results…" (isJust routineM)
                , Aria.label_ "Message Monoscope AI"
                , class_ "min-h-9 max-h-40 grow resize-none bg-transparent border-0 px-2 py-2 text-base leading-6 placeholder:text-textWeak sm:text-sm sm:leading-5 focus:outline-none"
                , [__|on keydown[key=='Enter' and not shiftKey]
                      halt the event then call my form.requestSubmit()
                    end|]
                ]
                ""
              input_ [type_ "hidden", name_ "mode", value_ "chat"]
              input_ [type_ "hidden", name_ "requestId", value_ requestId.toText]
              button_ [type_ "submit", Aria.label_ "Send message", class_ "group/send w-9 h-9 shrink-0 rounded-lg bg-fillBrand-strong text-white flex items-center justify-center cursor-pointer active:scale-[0.96] transition-transform"] do
                span_ [id_ "ai-thread-loader", class_ "htmx-indicator"] $ faSprite_ "spinner" "regular" "w-3.5 h-3.5 animate-spin"
                span_ [class_ "inline-flex group-has-[.htmx-request]/send:hidden"] $ faSprite_ "arrow-up" "regular" "w-3.5 h-3.5"
            when allowActionUi $ label_ [class_ "max-w-3xl mx-auto mt-2 flex cursor-pointer items-center justify-center gap-2 text-xs text-textWeak"] do
              input_ [type_ "checkbox", name_ "allowActions", value_ "true", class_ "checkbox checkbox-xs"]
              span_ "Allow the project changes I request in this message"
            p_ [class_ "max-w-3xl mx-auto mt-2 text-xs text-textWeak text-center"] "AI can use your project data and connected integrations. Review actions before relying on them."
      aside_
        ( [ id_ "ai-inspector"
          , Aria.label_ "Conversation details"
          , class_ $ "min-w-0 border-s border-strokeWeak bg-bgRaised focus:outline-none " <> bool "hidden" "absolute inset-0 z-20 flex flex-col lg:static lg:z-auto lg:w-1/2 lg:max-w-2xl lg:shrink-0" (isJust runContent || isJust openDoc)
          ]
            <> [[__|init if #ai-thread-input call #ai-thread-input.focus() end end|] | isNothing runContent && isNothing openDoc]
            <> [attribute | isJust runContent || isJust openDoc, attribute <- [term "data-open" "", tabindex_ "-1", [__|init call me.focus() end on keydown[key == 'Escape'] halt the event then send click to #ai-inspector-close end|]]]
        )
        do
          when (isJust runContent || isJust openDoc) do
            header_ [class_ "flex shrink-0 items-center justify-between gap-3 border-b border-strokeWeak px-4 py-3"] do
              span_ [class_ "text-sm font-medium text-textWeak"] $ bool "Run details" "Document" (isJust openDoc)
              case activeConversationIdM of
                Just _ ->
                  a_ ([id_ "ai-inspector-close", href_ threadHref, Aria.label_ "Close details", title_ "Close (Esc)", class_ "btn btn-sm btn-square btn-ghost text-iconNeutral transition-[color,background-color,scale] hover:bg-fillWeak hover:text-iconBrand active:scale-[0.96] max-md:min-h-11 max-md:min-w-11"] <> inspectorAttrs threadHref)
                    $ faSprite_ "xmark" "regular" "size-3.5"
                Nothing -> a_ ([id_ "ai-inspector-close", href_ $ urls.aiRoutinesGet projectId, class_ "btn btn-sm btn-ghost gap-2"] <> navTabAttrs) do
                  faSprite_ "arrow-left" "regular" "size-3.5"
                  span_ "All routines"
            div_ [class_ "min-h-0 grow overflow-y-auto px-5 py-5"] $ maybe (foldMap documentView_ openDoc) identity runContent
      where
        policyLabel = \case
          Issues.Observe -> "Read-only" :: Text
          Issues.Autonomous -> "Can make changes"
          Issues.ReviewSelected _ -> "Changes need review"
        reviewClassLabel = \case
          Catalog.KnowledgeChanges -> "Project knowledge"
          Catalog.ProjectChanges -> "Project resources"
          Catalog.ExternalPosts -> "External publication"
          Catalog.AdministrativeChanges -> "Administrative changes"
        reviewerLabel = \case
          Catalog.ReviewEditors -> "Editors or administrators"
          Catalog.ReviewAdministrators -> "Administrators"
        threadHref = foldMap (threadUrl projectId) activeConversationIdM
        docHref doc = foldMap (\cid -> urls.aiThreadGet projectId cid (Just doc.key)) activeConversationIdM
        (openBody, openToc) = maybe (mempty, []) (renderMarkdownTocWithPrefix $ "ai-document-" <> convId.toText <> "-") $ openDoc >>= (.content)
        inspectorAttrs href =
          [ hxGet_ href
          , hxTarget_ "#ai-inspector"
          , hxSelect_ "#ai-inspector"
          , hxSwap_ "outerHTML"
          , [__|on htmx:response:error or htmx:error
                  send errorToast(value: ['Could not load the details. Try again.']) to <body/>|]
          ]
        artifactLinkAttrs href = let url = threadHref <> href in href_ url : inspectorAttrs url
        navLink_ href = a_ ([href_ href, class_ "flex max-w-full items-center gap-2 rounded-md px-2 py-1.5 text-sm text-textWeak hover:bg-fillHover hover:text-textStrong"] <> inspectorAttrs href)

        documentsNav_ :: Html ()
        documentsNav_ = nav_ [Aria.label_ "Documents"] do
          h2_ [class_ "mb-2 text-sm font-semibold text-textStrong"] "Documents"
          let (recent, older) = splitAt 3 docs
              docList =
                div_ [class_ "-mx-2 grid grid-cols-1 gap-0.5"] . mapM_ \doc -> navLink_ (docHref doc) do
                  faSprite_ "file-lines" "regular" "h-3.5 w-3.5 shrink-0 text-iconBrand"
                  span_ [class_ "min-w-0 grow truncate"] $ toHtml doc.title
                  time_ [datetime_ $ show doc.updatedAt, class_ "shrink-0 text-xs tabular-nums text-textWeak"] $ toHtml $ formatTime defaultTimeLocale "%e %b" doc.updatedAt
          docList recent
          unless (null older) $ details_ [class_ "group/olderdocs mt-1"] do
            summary_ [class_ "cursor-pointer list-none rounded-md px-1 py-1 text-xs font-medium text-textBrand hover:underline [&::-webkit-details-marker]:hidden"] do
              span_ [class_ "group-open/olderdocs:hidden"] $ toHtml $ "Show all " <> show @Text (length docs)
              span_ [class_ "hidden group-open/olderdocs:inline"] "Show fewer"
            div_ [class_ "mt-1"] $ docList older

        documentView_ :: Issues.AIArtifact -> Html ()
        documentView_ doc =
          article_
            ( [id_ "ai-document", Aria.label_ doc.title]
                -- Routines keep appending to today's document; keep an open one current.
                <> bool [] [hxGet_ $ docHref doc, hxTrigger_ "every 60s", hxSelect_ "#ai-document", hxTarget_ "this", hxSwap_ "outerMorph"] (any ((.routineActive) . fst) routineM)
            )
            do
              h2_ [class_ "text-base font-semibold text-textStrong text-balance break-words"] $ toHtml doc.title
              p_ [class_ "mt-1 text-xs text-textWeak"] $ "Updated " >> localTimeFmt_ "MMM d, HH:mm" doc.updatedAt
              when (length openToc > 1) $ details_ [class_ "mt-4 text-sm text-textWeak"] do
                summary_ [class_ "cursor-pointer"] "On this page"
                ul_ [class_ "mt-2 space-y-1"] $ for_ openToc \(anchor, heading) ->
                  li_ $ a_ [href_ $ "#" <> anchor, class_ "block truncate text-textBrand hover:underline"] $ toHtml heading
              div_ [class_ "prose prose-sm mt-6 max-w-none break-words overflow-x-auto [&_h2]:scroll-mt-4"] openBody

        hasVisibleHistory :: [Issues.AIChatMessage] -> Bool
        hasVisibleHistory = any ((`elem` [Issues.ChatAssistant, Issues.ChatExecutionEvent]) . (.role))

        routineOverview_ :: Issues.ConversationSummary -> Html ()
        routineOverview_ conversation = section_ [class_ "space-y-2", Aria.label_ "Routine status"] do
          div_ [class_ "flex flex-wrap items-start justify-between gap-3"] do
            routineFacts_ conversation
            when (canEdit && isJust conversation.routineInterval) $ div_ [class_ "flex shrink-0 items-center gap-1"] do
              let button url label primary = form_ [hxPost_ url, hxSwap_ "none", term "hx-disable" "find button"] $ button_ [type_ "submit", class_ $ "btn btn-xs active:scale-[0.96] transition-transform " <> bool "btn-ghost" "btn-primary" primary] label
              if conversation.routineActive
                then do
                  unless (isJust conversation.routineRunningSince) $ button (urls.aiRoutineResumePost projectId conversation.conversationId True) (faSprite_ "play" "solid" "h-3 w-3" >> "Run now") True
                  button (urls.aiRoutinePausePost projectId conversation.conversationId) "Pause" False
                else button (urls.aiRoutineResumePost projectId conversation.conversationId False) "Resume" True
          -- An unedited catalog routine reads as its one-line ask; the full prompt stays under Settings.
          let template = find ((== conversation.templateKey) . Just . (.key)) Issues.routineTemplates
          whenJust (maybe instructions (\t -> if instructions == Just t.prompt then Just t.ask else instructions) template) $ p_ [class_ "line-clamp-2 text-sm text-textStrong text-pretty"] . toHtml

        routineFacts_ :: Issues.ConversationSummary -> Html ()
        routineFacts_ conversation =
          div_ [class_ "flex min-w-0 flex-wrap items-center gap-x-3 gap-y-1 text-sm text-textWeak sm:gap-x-2 sm:[&>*+*]:before:me-2 sm:[&>*+*]:before:text-textDisabled sm:[&>*+*]:before:content-['·']"] do
            span_ $ uncurry tonePill_ $ routineStatus conversation
            span_ $ toHtml $ maybe (foldMap Issues.routineKindLabel conversation.routineKind) Issues.routineCadence conversation.routineInterval
            span_ [class_ "tabular-nums"] case (conversation.routineLastRunAt, conversation.routineLastRunStatus) of
              (Just at, Just status) -> toHtml $ toText (formatTime defaultTimeLocale "Last run %-d %b, %H:%M UTC" at) <> " — " <> snd (runStatus status)
              _ -> "Not run yet"
            whenJust (conversation.routineNextRunAt <* guard conversation.routineActive) $ span_ [class_ "tabular-nums"] . toHtml . formatTime defaultTimeLocale "Next run %-d %b, %H:%M UTC"
            whenJust conversation.routinePolicy \policy -> span_ [title_ "Execution policy"] $ toHtml $ policyLabel policy
            whenJust (mfilter (/= Issues.DestinationConversation) conversation.routineDestination) $ span_ . toHtml . destinationLabel

        reportView_ :: AI.ReportDigest -> Html ()
        reportView_ digest = section_ [Aria.label_ "Latest report", class_ "space-y-6"] do
          div_ do
            h2_ [class_ "mb-1 text-xs font-medium text-textWeak"] "Latest report"
            p_ [class_ "text-lg font-semibold text-textStrong text-pretty"] $ toHtml digest.headline
          forM_ (zip [0 :: Int ..] digest.highlights) \(i, h) -> article_ [class_ "space-y-2 border-t border-strokeWeak pt-5"] do
            h3_ [class_ "text-sm font-semibold text-textStrong"] $ toHtml h.title
            unless (T.null h.detail) $ p_ [class_ "text-sm text-textStrong text-pretty"] $ toHtml h.detail
            whenJust h.widget \w -> IssuePage.aiChatWidgets_ projectId ("ai-report-" <> show i <> "-") [w] Nothing
            whenJust h.link \l -> a_ [href_ l, class_ "inline-flex items-center gap-1 text-sm text-textBrand hover:underline"] $ "Explore" >> faSprite_ "arrow-right" "regular" "h-3 w-3 rtl:-scale-x-100"

        routineEmpty_ :: (Issues.ConversationSummary, Issues.RoutineInterval) -> Html ()
        routineEmpty_ (conversation, _) =
          emptyState_ def{icon = Just "clock"} "No completed run yet"
            $ if conversation.routineActive
              then "This routine is scheduled. Its first result will appear here after the next run."
              else "This routine is paused. Resume its schedule to produce a new result."


routineInstallPostH :: Projects.ProjectId -> RoutineTemplateForm -> ATAuthCtx (ActionResult UnknownTemplate AIPage)
routineInstallPostH pid form = act do
  (_, project, _) <- mkAIPageCtx pid
  (sess, _) <- ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit pid
  forM (maybeToRight UnknownTemplate $ find ((== form.templateKey) . (.key)) Issues.routineTemplates) \template ->
    ThreadPage pid <$> do
      Issues.ensureRoutineTemplate sess.user.id pid project.timeZone template >>= \case
        Left convId -> convId <$ authorizeConversation pid convId
        Right (convId, scheduled) -> convId <$ enqueue (pure scheduled)


startThreadPostHWithTools :: Maybe AI.ProjectTools -> Projects.ProjectId -> AIChatForm -> ATAuthCtx (ActionResult NewThreadError AIPage)
startThreadPostHWithTools projectTools pid form = act do
  authorizeProject pid
  forM newThread \request -> do
    origin <- case request of
      NewChat _ -> pure Issues.WebConversation
      NewRoutine _ interval -> Issues.WebRoutine interval <$ ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit pid
    requestId <- maybe (UUIDId <$> UUID.genUUID) pure form.requestId
    let convId = UUIDId $ UUID5.generateNamed requestId.unUUIDId $ BS.unpack "web-conversation"
    ThreadPage pid convId <$ submitTurn projectTools origin requestId (form.allowActions == Just True) pid convId (case request of NewChat p -> p; NewRoutine p _ -> p)
  where
    newThread = do
      prompt <- first NewThreadPrompt $ normalizeQuery form.query
      case (form.mode, form.intervalMinutes) of
        (ModeChat, Nothing) -> Right $ NewChat prompt
        (ModeChat, Just _) -> Left ChatWithInterval
        (ModeRoutine, Nothing) -> Left RoutineWithoutInterval
        (ModeRoutine, Just interval) -> Right $ NewRoutine prompt interval


threadPostHWithTools :: Maybe AI.ProjectTools -> Projects.ProjectId -> UUIDId "conversation" -> AIChatForm -> ATAuthCtx (ActionResult TurnError AIPage)
threadPostHWithTools projectTools pid convId form = act do
  authorizeConversation pid convId
  authorizeProject pid
  case normalizeQuery form.query of
    Left err -> pure $ Left $ TurnPrompt err
    Right prompt ->
      do
        requestId <- maybe (UUIDId <$> UUID.genUUID) pure form.requestId
        Issues.getConversation pid convId >>= traverse \conversation -> do
          case Issues.conversationIssueId conversation of
            Just issueId -> void $ IssuePage.aiChatPostHWithTools projectTools pid issueId (IssuePage.AIChatForm prompt $ Just requestId)
            Nothing -> submitTurn projectTools Issues.WebMessage requestId (form.allowActions == Just True) pid convId prompt
          pure $ ThreadPage pid convId
        <&> maybeToRight ConversationGone


threadTitlePostH :: Projects.ProjectId -> UUIDId "conversation" -> TitleForm -> ATAuthCtx (ActionResult EmptyTitle AIPage)
threadTitlePostH pid convId form = act do
  authorizeConversation pid convId
  authorizeProject pid
  let title = T.strip form.conversationTitle
  if T.null title
    then pure $ Left EmptyTitle
    else Right (ThreadPage pid convId) <$ Issues.renameConversation pid convId title


threadDeleteH :: Projects.ProjectId -> UUIDId "conversation" -> ATAuthCtx (RespHeaders (Done AIPage))
threadDeleteH pid convId = authorizeConversation pid convId >> authorizeProject pid >> Issues.deleteConversation pid convId >> done (ThreadsPage pid)


submitTurn :: Maybe AI.ProjectTools -> Issues.InteractiveOrigin -> UUIDId "ai_routine_run" -> Bool -> Projects.ProjectId -> UUIDId "conversation" -> Text -> ATAuthCtx ()
submitTurn projectTools origin requestId allowActions pid convId prompt = do
  appCtx <- ask @AuthContext
  (sess, _) <- Projects.sessionAndProject pid
  let policy = if allowActions then Issues.Autonomous else Issues.Observe
  retained <- Issues.getRoutineRun pid requestId
  member <- ProjectMembers.getUserPermission pid sess.user.id
  let browserOnly = case (.source) <$> retained of
        Just Issues.InteractiveRun{} -> True
        (Nothing; Just Issues.ScheduledRun; Just Issues.TriggeredRun{}; Just Issues.ConversationRun{}; Just Issues.FixRun{}) -> isNothing member
      admit = if browserOnly then Issues.startInteractiveRun else Jobs.startConversation
  admit pid convId sess.user.id requestId prompt policy origin >>= \case
    Issues.ConflictingRequest -> throwError Servant.err409{Servant.errBody = "Request ID already belongs to another request"}
    Issues.AcceptedRequest scheduled -> when browserOnly $ for_ scheduled $ \routine -> BackgroundJobs.enqueueAIRoutine appCtx routine.id routine.scheduledAt
  when browserOnly do
    mode <- fmap AI.DurableRun $ AI.requireRunContext pid $ Issues.InteractiveClaim sess.user.id convId requestId
    result <- Bots.processAIQueryWithMode (Just appCtx.config) appCtx.env.enableTimefusionReads projectTools mode (AI.MemberAccess AI.WebSession sess.user.id) pid prompt (Just convId) appCtx.config.openaiModel appCtx.config.openaiApiKey
    case result of
      Left err -> throwError Servant.err502{Servant.errBody = encodeUtf8 err}
      Right _ -> pass


normalizeQuery :: Text -> Either PromptError Text
normalizeQuery = (\prompt -> if T.null prompt then Left EmptyPrompt else if T.length prompt > 4000 then Left PromptTooLong else Right prompt) . T.strip


authorizeProject :: Projects.ProjectId -> ATAuthCtx ()
authorizeProject pid = do
  (sess, _) <- Projects.sessionAndProject pid
  unlessM (isJust <$> ProjectMembers.getUserPermission pid sess.user.id) $ throwError Servant.err403


-- | Schedule the routine's next run, if the write produced one.
enqueue :: ATAuthCtx (Maybe Issues.ScheduledRoutine) -> ATAuthCtx ()
enqueue write = do
  appCtx <- ask @AuthContext
  whenJustM write \routine -> BackgroundJobs.enqueueAIRoutine appCtx routine.id routine.scheduledAt


routinePostH :: Projects.ProjectId -> UUIDId "conversation" -> RoutineForm -> ATAuthCtx (RespHeaders (Done AIPage))
routinePostH pid convId form = do
  authorizeConversation pid convId
  void (ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit pid)
  (sess, _) <- Projects.sessionAndProject pid
  whenJustM (Issues.getConversation pid convId) \conversation ->
    when (conversation.conversationType == Issues.CTWeb) $ enqueue (Issues.upsertRoutine sess.user.id pid convId form.intervalMinutes)
  done $ ThreadPage pid convId


routinePausePostH :: Projects.ProjectId -> UUIDId "conversation" -> ATAuthCtx (RespHeaders (Done AIPage))
routinePausePostH pid convId = authorizeConversation pid convId >> void (ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit pid) >> Issues.pauseRoutine pid convId >> Jobs.wakeStoppedReviews (Issues.StopRoutine pid convId) >> done (ThreadPage pid convId)


routineCancelPostH, routineDeleteH :: Projects.ProjectId -> UUIDId "conversation" -> ATAuthCtx (RespHeaders (Done AIPage))
routineCancelPostH pid convId = authorizeConversation pid convId >> void (ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit pid) >> Issues.cancelRoutineRun pid convId >> Jobs.wakeStoppedReviews (Issues.StopRoutine pid convId) >> done (RoutinesPage pid)


-- | Resume a paused routine on its schedule, or (@now@) start an active one immediately.
routineResumePostH :: Projects.ProjectId -> UUIDId "conversation" -> Bool -> ATAuthCtx (RespHeaders (Done AIPage))
routineResumePostH pid convId now = do
  authorizeConversation pid convId
  void (ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit pid)
  (sess, _) <- Projects.sessionAndProject pid
  enqueue (Issues.resumeRoutine (bool Issues.ResumeSchedule Issues.RunNow now) sess.user.id pid convId)
  done $ bool (RoutinesPage pid) (ThreadPage pid convId) now


routineDeleteH pid convId = authorizeConversation pid convId >> void (ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit pid) >> Issues.pauseRoutine pid convId >> Jobs.wakeStoppedReviews (Issues.StopRoutine pid convId) >> Issues.deleteRoutine pid convId >> done (RoutinesPage pid)


routineDestinationPostH :: Projects.ProjectId -> UUIDId "conversation" -> RoutineDestinationForm -> ATAuthCtx (RespHeaders (Done AIPage))
routineDestinationPostH pid convId form = do
  authorizeConversation pid convId
  void (ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit pid)
  Issues.setRoutineDestination pid convId form.destination
  done (RoutinesPage pid)


routinePolicyPostH :: Projects.ProjectId -> UUIDId "conversation" -> RoutinePolicyForm -> ATAuthCtx (ActionResult NoReviewClasses PolicySaved)
routinePolicyPostH pid conversation form = act do
  authorizeConversation pid conversation
  (session, _) <- ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit pid
  case form of
    MissingReviewClasses -> pure $ Left NoReviewClasses
    RoutinePolicyForm policy -> Right PolicySaved <$ Issues.setRoutinePolicy session.user.id pid conversation policy


data PolicySaved = PolicySaved
  deriving stock (Eq, Generic, Show)
  deriving (Outcome) via Says "Execution policy saved." PolicySaved


operationGetH :: Projects.ProjectId -> UUIDId "agent_operation" -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
operationGetH pid identifier = do
  run <- Investigations.getOperationRun pid identifier >>= maybe (throwError Servant.err404) pure
  routineRunGetH pid run Nothing


-- | The operation page re-rendered after a review decision; 'ReviewUnavailable' never reaches it (403).
data OperationReviewed = OperationReviewed {response :: Investigations.ReviewResponse, page :: Html ()}


instance ToHtml OperationReviewed where
  toHtml = toHtml . (.page)
  toHtmlRaw = toHtml


instance Outcome OperationReviewed where
  outcomeBody = toHtml
  outcomeToast r = case r.response of
    Investigations.ReviewSaved{} -> Just "Decision saved."
    Investigations.ReviewAlreadyDecided{} -> Just "This action already has a decision."
    Investigations.ReviewExpired -> Nothing
    Investigations.ReviewUnavailable -> Nothing


operationReviewPostH :: Projects.ProjectId -> UUIDId "agent_operation" -> OperationReviewForm -> ATAuthCtx (RespHeaders (Done OperationReviewed))
operationReviewPostH pid identifier form = do
  (session, _) <- ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit pid
  response <- Jobs.reviewOperation pid session.user.id identifier form.decision
  when (response == Investigations.ReviewUnavailable) $ throwError Servant.err403
  when (response == Investigations.ReviewExpired) $ addErrorToast "This action's review deadline has passed." Nothing
  done . OperationReviewed response . (.content) . Servant.getResponse =<< operationGetH pid identifier


-- | Bordered disclosure with a turning chevron, shared by routine settings and run evidence.
disclosure_ :: [Attribute] -> Html () -> Html () -> Html ()
disclosure_ attrs summary body = details_ ([class_ " group/disclosure rounded-xl border border-strokeWeak bg-bgRaised"] <> attrs) do
  summary_ [class_ "flex cursor-pointer list-none items-center gap-2 rounded-xl px-4 py-3 text-sm font-medium text-textStrong hover:bg-fillHover focus-visible:outline-2 focus-visible:-outline-offset-2 [&::-webkit-details-marker]:hidden"] do
    faSprite_ "chevron-right" "regular" "h-3 w-3 shrink-0 text-iconNeutral transition-transform duration-150 group-open/disclosure:rotate-90 rtl:-scale-x-100"
    summary
  div_ [class_ "border-t border-strokeWeak p-4"] body


routineStatus :: Issues.ConversationSummary -> (BadgeColor, Text)
routineStatus conversation
  | isJust conversation.routineRunningSince = (BrandBadge, "Running now")
  | conversation.routineActive = (SuccessBadge, "Active")
  | otherwise = (NeutralBadge, "Paused")


destinationLabel :: Issues.RoutineDestination -> Text
destinationLabel = \case
  Issues.DestinationConversation -> "In app only"
  Issues.DestinationEmail -> "Email to members"
  Issues.DestinationSlack -> "Post to Slack"


runErrorTone :: Issues.RoutineRunStatus -> Text
runErrorTone = \case
  Issues.RunFailed -> "text-textError"
  Issues.RunTimedOut -> "text-textWarning"
  (Issues.RunRunning; Issues.RunSucceeded; Issues.RunNoFindings; Issues.RunCancelled) -> "text-textWeak"


-- | First prose line of a markdown result, for one-line history rows.
--
-- >>> summaryLine "## Weekly report\n\n- **3 new issues** since Monday"
-- Just "Weekly report"
-- >>> summaryLine "- **3 new issues** since Monday"
-- Just "3 new issues since Monday"
-- >>> summaryLine "\n  \n"
-- Nothing
summaryLine :: Text -> Maybe Text
summaryLine = find (not . T.null) . map (T.strip . T.replace "**" "" . T.dropWhile (`elem` ("#*->| " :: String))) . lines


runStatus :: Issues.RoutineRunStatus -> (BadgeColor, Text)
runStatus = \case
  Issues.RunRunning -> (BrandBadge, "Running")
  Issues.RunSucceeded -> (SuccessBadge, "Succeeded")
  Issues.RunNoFindings -> (NeutralBadge, "No findings")
  Issues.RunFailed -> (ErrorBadge, "Failed")
  Issues.RunTimedOut -> (WarningBadge, "Timed out")
  Issues.RunCancelled -> (NeutralBadge, "Cancelled")


routineRunGetH :: Projects.ProjectId -> UUIDId "ai_routine_run" -> Maybe (UUIDId "conversation") -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
routineRunGetH pid runId originM = do
  run <- Issues.getRoutineRun pid runId >>= maybe (throwError Servant.err404) pure
  threadPageGetH pid (fromMaybe run.conversationId originM) Nothing (Just run) Nothing


routineInstructionsPostH :: Projects.ProjectId -> UUIDId "conversation" -> RoutineInstructionsForm -> ATAuthCtx (ActionResult PromptError AIPage)
routineInstructionsPostH pid convId form = act do
  authorizeConversation pid convId
  void $ ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit pid
  forM (normalizeQuery form.instructions) \instructions -> ThreadPage pid convId <$ Issues.setRoutineInstructions pid convId instructions


canEditRoutines :: Projects.ProjectId -> ATAuthCtx Bool
canEditRoutines pid = do
  (sess, _) <- Projects.sessionAndProject pid
  maybe False (>= ProjectMembers.PEdit) <$> ProjectMembers.getUserPermission pid sess.user.id


currentViewerPermission :: Projects.ProjectId -> ATAuthCtx ProjectMembers.Permissions
currentViewerPermission pid = do
  (session, _) <- Projects.sessionAndProject pid
  fromMaybe ProjectMembers.PView <$> ProjectMembers.getSessionPermission pid session.user.id


visibleConversations :: Projects.ProjectId -> ATAuthCtx [Issues.ConversationSummary]
visibleConversations pid = do
  (session, _) <- Projects.sessionAndProject pid
  role <- currentViewerPermission pid
  Issues.listVisibleConversations pid role (Investigations.MemberViewer session.user.id)


authorizeConversation :: Projects.ProjectId -> UUIDId "conversation" -> ATAuthCtx ()
authorizeConversation pid conversation = do
  (session, _) <- Projects.sessionAndProject pid
  unlessM (AI.canReadEvidence (AI.MemberAccess AI.WebSession session.user.id) pid conversation) $ throwError Servant.err403


threadUrl :: Projects.ProjectId -> UUIDId "conversation" -> Text
threadUrl pid conv = urls.aiThreadGet pid conv Nothing


runUrl :: Projects.ProjectId -> UUIDId "ai_routine_run" -> Text
runUrl pid run = urls.aiRoutineRunGet pid run Nothing
