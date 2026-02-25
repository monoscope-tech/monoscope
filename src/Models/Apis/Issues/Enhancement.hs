module Models.Apis.Issues.Enhancement (
  enhanceIssueWithLLM,
  generateEnhancedTitle,
  generateEnhancedDescription,
  classifyIssueCriticality,
  updateIssueClassification,
  IssueEnhancement (..),
) where

import Data.Aeson qualified as AE
import Data.Effectful.LLM qualified as ELLM
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.Newtypes (getAeson)
import Effectful (Eff, (:>))
import Models.Apis.Issues qualified as Issues
import NeatInterpolation (text)
import Pkg.AI qualified as AI
import PyF (fmtTrim)
import Relude hiding (id)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (DB)


data IssueEnhancement = IssueEnhancement
  { issueId :: Issues.IssueId
  , enhancedTitle :: Text
  , enhancedDescription :: Text
  , recommendedAction :: Text
  , migrationComplexity :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Enhance a single issue with LLM-generated title and description
enhanceIssueWithLLM :: ELLM.LLM :> es => AuthContext -> Issues.Issue -> Eff es (Either Text IssueEnhancement)
enhanceIssueWithLLM authCtx issue = runExceptT do
  enhancedTitle <- ExceptT $ generateEnhancedTitle authCtx issue
  (enhancedDescription, recommendedAction, migrationComplexity) <- ExceptT $ generateEnhancedDescription authCtx issue
  pure IssueEnhancement{issueId = issue.id, enhancedTitle, enhancedDescription, recommendedAction, migrationComplexity}


-- | Generate an enhanced title using LLM
generateEnhancedTitle :: ELLM.LLM :> es => AuthContext -> Issues.Issue -> Eff es (Either Text Text)
generateEnhancedTitle authCtx issue = runExceptT do
  r <- ExceptT $ AI.callOpenAIAPIEff (buildTitlePrompt issue) authCtx.config.openaiApiKey
  (title, _) <- hoistEither $ AI.getNormalTupleReponse r
  pure $ T.take 200 title


-- | Generate enhanced description with recommended actions
generateEnhancedDescription :: ELLM.LLM :> es => AuthContext -> Issues.Issue -> Eff es (Either Text (Text, Text, Text))
generateEnhancedDescription authCtx issue = runExceptT do
  r <- ExceptT $ AI.callOpenAIAPIEff (buildDescriptionPrompt issue) authCtx.config.openaiApiKey
  (response, _) <- hoistEither $ AI.getNormalTupleReponse r
  let lines' = lines response
  pure (fromMaybe "" $ viaNonEmpty head lines', fromMaybe "Review the changes and update your integration accordingly." $ lines' !!? 1, fromMaybe "medium" $ lines' !!? 2)


withIssueData :: AE.FromJSON a => Issues.Issue -> (a -> Text) -> Text -> Text
withIssueData issue f fallback = case AE.fromJSON (getAeson issue.issueData) of
  AE.Success d -> f d
  _ -> fallback


-- | Build prompt for title generation
buildTitlePrompt :: Issues.Issue -> Text
buildTitlePrompt issue =
  let baseContext = case issue.issueType of
        Issues.ApiChange ->
          withIssueData @Issues.APIChangeData
            issue
            ( \d ->
                [fmtTrim|Generate a concise, descriptive title for this API change.
            Endpoint: {d.endpointMethod} {d.endpointPath}
            New fields: {V.length d.newFields}
            Deleted fields: {V.length d.deletedFields}
            Modified fields: {V.length d.modifiedFields}
            Service: {Issues.serviceLabel issue.service}|]
            )
            "Generate a concise title for this API change."
        Issues.RuntimeException ->
          withIssueData @Issues.RuntimeExceptionData
            issue
            ( \d ->
                [fmtTrim|Generate a concise title for this runtime exception.
            Error type: {d.errorType}
            Error message: {T.take 100 d.errorMessage}
            Service: {Issues.serviceLabel issue.service}|]
            )
            "Generate a concise title for this runtime exception."
        Issues.QueryAlert ->
          withIssueData @Issues.QueryAlertData
            issue
            ( \d ->
                [fmtTrim|Generate a concise title for this query alert.
            Query: {d.queryName}
            Threshold: {d.thresholdValue} ({d.thresholdType})
            Actual value: {d.actualValue}|]
            )
            "Generate a concise title for this query alert."
        Issues.LogPattern ->
          withIssueData @Issues.LogPatternData
            issue
            ( \d ->
                [fmtTrim|Generate a concise title for this new log pattern detection.
            Log pattern: {d.logPattern}
            Sample message: {fromMaybe "N/A" d.sampleMessage}
            Log level: {fromMaybe "unknown" d.logLevel}
            Service: {Issues.serviceLabel d.serviceName}
            Occurrences: {d.occurrenceCount}|]
            )
            ("Generate a concise title for this log pattern. Title: " <> issue.title)
        Issues.LogPatternRateChange ->
          withIssueData @Issues.LogPatternRateChangeData
            issue
            ( \d ->
                let dir = display d.changeDirection
                 in [fmtTrim|Generate a concise title for this log pattern volume {dir}.
            Log pattern: {d.logPattern}
            Current rate: {Issues.showRate d.currentRatePerHour}
            Baseline: {Issues.showRate d.baselineMean}
            Change: {Issues.showPct d.changePercent}
            Service: {Issues.serviceLabel d.serviceName}|]
            )
            ("Generate a concise title for this log pattern rate change. Title: " <> issue.title)

      systemPrompt =
        unlines
          [ "You are an API monitoring assistant. Generate clear, actionable titles for API issues."
          , "Keep titles under 80 characters."
          , "Focus on the impact and what changed."
          , "Use present tense and active voice."
          , "Examples:"
          , "- 'New User Authentication Endpoint Added to Auth Service'"
          , "- 'Breaking Change: 5 Required Fields Removed from Order Response'"
          , "- 'Payment Service Schema Updated with 3 New Optional Fields'"
          , "- 'Critical: NullPointerException in Cart Service Checkout Flow'"
          ]
   in systemPrompt <> "\n\n" <> baseContext


-- | Build prompt for description generation
buildDescriptionPrompt :: Issues.Issue -> Text
buildDescriptionPrompt issue =
  let baseContext = case issue.issueType of
        Issues.ApiChange ->
          withIssueData @Issues.APIChangeData
            issue
            ( \d ->
                [fmtTrim|Describe this API change and its impact.
            Endpoint: {d.endpointMethod} {d.endpointPath}
            New fields: {show $ V.toList d.newFields}
            Deleted fields: {show $ V.toList d.deletedFields}
            Modified fields: {show $ V.toList d.modifiedFields}
            Total anomalies grouped: {V.length d.anomalyHashes}
            Service: {Issues.serviceLabel issue.service}|]
            )
            "Describe this API change and its implications."
        Issues.RuntimeException ->
          withIssueData @Issues.RuntimeExceptionData
            issue
            ( \d ->
                [fmtTrim|Analyze this runtime exception and provide debugging guidance.
            Error type: {d.errorType}
            Error message: {d.errorMessage}
            Stack trace: {T.take 500 d.stackTrace}
            Request context: {fromMaybe "Unknown" d.requestMethod} {fromMaybe "Unknown" d.requestPath}
            Occurrences: {d.occurrenceCount}|]
            )
            "Analyze this runtime exception."
        Issues.QueryAlert ->
          withIssueData @Issues.QueryAlertData
            issue
            ( \d ->
                [fmtTrim|Describe this query alert and recommended actions.
            Query: {d.queryName}
            Expression: {d.queryExpression}
            Threshold: {d.thresholdValue} ({d.thresholdType})
            Actual value: {d.actualValue}
            Triggered at: {show d.triggeredAt}|]
            )
            "Describe this query alert."
        Issues.LogPattern ->
          withIssueData @Issues.LogPatternData
            issue
            ( \d ->
                [fmtTrim|Describe this new log pattern and its implications.
            Log pattern: {d.logPattern}
            Sample message: {fromMaybe "N/A" d.sampleMessage}
            Log level: {fromMaybe "unknown" d.logLevel}
            Service: {Issues.serviceLabel d.serviceName}
            Source: {d.sourceField}
            Occurrences: {d.occurrenceCount}
            First seen: {show d.firstSeenAt}|]
            )
            ("Describe this log pattern issue. Title: " <> issue.title)
        Issues.LogPatternRateChange ->
          withIssueData @Issues.LogPatternRateChangeData
            issue
            ( \d ->
                let dir = display d.changeDirection
                 in [fmtTrim|Describe this log pattern volume {dir} and its implications.
            Log pattern: {d.logPattern}
            Sample message: {fromMaybe "N/A" d.sampleMessage}
            Current rate: {Issues.showRate d.currentRatePerHour}
            Baseline mean: {Issues.showRate d.baselineMean}
            Baseline MAD: {Issues.showRate d.baselineMad}
            Z-score: {show (round d.zScore :: Int)} standard deviations
            Change: {Issues.showPct d.changePercent}
            Service: {Issues.serviceLabel d.serviceName}
            Log level: {fromMaybe "unknown" d.logLevel}|]
            )
            ("Describe this log pattern rate change. Title: " <> issue.title)

      systemPrompt =
        [text|
          You are an API monitoring assistant. Generate detailed descriptions for API issues.
          Structure your response in exactly 3 lines:
          Line 1: A clear description of what changed and why it matters (1-2 sentences)
          Line 2: Recommended action for developers (1 sentence)
          Line 3: Migration complexity: 'low', 'medium', or 'high'

          Guidelines:
          - Be specific about the impact on API consumers
          - Mention backward compatibility concerns
          - Provide actionable recommendations
          - Consider both immediate and long-term implications

          Example response:
          The /api/v1/orders endpoint schema has been updated with 3 new required fields (customerId, shippingAddress, paymentMethod), breaking backward compatibility for existing integrations.
          Update your API clients to include the new required fields before the deprecation deadline, and implement proper validation for the new schema.
          high
          |]
   in systemPrompt <> "\n\n" <> baseContext


-- | Classify issue as critical/safe and count breaking/incremental changes
classifyIssueCriticality :: ELLM.LLM :> es => AuthContext -> Issues.Issue -> Eff es (Either Text (Bool, Int, Int))
classifyIssueCriticality authCtx issue = runExceptT do
  res <- ExceptT $ AI.callOpenAIAPIEff (buildCriticalityPrompt issue) authCtx.config.openaiApiKey
  (response, _) <- hoistEither $ AI.getNormalTupleReponse res
  case lines response of
    [criticalStr, breakingStr, incrementalStr] ->
      pure (T.toLower criticalStr == "critical", fromMaybe 0 $ readMaybe $ toString breakingStr, fromMaybe 0 $ readMaybe $ toString incrementalStr)
    _ -> hoistEither $ Left "Invalid response format from LLM"


-- | Build prompt for criticality classification
buildCriticalityPrompt :: Issues.Issue -> Text
buildCriticalityPrompt issue =
  let context = case issue.issueType of
        Issues.ApiChange ->
          withIssueData @Issues.APIChangeData
            issue
            ( \d ->
                [fmtTrim|API change detected
            Endpoint: {d.endpointMethod} {d.endpointPath}
            New fields: {V.length d.newFields}
            Deleted fields: {V.length d.deletedFields}
            Modified fields: {V.length d.modifiedFields}|]
            )
            ("API change: " <> issue.title)
        Issues.RuntimeException -> "Runtime exception: " <> issue.title
        Issues.QueryAlert -> "Query alert: " <> issue.title
        Issues.LogPattern -> "Log pattern: " <> issue.title
        Issues.LogPatternRateChange -> "Log pattern rate change: " <> issue.title

      systemPrompt =
        [text|
          You are an API monitoring assistant. Analyze this API change and classify it.
          Respond with exactly 3 lines:
          Line 1: 'critical' or 'safe' - Is this change critical?
          Line 2: Number of breaking changes (integer)
          Line 3: Number of incremental/safe changes (integer)

          Critical changes include:
          - Removing required fields
          - Changing field types incompatibly
          - Removing endpoints
          - Authentication/authorization changes
          - Runtime exceptions in core functionality

          Safe changes include:
          - Adding optional fields
          - New endpoints
          - Additional response data
          - Non-breaking format updates
          |]
   in systemPrompt <> "\n\n" <> context


-- | Update issue classification in database
updateIssueClassification :: DB es => Issues.IssueId -> Bool -> Int -> Int -> Eff es ()
updateIssueClassification issueId isCritical breakingCount incrementalCount = do
  let severity
        | isCritical = "critical"
        | breakingCount > 0 = "warning"
        | otherwise = "info"
  Issues.updateIssueCriticality issueId isCritical severity
