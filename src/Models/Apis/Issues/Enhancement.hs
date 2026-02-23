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
enhanceIssueWithLLM authCtx issue = do
  -- Generate enhanced title
  titleResult <- generateEnhancedTitle authCtx issue
  case titleResult of
    Left err -> pure $ Left err
    Right enhancedTitle -> do
      -- Generate enhanced description
      descResult <- generateEnhancedDescription authCtx issue
      case descResult of
        Left err -> pure $ Left err
        Right (enhancedDesc, recommendedAction, complexity) ->
          pure
            $ Right
              IssueEnhancement
                { issueId = issue.id
                , enhancedTitle = enhancedTitle
                , enhancedDescription = enhancedDesc
                , recommendedAction = recommendedAction
                , migrationComplexity = complexity
                }


-- | Generate an enhanced title using LLM
generateEnhancedTitle :: ELLM.LLM :> es => AuthContext -> Issues.Issue -> Eff es (Either Text Text)
generateEnhancedTitle authCtx issue = do
  let prompt = buildTitlePrompt issue
  result <- AI.callOpenAIAPIEff prompt authCtx.config.openaiApiKey
  case result of
    Left err -> pure $ Left err
    Right r -> do
      let response' = AI.getNormalTupleReponse r
      case response' of
        Left e -> pure $ Left e
        Right (title, _) -> pure $ Right $ T.take 200 title -- Limit title length


-- | Generate enhanced description with recommended actions
generateEnhancedDescription :: ELLM.LLM :> es => AuthContext -> Issues.Issue -> Eff es (Either Text (Text, Text, Text))
generateEnhancedDescription authCtx issue = do
  let prompt = buildDescriptionPrompt issue
  result <- AI.callOpenAIAPIEff prompt authCtx.config.openaiApiKey
  case result of
    Left err -> pure $ Left err
    Right r -> do
      let response' = AI.getNormalTupleReponse r
      case response' of
        Left e -> pure $ Left e
        Right (response, _) -> do
          let lines' = lines response
              description = fromMaybe "" $ viaNonEmpty head lines'
              recommendedAction = fromMaybe "Review the changes and update your integration accordingly." $ lines' !!? 1
              complexity = fromMaybe "medium" $ lines' !!? 2
          -- Note: Classification happens separately in the background job
          pure $ Right (description, recommendedAction, complexity)


-- | Build prompt for title generation
buildTitlePrompt :: Issues.Issue -> Text
buildTitlePrompt issue =
  let baseContext = case issue.issueType of
        Issues.APIChange -> case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.APIChangeData) ->
            [fmtTrim|
            Generate a concise, descriptive title for this API change.
            Endpoint: {d.endpointMethod} {d.endpointPath}
            New fields: {V.length d.newFields}
            Deleted fields: {V.length d.deletedFields}
            Modified fields: {V.length d.modifiedFields}
            Service: {issue.service}
            |]
          _ -> "Generate a concise title for this API change."
        Issues.RuntimeException -> case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.RuntimeExceptionData) ->
            [fmtTrim|
            Generate a concise title for this runtime exception.
            Error type: {d.errorType}
            Error message: {T.take 100 d.errorMessage}
            Service: {issue.service}
            |]
          _ -> "Generate a concise title for this runtime exception."
        Issues.QueryAlert -> case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.QueryAlertData) ->
            [fmtTrim|
            Generate a concise title for this query alert.
            Query: {d.queryName}
            Threshold: {d.thresholdValue} ({d.thresholdType})
            Actual value: {d.actualValue}
            |]
          _ -> "Generate a concise title for this query alert."
        Issues.LogPattern ->
          let title = issue.title
              service = fromMaybe "unknown-service" issue.service
           in [text|
                Generate a concise title for this log pattern issue.
                Title: $title
                Service: $service
                |]
        Issues.LogPatternRateChange ->
          let title = issue.title
              service = fromMaybe "unknown-service" issue.service
           in [text|
                Generate a concise title for this log pattern rate change.
                Title: $title
                Service: $service
                |]

      systemPrompt =
        [text|
          You are an API monitoring assistant. Generate clear, actionable titles for API issues.
          Keep titles under 80 characters.
          Focus on the impact and what changed.
          Use present tense and active voice.
          Examples:
          - 'New User Authentication Endpoint Added to Auth Service'
          - 'Breaking Change: 5 Required Fields Removed from Order Response'
          - 'Payment Service Schema Updated with 3 New Optional Fields'
          - 'Critical: NullPointerException in Cart Service Checkout Flow'
          |]
   in systemPrompt <> "\n\n" <> baseContext


-- | Build prompt for description generation
buildDescriptionPrompt :: Issues.Issue -> Text
buildDescriptionPrompt issue =
  let baseContext = case issue.issueType of
        Issues.APIChange -> case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.APIChangeData) ->
            [fmtTrim|
            Describe this API change and its impact.
            Endpoint: {d.endpointMethod} {d.endpointPath}
            New fields: {show $ V.toList d.newFields}
            Deleted fields: {show $ V.toList d.deletedFields}
            Modified fields: {show $ V.toList d.modifiedFields}
            Total anomalies grouped: {V.length d.anomalyHashes}
            Service: {issue.service}
            |]
          _ -> "Describe this API change and its implications."
        Issues.RuntimeException -> case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.RuntimeExceptionData) ->
            [fmtTrim|
            Analyze this runtime exception and provide debugging guidance.
            Error type: {d.errorType}
            Error message: {d.errorMessage}
            Stack trace: {T.take 500 d.stackTrace}
            Request context: {fromMaybe "Unknown" d.requestMethod} {fromMaybe "Unknown" d.requestPath}
            Occurrences: {d.occurrenceCount}
            |]
          _ -> "Analyze this runtime exception."
        Issues.QueryAlert -> case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.QueryAlertData) ->
            [fmtTrim|
            Describe this query alert and recommended actions.
            Query: {d.queryName}
            Expression: {d.queryExpression}
            Threshold: {d.thresholdValue} ({d.thresholdType})
            Actual value: {d.actualValue}
            Triggered at: {show d.triggeredAt}
            |]
          _ -> "Describe this query alert."

        Issues.LogPattern ->
          let title = issue.title
              service = fromMaybe "unknown-service" issue.service
           in [text|
                Describe this log pattern issue and its implications.
                Title: $title
                Service: $service
                |]
        Issues.LogPatternRateChange ->
          let title = issue.title
              service = fromMaybe "unknown-service" issue.service
           in [text|
                Describe this log pattern rate change and its implications.
                Title: $title
                Service: $service
                |]


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
classifyIssueCriticality authCtx issue = do
  let prompt = buildCriticalityPrompt issue
  result' <- AI.callOpenAIAPIEff prompt authCtx.config.openaiApiKey
  case result' of
    Left err -> pure $ Left err
    Right res -> do
      let r = AI.getNormalTupleReponse res
      case r of
        Left e -> pure $ Left e
        Right (response, _) -> do
          let lines' = lines response
          case lines' of
            [criticalStr, breakingStr, incrementalStr] -> do
              let isCritical = T.toLower criticalStr == "critical"
                  breakingCount = fromMaybe 0 $ readMaybe $ toString breakingStr
                  incrementalCount = fromMaybe 0 $ readMaybe $ toString incrementalStr
              pure $ Right (isCritical, breakingCount, incrementalCount)
            _ -> pure $ Left "Invalid response format from LLM"


-- | Build prompt for criticality classification
buildCriticalityPrompt :: Issues.Issue -> Text
buildCriticalityPrompt issue =
  let context = case issue.issueType of
        Issues.APIChange -> case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.APIChangeData) ->
            [fmtTrim|
            API change detected
            Endpoint: {d.endpointMethod} {d.endpointPath}
            New fields: {V.length d.newFields}
            Deleted fields: {V.length d.deletedFields}
            Modified fields: {V.length d.modifiedFields}
            |]
          _ -> "API change: " <> issue.title
        Issues.RuntimeException -> "Runtime exception: " <> issue.title
        Issues.QueryAlert -> "Query alert: " <> issue.title
        Issues.LogPattern ->
          "Log pattern: " <> issue.title
        Issues.LogPatternRateChange ->
          "Log pattern rate change: " <> issue.title

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
