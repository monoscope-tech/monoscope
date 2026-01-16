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
import Database.PostgreSQL.Simple.Newtypes (Aeson (..), getAeson)
import Effectful (Eff, (:>))
import Models.Apis.Issues qualified as Issues
import Pkg.AI qualified as AI
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


-- | Generate an enhanced title using LLM (or simple generator for supported types)
generateEnhancedTitle :: ELLM.LLM :> es => AuthContext -> Issues.Issue -> Eff es (Either Text Text)
generateEnhancedTitle authCtx issue = do
  -- First try simple title generation for supported types
  case simpleTitle issue of
    Just title -> pure $ Right title
    Nothing -> do
      -- Fall back to LLM for RuntimeException and QueryAlert
      let prompt = buildTitlePrompt issue
      result <- AI.callOpenAIAPIEff prompt authCtx.config.openaiApiKey
      case result of
        Left err -> pure $ Left err
        Right r -> do
          let response' = AI.getNormalTupleReponse r
          case response' of
            Left e -> pure $ Left e
            Right (title, _) -> pure $ Right $ T.take 200 title -- Limit title length


-- | Generate enhanced description with recommended actions (or simple generator for supported types)
generateEnhancedDescription :: ELLM.LLM :> es => AuthContext -> Issues.Issue -> Eff es (Either Text (Text, Text, Text))
generateEnhancedDescription authCtx issue = do
  -- First try simple description generation for supported types
  case simpleDescription issue of
    Just result -> pure $ Right result
    Nothing -> do
      -- Fall back to LLM for RuntimeException and QueryAlert
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
        Issues.RuntimeException ->
          let Aeson issueDataValue = issue.issueData
           in case AE.fromJSON issueDataValue of
                AE.Success (errorData :: Issues.RuntimeExceptionData) ->
                  "Generate a concise title for this runtime exception.\n"
                    <> "Error type: "
                    <> errorData.errorType
                    <> "\n"
                    <> "Error message: "
                    <> T.take 100 errorData.errorMessage
                    <> "\n"
                    <> "Service: "
                    <> fromMaybe "" issue.service
                _ -> "Generate a concise title for this runtime exception."
        Issues.QueryAlert ->
          let Aeson issueDataValue = issue.issueData
           in case AE.fromJSON issueDataValue of
                AE.Success (alertData :: Issues.QueryAlertData) ->
                  "Generate a concise title for this query alert.\n"
                    <> "Query: "
                    <> alertData.queryName
                    <> "\n"
                    <> "Threshold: "
                    <> toText (show alertData.thresholdValue)
                    <> " ("
                    <> alertData.thresholdType
                    <> ")\n"
                    <> "Actual value: "
                    <> toText (show alertData.actualValue)
                _ -> "Generate a concise title for this query alert."

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
        Issues.RuntimeException ->
          let Aeson issueDataValue = issue.issueData
           in case AE.fromJSON issueDataValue of
                AE.Success (errorData :: Issues.RuntimeExceptionData) ->
                  "Analyze this runtime exception and provide debugging guidance.\n"
                    <> "Error type: "
                    <> errorData.errorType
                    <> "\n"
                    <> "Error message: "
                    <> errorData.errorMessage
                    <> "\n"
                    <> "Stack trace: "
                    <> T.take 500 errorData.stackTrace
                    <> "\n"
                    <> "Request context: "
                    <> fromMaybe "Unknown" errorData.requestMethod
                    <> " "
                    <> fromMaybe "Unknown" errorData.requestPath
                    <> "\n"
                    <> "Occurrences: "
                    <> toText (show errorData.occurrenceCount)
                _ -> "Analyze this runtime exception."
        Issues.QueryAlert ->
          case AE.fromJSON (getAeson issue.issueData) of
            AE.Success (alertData :: Issues.QueryAlertData) ->
              "Describe this query alert and recommended actions.\n"
                <> "Query: "
                <> alertData.queryName
                <> "\n"
                <> "Expression: "
                <> alertData.queryExpression
                <> "\n"
                <> "Threshold: "
                <> toText (show alertData.thresholdValue)
                <> " ("
                <> alertData.thresholdType
                <> ")\n"
                <> "Actual value: "
                <> toText (show alertData.actualValue)
                <> "\n"
                <> "Triggered at: "
                <> toText (show alertData.triggeredAt)
            _ -> "Describe this query alert."

      systemPrompt =
        unlines
          [ "You are an API monitoring assistant. Generate detailed descriptions for API issues."
          , "Structure your response in exactly 3 lines:"
          , "Line 1: A clear description of what changed and why it matters (1-2 sentences)"
          , "Line 2: Recommended action for developers (1 sentence)"
          , "Line 3: Migration complexity: 'low', 'medium', or 'high'"
          , ""
          , "Guidelines:"
          , "- Be specific about the impact on API consumers"
          , "- Mention backward compatibility concerns"
          , "- Provide actionable recommendations"
          , "- Consider both immediate and long-term implications"
          , ""
          , "Example response:"
          , "The /api/v1/orders endpoint schema has been updated with 3 new required fields (customerId, shippingAddress, paymentMethod), breaking backward compatibility for existing integrations."
          , "Update your API clients to include the new required fields before the deprecation deadline, and implement proper validation for the new schema."
          , "high"
          ]
   in systemPrompt <> "\n\n" <> baseContext


-- | Classify issue as critical/safe and count breaking/incremental changes (or simple classifier for supported types)
classifyIssueCriticality :: ELLM.LLM :> es => AuthContext -> Issues.Issue -> Eff es (Either Text (Bool, Int, Int))
classifyIssueCriticality authCtx issue = do
  -- First try simple criticality classification for supported types
  case simpleCriticality issue of
    Just result -> pure $ Right result
    Nothing -> do
      -- Fall back to LLM for RuntimeException and QueryAlert
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
        Issues.RuntimeException ->
          "Runtime exception: " <> issue.title
        Issues.QueryAlert ->
          "Query alert: " <> issue.title

      systemPrompt =
        unlines
          [ "You are an API monitoring assistant. Analyze this API change and classify it."
          , "Respond with exactly 3 lines:"
          , "Line 1: 'critical' or 'safe' - Is this change critical?"
          , "Line 2: Number of breaking changes (integer)"
          , "Line 3: Number of incremental/safe changes (integer)"
          , ""
          , "Critical changes include:"
          , "- Removing required fields"
          , "- Changing field types incompatibly"
          , "- Removing endpoints"
          , "- Authentication/authorization changes"
          , "- Runtime exceptions in core functionality"
          , ""
          , "Safe changes include:"
          , "- Adding optional fields"
          , "- New endpoints"
          , "- Additional response data"
          , "- Non-breaking format updates"
          ]
   in systemPrompt <> "\n\n" <> context


-- | Update issue classification in database
updateIssueClassification :: DB es => Issues.IssueId -> Bool -> Int -> Int -> Eff es ()
updateIssueClassification issueId isCritical breakingCount incrementalCount = do
  let severity
        | isCritical = "critical"
        | breakingCount > 0 = "warning"
        | otherwise = "info"
  Issues.updateIssueCriticality issueId isCritical severity


-- | Generate a simple title for issue types that don't need LLM
simpleTitle :: Issues.Issue -> Maybe Text
simpleTitle issue = case issue.issueType of
  Issues.NewEndpoint -> Just $ "New endpoint detected"
  Issues.NewShape -> Just $ "New response shape detected"
  Issues.FieldChange ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.FieldChangeData) ->
        Just $ "Field " <> d.changeType <> " at " <> d.keyPath <> " on " <> d.endpointMethod <> " " <> d.endpointPath
      _ -> Just "Field change detected"
  Issues.LogPattern ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.LogPatternData) ->
        let level = fromMaybe "LOG" d.logLevel
         in Just $ "New " <> level <> " pattern detected"
      _ -> Just "New log pattern detected"
  Issues.ErrorEscalating ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.ErrorEscalatingData) ->
        Just $ "Error escalating: " <> T.take 60 d.errorType <> " (" <> toText (show d.escalationRate) <> "x in " <> d.escalationWindow <> ")"
      _ -> Just "Error rate escalating"
  Issues.ErrorRegressed ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.ErrorRegressedData) ->
        Just $ "Error regressed: " <> T.take 80 d.errorType
      _ -> Just "Previously resolved error has regressed"
  Issues.LogPatternRateChange ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.LogPatternRateChangeData) ->
        let direction = if d.changeDirection == "spike" then "spike" else "drop"
         in Just $ "Log pattern " <> direction <> ": " <> toText (show (round d.changePercent :: Int)) <> "% change"
      _ -> Just "Log pattern rate change detected"
  Issues.EndpointLatencyDegradation ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.EndpointLatencyDegradationData) ->
        Just $ "Latency degradation on " <> d.endpointMethod <> " " <> d.endpointPath <> " (" <> d.percentile <> " +" <> toText (show (round d.degradationPercent :: Int)) <> "%)"
      _ -> Just "Endpoint latency degradation detected"
  Issues.EndpointErrorRateSpike ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.EndpointErrorRateSpikeData) ->
        Just $ "Error rate spike on " <> d.endpointMethod <> " " <> d.endpointPath <> " (" <> toText (show (round (d.currentErrorRate * 100) :: Int)) <> "% error rate)"
      _ -> Just "Endpoint error rate spike detected"
  Issues.EndpointVolumeRateChange ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.EndpointVolumeRateChangeData) ->
        let direction = if d.changeDirection == "spike" then "Traffic spike" else "Traffic drop"
         in Just $ direction <> " on " <> d.endpointMethod <> " " <> d.endpointPath <> " (" <> toText (show (round d.changePercent :: Int)) <> "%)"
      _ -> Just "Endpoint traffic volume change detected"
  -- LLM-based types return Nothing to use LLM
  Issues.RuntimeException -> Nothing
  Issues.QueryAlert -> Nothing


-- | Generate a simple description for issue types that don't need LLM
-- Returns (description, recommendedAction, complexity)
simpleDescription :: Issues.Issue -> Maybe (Text, Text, Text)
simpleDescription issue = case issue.issueType of
  Issues.NewEndpoint ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.NewEndpointData) ->
        Just
          ( "A new endpoint " <> d.endpointMethod <> " " <> d.endpointPath <> " was discovered on host " <> d.endpointHost <> ". This endpoint was not previously tracked."
          , "Review the endpoint to ensure it's expected and properly documented."
          , "low"
          )
      _ -> Just ("A new endpoint was discovered.", "Review the endpoint.", "low")
  Issues.NewShape ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.NewShapeData) ->
        let newCount = V.length d.newFields
            deletedCount = V.length d.deletedFields
            modifiedCount = V.length d.modifiedFields
            complexity
              | deletedCount > 0 = "high"
              | modifiedCount > 0 = "medium"
              | otherwise = "low"
         in Just
              ( "New response shape detected on "
                  <> d.endpointMethod
                  <> " "
                  <> d.endpointPath
                  <> " (status "
                  <> toText (show d.statusCode)
                  <> "). "
                  <> "New fields: "
                  <> toText (show newCount)
                  <> ", deleted fields: "
                  <> toText (show deletedCount)
                  <> ", modified fields: "
                  <> toText (show modifiedCount)
                  <> "."
              , "Review the schema changes and update API clients if necessary."
              , complexity
              )
      _ -> Just ("A new response shape was detected.", "Review the schema changes.", "medium")
  Issues.FieldChange ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.FieldChangeData) ->
        let complexity = case d.changeType of
              "deleted" -> "high"
              "type_changed" -> "high"
              "added" -> "low"
              _ -> "medium"
            typeInfo = case d.previousType of
              Just prev -> " Changed from " <> prev <> " to " <> d.newType <> "."
              Nothing -> " New type: " <> d.newType <> "."
         in Just
              ( "Field " <> d.changeType <> " detected at path '" <> d.keyPath <> "' on " <> d.endpointMethod <> " " <> d.endpointPath <> "." <> typeInfo
              , "Update API clients to handle this field change."
              , complexity
              )
      _ -> Just ("A field change was detected.", "Review the field change.", "medium")
  Issues.LogPattern ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.LogPatternData) ->
        let level = fromMaybe "LOG" d.logLevel
            svc = fromMaybe "unknown service" d.serviceName
         in Just
              ( "New " <> level <> " log pattern detected in " <> svc <> ". Pattern: " <> T.take 200 d.logPattern <> ". Occurrences: " <> toText (show d.occurrenceCount) <> "."
              , "Investigate the log pattern to determine if action is needed."
              , "low"
              )
      _ -> Just ("A new log pattern was detected.", "Review the log pattern.", "low")
  Issues.ErrorEscalating ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.ErrorEscalatingData) ->
        let svc = fromMaybe "unknown service" d.serviceName
         in Just
              ( "Error '"
                  <> T.take 100 d.errorType
                  <> "' is escalating in "
                  <> svc
                  <> ". "
                  <> "Rate increased "
                  <> toText (show d.escalationRate)
                  <> "x over the last "
                  <> d.escalationWindow
                  <> ". "
                  <> "Last hour: "
                  <> toText (show d.occurrences1h)
                  <> " occurrences, last 24h: "
                  <> toText (show d.occurrences24h)
                  <> "."
              , "Investigate and fix the root cause urgently to prevent further escalation."
              , "high"
              )
      _ -> Just ("An error is escalating.", "Investigate immediately.", "high")
  Issues.ErrorRegressed ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.ErrorRegressedData) ->
        let svc = fromMaybe "unknown service" d.serviceName
            quietDays = d.quietPeriodMinutes `div` 1440
            quietHours = (d.quietPeriodMinutes `mod` 1440) `div` 60
            quietStr
              | quietDays > 0 = toText (show quietDays) <> " days"
              | otherwise = toText (show quietHours) <> " hours"
         in Just
              ( "Previously resolved error '"
                  <> T.take 100 d.errorType
                  <> "' has regressed in "
                  <> svc
                  <> ". "
                  <> "It was quiet for "
                  <> quietStr
                  <> " before reappearing. "
                  <> "New occurrences: "
                  <> toText (show d.newOccurrences)
                  <> "."
              , "Investigate why this error has returned after being resolved."
              , "high"
              )
      _ -> Just ("A previously resolved error has regressed.", "Investigate the regression.", "high")
  Issues.LogPatternRateChange ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.LogPatternRateChangeData) ->
        let svc = fromMaybe "unknown service" d.serviceName
            direction = if d.changeDirection == "spike" then "spiked" else "dropped"
            complexity = if d.changeDirection == "spike" then "medium" else "low"
         in Just
              ( "Log pattern rate "
                  <> direction
                  <> " in "
                  <> svc
                  <> ". "
                  <> "Current rate: "
                  <> toText (show (round d.currentRatePerHour :: Int))
                  <> "/hour (baseline: "
                  <> toText (show (round d.baselineMean :: Int))
                  <> "/hour). "
                  <> "Change: "
                  <> toText (show (round d.changePercent :: Int))
                  <> "%, z-score: "
                  <> toText (show (round d.zScore :: Int))
                  <> "."
              , "Review the log volume change to determine if it indicates an issue."
              , complexity
              )
      _ -> Just ("Log pattern rate changed significantly.", "Review the change.", "medium")
  Issues.EndpointLatencyDegradation ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.EndpointLatencyDegradationData) ->
        let svc = maybe "" (" in " <>) d.serviceName
         in Just
              ( "Latency degradation detected on "
                  <> d.endpointMethod
                  <> " "
                  <> d.endpointPath
                  <> svc
                  <> ". "
                  <> d.percentile
                  <> " latency increased from "
                  <> toText (show (round d.baselineLatencyMs :: Int))
                  <> "ms to "
                  <> toText (show (round d.currentLatencyMs :: Int))
                  <> "ms (+"
                  <> toText (show (round d.degradationPercent :: Int))
                  <> "%)."
              , "Profile the endpoint to identify performance bottlenecks."
              , "medium"
              )
      _ -> Just ("Endpoint latency has degraded.", "Investigate performance issues.", "medium")
  Issues.EndpointErrorRateSpike ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.EndpointErrorRateSpikeData) ->
        let svc = maybe "" (" in " <>) d.serviceName
         in Just
              ( "Error rate spike on "
                  <> d.endpointMethod
                  <> " "
                  <> d.endpointPath
                  <> svc
                  <> ". "
                  <> "Current error rate: "
                  <> toText (show (round (d.currentErrorRate * 100) :: Int))
                  <> "% ("
                  <> toText (show d.errorCount)
                  <> " errors out of "
                  <> toText (show d.totalRequests)
                  <> " requests). "
                  <> "Baseline: "
                  <> toText (show (round (d.baselineErrorRate * 100) :: Int))
                  <> "%."
              , "Investigate the error spike and fix the underlying issues."
              , "high"
              )
      _ -> Just ("Endpoint error rate has spiked.", "Investigate errors immediately.", "high")
  Issues.EndpointVolumeRateChange ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.EndpointVolumeRateChangeData) ->
        let svc = maybe "" (" in " <>) d.serviceName
            direction = if d.changeDirection == "spike" then "spiked" else "dropped"
            complexity = if d.changeDirection == "spike" then "medium" else "medium"
         in Just
              ( "Traffic volume "
                  <> direction
                  <> " on "
                  <> d.endpointMethod
                  <> " "
                  <> d.endpointPath
                  <> svc
                  <> ". "
                  <> "Current rate: "
                  <> toText (show (round d.currentRatePerHour :: Int))
                  <> " req/hour (baseline: "
                  <> toText (show (round d.baselineRatePerHour :: Int))
                  <> " req/hour). "
                  <> "Change: "
                  <> toText (show (round d.changePercent :: Int))
                  <> "%."
              , "Review traffic patterns to determine if this is expected or indicates an issue."
              , complexity
              )
      _ -> Just ("Endpoint traffic volume changed significantly.", "Review traffic patterns.", "medium")
  -- LLM-based types return Nothing to use LLM
  Issues.RuntimeException -> Nothing
  Issues.QueryAlert -> Nothing


-- | Simple criticality classification for non-LLM issue types
-- Returns (isCritical, breakingCount, incrementalCount)
simpleCriticality :: Issues.Issue -> Maybe (Bool, Int, Int)
simpleCriticality issue = case issue.issueType of
  Issues.NewEndpoint -> Just (False, 0, 1)
  Issues.NewShape ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.NewShapeData) ->
        let deletedCount = V.length d.deletedFields
            modifiedCount = V.length d.modifiedFields
            newCount = V.length d.newFields
            isCritical = deletedCount > 0 || modifiedCount > 0
         in Just (isCritical, deletedCount + modifiedCount, newCount)
      _ -> Just (False, 0, 1)
  Issues.FieldChange ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.FieldChangeData) ->
        let isCritical = d.changeType `elem` ["deleted", "type_changed"]
         in Just (isCritical, if isCritical then 1 else 0, if isCritical then 0 else 1)
      _ -> Just (False, 0, 1)
  Issues.LogPattern -> Just (False, 0, 1)
  Issues.ErrorEscalating -> Just (True, 1, 0)
  Issues.ErrorRegressed -> Just (True, 1, 0)
  Issues.LogPatternRateChange ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.LogPatternRateChangeData) ->
        let isCritical = d.changeDirection == "spike" && d.zScore > 5
         in Just (isCritical, if isCritical then 1 else 0, 1)
      _ -> Just (False, 0, 1)
  Issues.EndpointLatencyDegradation ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.EndpointLatencyDegradationData) ->
        let isCritical = d.degradationPercent > 100 || d.zScore > 5
         in Just (isCritical, if isCritical then 1 else 0, 1)
      _ -> Just (False, 0, 1)
  Issues.EndpointErrorRateSpike -> Just (True, 1, 0)
  Issues.EndpointVolumeRateChange ->
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.EndpointVolumeRateChangeData) ->
        let isCritical = d.changeDirection == "drop" && d.changePercent < -50
         in Just (isCritical, if isCritical then 1 else 0, 1)
      _ -> Just (False, 0, 1)
  -- LLM-based types return Nothing to use LLM
  Issues.RuntimeException -> Nothing
  Issues.QueryAlert -> Nothing
