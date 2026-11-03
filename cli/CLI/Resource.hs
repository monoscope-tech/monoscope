-- | Resource commands derived from "Web.Operations": 'runCall' interprets any
-- catalogued v1 operation, plus the shared list rendering and picker.
module CLI.Resource (
  ApiCall (..),
  Input (..),
  runCall,
  selectResourceId,
  runAPI,
) where

import Relude

import CLI.Config (CLIConfig (..))
import CLI.Core (APIError, OutputMode (..), apiDelete, apiGetJson, apiPatchJson, apiPostJson, apiPutJson, dieWith, printError, renderAPIError, renderByMode, renderWith)
import CLI.Table (Align (..), CellStyle (..), RichTableOpts (..), brand, defaultRichTableOpts, level, muted, numeric, paginationFooter, parseSeverity, plain, renderRichTableWith)
import CLI.UI (Picker, pickItem, pickerAvailable)
import Control.Lens (has, (^..), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AK
import Data.Aeson.Lens qualified as AL
import Data.Effectful.Wreq (HTTP)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import Effectful
import Effectful.Environment (Environment)
import Effectful.FileSystem (FileSystem, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Web.Operations qualified as Op
import Web.Wire (Paged (..))


-- | One parsed argument, already in its wire form.
data Input = QueryParam Text Text | BodyField AE.Key AE.Value | BodyFrom FilePath | ApplyFrom FilePath


-- | A parsed catalogue command: path values after the pickable id, and its inputs.
data ApiCall = ApiCall {operation :: Op.Operation, listing :: Maybe Op.Listing, ident :: Maybe Text, pathArgs :: [Text], inputs :: [Input]}


-- | Path params fill in order (the first via the picker); a listing renders its
-- table, @/yaml@ as YAML, DELETE as a confirmation line.
runCall :: (Environment :> es, FileSystem :> es, HTTP :> es, IOE :> es, Picker :> es) => CLIConfig -> OutputMode -> ApiCall -> Eff es ()
runCall cfg mode call = do
  ids <- case Op.pathParams o.path of
    [] -> pure []
    _ -> (: call.pathArgs) <$> selectResourceId cfg (T.takeWhile (/= '/') (T.drop 1 o.path)) mode call.ident
  let url = "/api/v1" <> T.intercalate "/" (fill ids (T.splitOn "/" o.path))
      query = [(k, v) | QueryParam k v <- call.inputs]
      fields = [(k, v) | BodyField k v <- call.inputs]
  case (o.method, [p | ApplyFrom p <- call.inputs]) of
    (_, p : _) -> runApplyResource cfg url p mode
    (Op.GET, _)
      | Just l <- call.listing -> runList cfg l url query mode
      | "/yaml" `T.isSuffixOf` o.path -> withResult (apiGetJson @_ @AE.Value cfg url []) renderAPIError (liftIO . putBSLn . Yaml.encode)
      | otherwise -> runAPI mode (apiGetJson @_ @AE.Value cfg url query)
    (Op.DELETE, _) -> withResult (apiDelete cfg url) renderAPIError \() -> putTextLn (url <> " deleted")
    (Op.Write verb, _) -> do
      body <- maybe (pure $ if null fields then AE.Null else AE.object fields) readYamlOrJson (listToMaybe [p | BodyFrom p <- call.inputs])
      runAPI mode (writeJsonRaw cfg verb url query body)
  where
    o = call.operation
    fill vs = \case
      seg : rest | "{" `T.isPrefixOf` seg, v : vs' <- vs -> v : fill vs' rest
      seg : rest -> seg : fill vs rest
      [] -> []


-- | Run an API-producing action, exit with `printError` on Left, call
-- @onOk@ on Right. Used to dispatch API responses uniformly across
-- command handlers.
withResult :: IOE :> es => Eff es (Either e a) -> (e -> Text) -> (a -> Eff es ()) -> Eff es ()
withResult act renderErr onOk =
  act >>= \case
    Left e -> dieWith (renderErr e)
    Right v -> onOk v


-- | Specialised `withResult` for API calls returning `APIError`, with the
-- common "render result with OutputMode, no table variant" continuation.
runAPI :: (AE.ToJSON a, IOE :> es) => OutputMode -> Eff es (Either APIError a) -> Eff es ()
runAPI mode act = withResult act renderAPIError (renderByMode mode Nothing)


-- | Fetch a list endpoint and normalise it to @{data, pagination}@ (the server
-- returns bare arrays for some resources, 'Paged' for others), so agents see
-- one shape across every listing. Unrecognised envelopes go to always-on
-- stderr (not printDebug): an agent expecting {data, pagination} would
-- silently get a broken shape otherwise.
runList :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> Op.Listing -> Text -> [(Text, Text)] -> OutputMode -> Eff es ()
runList cfg l url params mode =
  apiGetJson @_ @AE.Value cfg url params >>= \case
    Left e -> dieWith (renderAPIError e)
    Right v -> case normalizeListE v of
      Left (raw, reason) -> printError ("list " <> url <> ": " <> reason) >> renderByMode mode Nothing raw
      Right payload -> renderWith mode payload do
        renderRichTableWith defaultRichTableOpts{alignments = [bool AlignLeft AlignRight (c.style == Op.Numeric) | c <- cols]} (map (.header) cols) [map (cell row) cols | row <- payload ^.. AL.key "data" . AL.values]
        when (payload ^? AL.key "pagination" . AL.key "has_more" == Just (AE.Bool True))
          $ paginationFooter "… more — pass --limit / --page or use --json to see the cursor"
  where
    cols = idColumn l : l.columns
    cell row c =
      let t = cellText row c
       in case c.style of
            Op.Brand -> brand t
            Op.Muted -> muted t
            Op.Numeric -> numeric t
            Op.Plain -> plain t
            Op.Active -> case t of "true" -> (Success, "active"); "false" -> muted "inactive"; _ -> plain t
            Op.State ->
              let u = T.toUpper t
               in case parseSeverity u of
                    Just sev -> level sev u
                    Nothing
                      | u `elem` ["OK", "RESOLVED"] -> (Success, u)
                      | u `elem` ["FAILED", "ALERTING"] -> (Failure, u)
                      | T.null u -> muted "-"
                      | otherwise -> plain u


idColumn :: Op.Listing -> Op.Column
idColumn l = Op.Column l.idField (pure l.idField) Op.Brand


-- | First non-empty dotted field of a listing row.
cellText :: AE.Value -> Op.Column -> Text
cellText row c = fromMaybe "" $ find (not . T.null) [maybe "" scalar (foldl' (\v k -> v >>= (^? AL.key (AK.fromText k))) (Just row) (T.splitOn "." f)) | f <- toList c.fields]
  where
    scalar = \case
      AE.String s -> s
      AE.Number n -> decodeUtf8 (AE.encode n)
      AE.Bool b -> bool "false" "true" b
      AE.Null -> ""
      _ -> "…"


-- | Pick a row from the segment's listing (all pages) when no id was given.
selectResourceId :: (Environment :> es, HTTP :> es, IOE :> es, Picker :> es) => CLIConfig -> Text -> OutputMode -> Maybe Text -> Eff es Text
selectResourceId _ _ _ (Just rid) = pure rid
selectResourceId cfg segment mode Nothing = do
  terminal <- pickerAvailable
  (resource, path, l) <- maybe (dieWith "An ID is required.") pure (Op.listingOf segment)
  unless (terminal && mode == OutputTable) $ dieWith "An ID is required outside an interactive terminal. Use the list command to find one."
  items <- fetchPages ("/api/v1" <> path) 0
  when (null items) $ dieWith "No items available to select."
  pickItem ("Select " <> resource) [(cellText v (idColumn l), T.intercalate " · " (map (cellText v) (l.columns <> [idColumn l]))) | v <- items] >>= maybe exitSuccess pure
  where
    fetchPages url page =
      apiGetJson @_ @AE.Value cfg url [("page", show (page :: Int))] >>= \case
        Left err -> dieWith (renderAPIError err)
        Right value -> case normalizeListE value of
          Left (_, reason) -> dieWith reason
          Right payload -> case payload ^? AL.key "data" . AL._Array of
            Nothing -> dieWith "List response has no data array."
            Just rows -> (toList rows <>) <$> bool (pure []) (fetchPages url (page + 1)) (payload ^? AL.key "pagination" . AL.key "has_more" == Just (AE.Bool True))


-- | Normalise a list response to @{data, pagination}@. Returns 'Right' with
-- the normalised value, or 'Left' carrying @(rawValue, reason)@ when the
-- shape doesn't match any known envelope. The reason includes the aeson
-- decode error when an Object fails @Paged@ decoding so contributors see
-- *what* changed in the server envelope, not just a generic mismatch.
--
-- Recognises three shapes:
--   1. Already @{data, pagination}@ → passthrough
--   2. Server's typed @Paged@ envelope (Web.ApiTypes.Paged) → repackaged
--   3. Bare array → wrapped with empty pagination metadata
normalizeListE :: AE.Value -> Either (AE.Value, Text) AE.Value
normalizeListE v
  | hasKey "data" && hasKey "pagination" = Right v
  | AE.Array _ <- v =
      Right
        $ AE.object
          [ "data" AE..= v
          , "pagination" AE..= AE.object ["has_more" AE..= False, "total" AE..= AE.Null, "cursor" AE..= AE.Null]
          ]
  | AE.Object _ <- v = case AE.fromJSON @(Paged AE.Value) v of
      AE.Success p ->
        Right
          $ AE.object
            [ "data" AE..= p.items
            , "pagination"
                AE..= AE.object
                  [ "has_more" AE..= p.hasMore
                  , "total" AE..= p.totalCount
                  , "cursor" AE..= AE.Null
                  , "page" AE..= p.page
                  , "per_page" AE..= p.perPage
                  ]
            ]
      AE.Error msg -> Left (v, "Paged decode failed: " <> toText msg)
  | otherwise = Left (v, "response is neither Object nor Array")
  where
    hasKey k = has (AL.key k) v


-- | Non-exiting write, so 'runApplyResource' can continue past a failure.
writeJsonRaw
  :: (Environment :> es, HTTP :> es, IOE :> es)
  => CLIConfig -> Op.Verb -> Text -> [(Text, Text)] -> AE.Value -> Eff es (Either APIError AE.Value)
writeJsonRaw cfg = \case
  Op.POST -> apiPostJson @_ @_ @AE.Value cfg
  Op.PUT -> apiPutJson @_ @_ @AE.Value cfg
  Op.PATCH -> apiPatchJson @_ @_ @AE.Value cfg


-- | POST each .yaml/.yml/.json file at @path@ (or @path@ itself) to an
-- @/apply@ upsert @url@. Per-file failures are printed but do not abort the
-- batch; the command exits non-zero at the end iff any file failed.
runApplyResource
  :: (Environment :> es, FileSystem :> es, HTTP :> es, IOE :> es)
  => CLIConfig -> Text -> FilePath -> OutputMode -> Eff es ()
runApplyResource cfg url path mode = do
  paths <-
    ifM
      (doesDirectoryExist path)
      (fmap (path </>) . filter ((`elem` [".yaml", ".yml", ".json"]) . takeExtension) <$> listDirectory path)
      (ifM (doesFileExist path) (pure [path]) (dieWith ("file not found: " <> toText path)))
  when (null paths)
    $ dieWith ("no applyable (.yaml/.yml/.json) files in " <> toText path)
  failures <- fmap sum . forM paths $ \p ->
    readYamlOrJson p >>= writeJsonRaw cfg Op.POST url [] >>= \case
      Left e -> printError (toText p <> ": " <> renderAPIError e) $> (1 :: Int)
      Right v -> renderByMode mode Nothing v $> 0
  when (failures > 0)
    $ dieWith (show failures <> " file(s) failed to apply")


-- | Read + parse a YAML or JSON file. Exits with an error message on missing file or parse failure.
-- Uses the JSON parser for @.json@ (clearer errors) and the YAML parser otherwise
-- (YAML is a JSON superset, so this handles @.yaml@/@.yml@/unsuffixed files).
readYamlOrJson :: (FileSystem :> es, IOE :> es) => FilePath -> Eff es AE.Value
readYamlOrJson path = do
  unlessM (doesFileExist path) $ dieWith ("file not found: " <> toText path)
  if takeExtension path == ".json"
    then liftIO (AE.eitherDecodeFileStrict path) >>= either (failWith "json") pure
    else liftIO (Yaml.decodeFileEither path) >>= either (failWith "yaml" . Yaml.prettyPrintParseException) pure
  where
    failWith kind err = dieWith (toText path <> ": " <> kind <> " parse error: " <> toText err)
