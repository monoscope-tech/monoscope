-- | In-memory interpreter for the filter subset of 'Pkg.Parser.Expr'.
--
-- The Events tab compiles an 'Expr' to a SQL @WHERE@ clause via @Display Expr@ and hands it to
-- TimeFusion. Live Tail has no database to hand it to: it holds the row in memory on the ingest
-- pod, before the write. So the same AST needs a second interpreter that answers "does this row
-- match?" directly.
--
-- Two interpreters over one AST rather than a second filter language is the point — a query
-- typed into Live Tail selects exactly the rows the identical query would have selected in
-- Events, and neither can drift from the parser.
--
-- == Semantics
--
-- The contract is __parity with the SQL this AST already lowers to__, not with Haskell
-- intuition or with Microsoft's KQL. Where those disagree, the SQL wins, because a user
-- switching between the two tabs with one query in hand is the case that must not surprise
-- them. Specifically:
--
-- [Missing fields] A subject that resolves to no value is SQL @NULL@: every comparison against
--   it is false, /including the negated ones/. @level != "error"@ does not match a row with no
--   @level@, because @NULL != 'error'@ is @NULL@ and the Events tab drops that row.
--   @x == null@ and @x != null@ are the presence tests (@IS NULL@ / @IS NOT NULL@).
--
-- [JSON null] A field explicitly set to @null@ is NULL as well: the stored column cannot
--   distinguish it from an absent one, so @x == null@ holds for both and @x != null@ for
--   neither.
--
-- [Numbers vs strings] Both sides are coerced to a number when both parse as one, so
--   @status_code >= 500@ works whether the SDK sent the status as @503@ or @"503"@. Otherwise
--   both sides are compared as text.
--
-- [Case sensitivity] @==@, @!=@, @in@, @!in@ and the ordering operators are case-/sensitive/,
--   matching SQL @=@. Every text operator — @has@, @contains@, @startswith@, @endswith@, their
--   negations, @has_any@, @has_all@, @=~@ and @matches regex@ — is case-/insensitive/, matching
--   the @~*@ / @like_regex … flag "i"@ that @Display Expr@ emits.
--
-- [@has@ token boundaries] Real KQL matches @has@ on whole tokens and @contains@ on substrings.
--   This codebase's SQL lowers /both/ to a case-insensitive substring match (@~*@ over a
--   regex-escaped term), so this evaluator does too. Parity with the shipped Events behaviour
--   beats parity with upstream KQL; changing it is a change to both interpreters at once.
--
-- [Arrays] Comparing against an array succeeds if /any/ element satisfies the comparison,
--   mirroring the jsonpath existence test the wildcard path lowers to. Objects and arrays used
--   as text compare as their JSON encoding, mirroring @::text@.
--
-- [Wildcards] @events[*].name@ denotes every matching element; a predicate holds when any one
--   of them satisfies it.
--
-- [Errors] An invalid or over-long regex is an 'EvalError', never a match. Callers count these
--   and treat the row as non-matching — silently matching on a broken filter would leak rows a
--   user did not ask for.
module Pkg.Parser.Eval (
  Resolver,
  EvalError (..),
  maxRegexLength,
  resolveIn,
  evalExpr,
  evalValue,
  filterExpr,
  jsonAsText,
  matchesRegex,
) where

import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as KEM
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific, toRealFloat)
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as TLE
import Pkg.Parser.Expr (Expr, Subject, Values)
import Pkg.Parser.Expr qualified as E
import Pkg.Parser.Stats (Section)
import Pkg.Parser.Stats qualified as S
import Relude
import Text.Regex.TDFA qualified as RE
import Text.Regex.TDFA.Text qualified as RE


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Pkg.Parser.Stats (parseQueryToAST)
-- >>> import Data.Either (fromRight)
-- >>> import Pkg.Parser.Expr (Subject(..), FieldKey(..))
-- >>> :{
-- let row :: AE.Value
--     row = fromRight AE.Null $ AE.eitherDecodeStrict' "{\"level\":\"error\",\"name\":\"GET /pay\",\"duration\":4200,\"resource\":{\"service\":{\"name\":\"checkout\"}},\"attributes\":{\"http\":{\"response\":{\"status_code\":503}}},\"summary\":[\"boom\",\"retrying\"],\"status_message\":null}"
--     match :: Text -> Either EvalError Bool
--     match q = case parseQueryToAST q >>= \s -> maybeToRight "not a filter" (filterExpr s) of
--       Right e -> evalExpr (resolveIn row) e
--       Left _ -> Right False
-- :}


-- | Every value a 'Subject' denotes in the row under test.
--
-- A list rather than a 'Maybe' so array wildcards fall out for free: the SQL side lowers those
-- to a @jsonb_path_query@ existence test, and here a predicate holds when /any/ denoted value
-- satisfies it. A plain subject denotes 0 or 1 value, so 'Maybe' would have forced wildcards
-- into a second, divergent code path.
type Resolver = Subject -> [AE.Value]


-- | Why a filter could not be decided for a row. Never a match.
data EvalError
  = -- | Pattern rejected by the regex engine.
    BadRegex Text
  | -- | Pattern longer than 'maxRegexLength'; refused before compiling it.
    RegexTooLong Int
  deriving stock (Eq, Show)


-- | Upper bound on a regex pattern, applied before compilation. KQL text is untrusted input
-- and TDFA builds an automaton eagerly, so an unbounded pattern is an ingest-path hazard.
maxRegexLength :: Int
maxRegexLength = 512


-- | The filter half of a parsed query, or 'Nothing' if it is not purely a filter.
--
-- Live Tail streams rows as they arrive, so anything needing a result /set/ — @summarize@,
-- @sort@, @take@, @project@, @extend@ — has no meaning here and is rejected at registration
-- rather than silently ignored. @source=@ is rejected too: Live Tail's source is fixed.
--
-- >>> filterExpr <$> parseQueryToAST "level == \"error\""
-- Right (Just (Eq (Subject "level" "level" []) (Str "error")))
--
-- Multiple filter sections conjoin, so a piped @where@ narrows rather than replaces:
--
-- >>> filterExpr <$> parseQueryToAST "level == \"error\" | where duration > 10"
-- Right (Just (And (Eq (Subject "level" "level" []) (Str "error")) (GT (Subject "duration" "duration" []) (Num "10"))))
--
-- >>> filterExpr <$> parseQueryToAST "level == \"error\" | summarize count() by kind"
-- Right Nothing
--
-- An empty query is the filter that matches everything:
--
-- >>> filterExpr []
-- Just (ValEq (Boolean True) (Boolean True))
filterExpr :: [Section] -> Maybe Expr
filterExpr = fmap (fromMaybe matchAll) . foldlM step Nothing
  where
    matchAll = E.ValEq (E.Boolean True) (E.Boolean True)
    step acc = \case
      S.Search e -> keep acc e
      S.WhereClause e -> keep acc e
      S.Source{} -> Nothing
      S.SummarizeCommand{} -> Nothing
      S.HavingClause{} -> Nothing
      S.ExtendCommand{} -> Nothing
      S.ProjectCommand{} -> Nothing
      S.SortCommand{} -> Nothing
      S.TakeCommand{} -> Nothing
    keep acc e = Just (Just (maybe e (`E.And` e) acc))


-- | Does the row satisfy the filter?
--
-- >>> match "level == \"error\""
-- Right True
-- >>> match "level == \"ERROR\""
-- Right False
--
-- Nested and aliased fields resolve the way they do in Events:
--
-- >>> match "service == \"checkout\" and attributes.http.response.status_code >= 500"
-- Right True
-- >>> match "span_name startswith \"get\" and duration > 1000"
-- Right True
--
-- Text operators are case-insensitive and match literally:
--
-- >>> match "name contains \"/PAY\""
-- Right True
-- >>> match "summary has_any (\"nope\", \"BOOM\")"
-- Right True
-- >>> match "summary has_all (\"boom\", \"missing\")"
-- Right False
-- >>> match "name !endswith \"/checkout\""
-- Right True
--
-- An absent field is SQL @NULL@: no comparison against it holds, negated or not.
--
-- >>> match "status_code == \"ERROR\""
-- Right False
-- >>> match "status_code != \"ERROR\""
-- Right False
-- >>> match "status_code !contains \"ERROR\""
-- Right False
-- >>> match "status_code == null"
-- Right True
-- >>> match "level != null"
-- Right True
--
-- A field explicitly set to JSON null is NULL too — SQL cannot tell it apart from an absent
-- column, so neither does this:
--
-- >>> match "status_message == null"
-- Right True
-- >>> match "status_message != null"
-- Right False
--
-- A broken regex is an error, never a match:
--
-- >>> evalExpr (resolveIn AE.Null) (E.Regex (Subject "name" "name" []) "[")
-- Left (BadRegex "[")
evalExpr :: Resolver -> Expr -> Either EvalError Bool
evalExpr r = go
  where
    go = \case
      E.And a b -> (&&) <$> go a <*> go b
      E.Or a b -> (||) <$> go a <*> go b
      E.Paren a -> go a
      -- `x == null` is `IS NULL`. An absent field and a field explicitly set to JSON null are
      -- both NULL to SQL — the stored column cannot tell them apart, so neither may this.
      E.Eq s E.Null -> pure (all (== AE.Null) (r s))
      E.NotEq s E.Null -> pure (any (/= AE.Null) (r s))
      E.Eq s v -> cmp s v (== EQ)
      E.NotEq s v -> cmp s v (/= EQ)
      E.GT s v -> cmp s v (== GT)
      E.LT s v -> cmp s v (== LT)
      E.GTEq s v -> cmp s v (/= LT)
      E.LTEq s v -> cmp s v (/= GT)
      E.In s v -> pure $ anyOf s \a -> any (eq a) (rhs v)
      E.NotIn s v -> pure $ anyOf s \a -> not (any (eq a) (rhs v))
      E.Has s v -> pure (txt s v T.isInfixOf)
      E.NotHas s v -> pure (negated s (txt s v T.isInfixOf))
      E.Contains s v -> pure (txt s v T.isInfixOf)
      E.NotContains s v -> pure (negated s (txt s v T.isInfixOf))
      E.StartsWith s v -> pure (txt s v T.isPrefixOf)
      E.NotStartsWith s v -> pure (negated s (txt s v T.isPrefixOf))
      E.EndsWith s v -> pure (txt s v T.isSuffixOf)
      E.NotEndsWith s v -> pure (negated s (txt s v T.isSuffixOf))
      E.HasAny s v -> pure $ any (\t -> txt s t T.isInfixOf) (items v)
      E.HasAll s v -> pure $ all (\t -> txt s t T.isInfixOf) (items v)
      E.Regex s pat -> re s pat
      E.Matches s pat -> re s pat
      E.ValEq a b -> pure (vcmp a b (== EQ))
      E.ValNotEq a b -> pure (vcmp a b (/= EQ))
      E.ValGT a b -> pure (vcmp a b (== GT))
      E.ValLT a b -> pure (vcmp a b (== LT))
      E.ValGTEq a b -> pure (vcmp a b (/= LT))
      E.ValLTEq a b -> pure (vcmp a b (/= GT))
      E.BoolFunc v -> pure (any truthy (evalValue r v))

    re s pat = (\p -> anyOf s (p . jsonAsText)) <$> matchesRegex pat

    anyOf s p = any p (r s)

    -- A negated text predicate still needs the subject to exist, so `!contains` drops
    -- absent-field rows exactly as `NOT (x ~* '…')` does on a NULL x.
    negated s p = not (null (r s)) && not p

    cmp s v p = pure case evalValue r v of
      [] -> False
      ys -> anyOf s \a -> any (maybe False p . cmpJson a) ys

    vcmp a b p = or [maybe False p (cmpJson x y) | x <- evalValue r a, y <- evalValue r b]

    txt s v f = case evalValue r v of
      [] -> False
      ys -> anyOf s \a -> any (\b -> T.toCaseFold (jsonAsText b) `f` T.toCaseFold (jsonAsText a)) ys

    eq a b = cmpJson a b == Just EQ

    -- The right-hand side of a list operator, as the values it denotes. `in` and `has_any`
    -- take a list; every other operator takes one value and reads as a singleton list.
    rhs = concatMap (evalValue r) . items

    items = \case
      E.List vs -> vs
      v -> [v]


-- | Every value a 'Values' denotes. Literals denote themselves; field references and scalar
-- functions resolve against the row.
--
-- Time literals are expected to have been resolved to 'E.TimestampLit' upstream (see
-- @resolveWildcardTimes@); an unresolved @now()@ denotes nothing rather than reaching for a
-- clock, which keeps this interpreter pure. Live Tail rejects time predicates at registration
-- anyway — every row it sees just arrived.
evalValue :: Resolver -> Values -> [AE.Value]
evalValue r = go
  where
    go = \case
      E.Num t -> maybe [] (pure . AE.Number) (readMaybe (toString t))
      E.Str t -> [AE.String t]
      E.Boolean b -> [AE.Bool b]
      E.Null -> [AE.Null]
      E.Duration _ ns -> [AE.Number (fromInteger ns)]
      E.TimestampLit iso -> [AE.String iso]
      E.List vs -> [AE.Array (fromList (concatMap go vs))]
      E.Field s -> r s
      E.TimeFunction _ -> []
      E.NowExpression -> []
      E.AgoExpression _ -> []
      E.ScalarFunc name args -> scalarFunc (T.toLower name) args

    -- Only the functions answerable from a single row. Anything else denotes nothing, which
    -- makes the comparison enclosing it false.
    scalarFunc name args = case (name, args) of
      ("coalesce", vs) -> take 1 (concatMap (filter (/= AE.Null) . go) vs)
      ("strcat", vs) -> [AE.String (foldMap (foldMap jsonAsText . go) vs)]
      ("iff", [c, t, f]) -> if any truthy (go c) then go t else go f
      ("isnull", [v]) -> [AE.Bool (all (== AE.Null) (go v))]
      ("isnotnull", [v]) -> [AE.Bool (any (/= AE.Null) (go v))]
      ("isempty", [v]) -> [AE.Bool (all (T.null . jsonAsText) (go v))]
      ("isnotempty", [v]) -> [AE.Bool (not (all (T.null . jsonAsText) (go v)))]
      ("tostring", [v]) -> [AE.String (foldMap jsonAsText (go v))]
      ("tolower", [v]) -> [AE.String (T.toLower (foldMap jsonAsText (go v)))]
      ("toupper", [v]) -> [AE.String (T.toUpper (foldMap jsonAsText (go v)))]
      ("tobool", [v]) -> [AE.Bool (any truthy (go v))]
      (n, [v]) | n `elem` numericCasts -> mapMaybe (fmap AE.Number . jsonAsNumber) (go v)
      _ -> []

    numericCasts = ["toint", "tolong", "tofloat", "todouble"] :: [Text]


truthy :: AE.Value -> Bool
truthy = \case
  AE.Bool b -> b
  AE.Null -> False
  AE.Number n -> n /= 0
  AE.String s -> not (T.null s)
  _ -> True


-- | The @::text@ rendering the SQL side compares against: strings as themselves, everything
-- else as its JSON encoding.
--
-- >>> jsonAsText (AE.String "hi")
-- "hi"
-- >>> jsonAsText (AE.Number 1)
-- "1"
-- >>> jsonAsText AE.Null
-- ""
jsonAsText :: AE.Value -> Text
jsonAsText = \case
  AE.String s -> s
  AE.Null -> ""
  v -> toStrict (TLE.decodeUtf8 (AE.encode v))


jsonAsNumber :: AE.Value -> Maybe Scientific
jsonAsNumber = \case
  AE.Number n -> Just n
  AE.String s -> readMaybe (toString s)
  AE.Bool b -> Just (if b then 1 else 0)
  _ -> Nothing


-- | Ordering across the loose typing a JSON row carries. Numbers win over strings when both
-- sides parse as numbers; otherwise both are compared as text.
cmpJson :: AE.Value -> AE.Value -> Maybe Ordering
cmpJson a b = case (a, b) of
  (AE.Null, AE.Null) -> Just EQ
  (AE.Null, _) -> Nothing
  (_, AE.Null) -> Nothing
  (AE.Bool x, AE.Bool y) -> Just (compare x y)
  (AE.Array xs, _) -> asum [cmpJson x b | x <- toList xs]
  (_, AE.Array ys) -> asum [cmpJson a y | y <- toList ys]
  _ -> case (jsonAsNumber a, jsonAsNumber b) of
    (Just x, Just y) -> Just (compare (toRealFloat @Double x) (toRealFloat y))
    _ -> Just (compare (jsonAsText a) (jsonAsText b))


-- | Compile a KQL pattern to a case-insensitive matcher, mirroring the @~*@ /
-- @like_regex … flag "i"@ the SQL side emits.
--
-- Returns the matcher rather than a 'Bool' so a filter compiles its patterns once at
-- registration instead of once per telemetry row.
--
-- >>> either (const Nothing) (\f -> Just (f "Hello")) (matchesRegex "hel+o")
-- Just True
-- >>> leftToMaybe (matchesRegex "[")
-- Just (BadRegex "[")
-- >>> leftToMaybe (matchesRegex (T.replicate 600 "a"))
-- Just (RegexTooLong 600)
matchesRegex :: Text -> Either EvalError (Text -> Bool)
matchesRegex pat
  | T.length pat > maxRegexLength = Left (RegexTooLong (T.length pat))
  | otherwise = case RE.compile RE.defaultCompOpt{RE.caseSensitive = False} RE.defaultExecOpt pat of
      Right re -> Right (either (const False) isJust . RE.execute re)
      Left _ -> Left (BadRegex pat)


-- | Resolve a subject against a decoded row.
--
-- Attribute keys arrive nested (the ingest path expands dot notation), but a handful stay
-- flat, so each step tries the whole remaining dotted path as a single key before descending
-- one level. That covers @attributes.http.request.method@ whichever shape it landed in.
--
-- >>> resolveIn (AE.object ["a" AE..= AE.object ["b" AE..= (1 :: Int)]]) (Subject "a.b" "a" [FieldKey "b"])
-- [Number 1.0]
-- >>> resolveIn (AE.object ["a.b" AE..= (2 :: Int)]) (Subject "a.b" "a" [FieldKey "b"])
-- [Number 2.0]
-- >>> resolveIn (AE.object ["a" AE..= ([1, 2] :: [Int])]) (Subject "a[*]" "a" [ArrayWildcard ""])
-- [Number 1.0,Number 2.0]
-- >>> resolveIn (AE.object ["a" AE..= (1 :: Int)]) (Subject "z" "z" [])
-- []
resolveIn :: AE.Value -> Resolver
resolveIn root (E.Subject entire primary keys) =
  case Map.lookup entire E.outputFieldAliases of
    -- Aliases are stored as SQL column names (`resource___service___name`); walk the
    -- underscore path so an alias resolves the same way its expansion would.
    Just col -> descend root (map E.FieldKey (T.splitOn "___" col))
    Nothing -> descend root (E.FieldKey primary : keys)
  where
    descend v [] = [v]
    descend v (k : rest) = case (k, v) of
      (E.FieldKey name, AE.Object o) -> flatOrNested o name rest
      (E.ArrayWildcard "", AE.Array xs) -> concatMap (`descend` rest) (toList xs)
      (E.ArrayWildcard name, AE.Object o) -> concatMap (`descend` rest) (arrayAt o name)
      (E.ArrayIndex "" i, AE.Array xs) -> maybe [] (`descend` rest) (toList xs !!? i)
      (E.ArrayIndex name i, AE.Object o) -> maybe [] (`descend` rest) (arrayAt o name !!? i)
      _ -> []

    arrayAt o name = case KEM.lookup (AEK.fromText name) o of
      Just (AE.Array xs) -> toList xs
      _ -> []

    -- `rest` may be spelled as one flat key ("http.request.method") or as nested objects.
    flatOrNested o name rest = fromMaybe nested flat
      where
        flat = do
          names <- traverse (\case E.FieldKey n -> Just n; _ -> Nothing) rest
          one <$> KEM.lookup (AEK.fromText (T.intercalate "." (name : names))) o
        nested = maybe [] (`descend` rest) (KEM.lookup (AEK.fromText name) o)
