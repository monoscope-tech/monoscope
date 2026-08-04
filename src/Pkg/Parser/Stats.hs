module Pkg.Parser.Stats (
  -- Types
  AggFunction (..),
  ArithOp (..),
  ScalarExpr (..),
  Section (..),
  SummarizeByClause (..),
  ByClauseItem (..),
  BinFunction (..),
  SortField (..),
  SortDir (..),
  Sources (..),
  SubjectExpr (..),
  -- Section parsers
  pSummarizeSection,
  pSortSection,
  pTakeSection,
  pExtendSection,
  pProjectSection,
  parseQuery,
  transformAST,
  pSource,
  -- Aggregation parsers
  aggFunctionParser,
  namedAggregation,
  pBinFunction,
  pSummarizeByClause,
  pSortField,
  -- KQL function parsers
  pCountIf,
  pDCount,
  pCount,
  pCoalesce,
  pStrcat,
  pIff,
  pCase,
  pRound,
  pToFloat,
  pToInt,
  pToString,
  pPercentileSingle,
  pPercentilesMulti,
  pSubjectExpr,
  pScalarExpr,
  pScalarExprNested,
  -- Utilities
  defaultBinSize,
  extractPercentilesInfo,
  subjectExprToSQL,
  rewriteSectionsForSource,
) where

import Control.Lens (set, view)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Aeson qualified as AE
import Data.Generics.Product (typed)
import Data.Text qualified as T
import Data.Text.Display (Display, display, displayBuilder, displayPrec)
import Pkg.DeriveUtils (WrappedEnumSC (..))
import Pkg.Parser.Expr (Expr (..), Parser, Subject (..), ToQueryText (..), Values (..), kqlTimespanToTimeBucket, pExpr, pSubject, pValues)
import Relude hiding (GT, LT, Sum, many, some)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, hspace, space, string)
import Text.Megaparsec.Char.Lexer qualified as L


-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import Pkg.Parser.Expr (FieldKey(..))
-- >>> import Pkg.Parser.Stats
-- >>> :set -XOverloadedStrings


-- Default bin size for auto binning if not otherwise specified
defaultBinSize :: Text
defaultBinSize = "5 minutes"


-- | Comma separator with trailing space - common parser pattern
comma :: Parser ()
comma = void $ string "," *> space


-- | Helper to avoid repeating <*> pure Nothing for alias-less aggregations
withoutAlias :: Parser (Maybe Text -> a) -> Parser a
withoutAlias p = p <*> pure Nothing


-- | Simple arithmetic expression for percentile functions
-- Supports: field, field / number, field * number
data SubjectExpr = SubjectExpr Subject (Maybe (Text, Double))
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Display a SubjectExpr as SQL
subjectExprToSQL :: SubjectExpr -> Text
subjectExprToSQL (SubjectExpr sub Nothing) = display sub
subjectExprToSQL (SubjectExpr sub (Just (op, val))) = "(" <> display sub <> " " <> op <> " " <> show val <> ")"


instance ToQueryText SubjectExpr where
  toQText (SubjectExpr sub Nothing) = toQText sub
  toQText (SubjectExpr sub (Just (op, val))) = toQText sub <> " " <> op <> " " <> show val


-- | Arithmetic operators for scalar expressions
data ArithOp = Mul | Div | Add | Sub
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Scalar expression that can be a value, aggregation, or arithmetic combination
-- Enables: round(countif(...) * 100.0 / count(), 2)
data ScalarExpr = SVal Values | SAgg AggFunction | SArith ScalarExpr ArithOp ScalarExpr
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


arithOpText :: ArithOp -> Text
arithOpText = \case Mul -> " * "; Div -> " / "; Add -> " + "; Sub -> " - "


instance Display ScalarExpr where
  displayPrec p (SVal v) = displayPrec p v
  displayPrec _ (SAgg agg) = displayBuilder $ aggToSqlNoAlias agg
  displayPrec _ (SArith l Div r) = displayBuilder $ "COALESCE((" <> display l <> " / NULLIF(" <> display r <> ", 0)), 0)"
  displayPrec _ (SArith l op r) = displayBuilder $ "(" <> display l <> arithOpText op <> display r <> ")"


instance ToQueryText ScalarExpr where
  toQText (SVal v) = toQText v
  toQText (SAgg agg) = toQText agg
  toQText (SArith l op r) = toQText l <> arithOpText op <> toQText r


-- | SQL pattern for simple aggregations: fn((subject)::float)
simpleAggSQL :: Text -> Subject -> Text
simpleAggSQL fn sub = fn <> "((" <> display sub <> ")::float)"


-- | Timescale Toolkit-compatible bounded percentile aggregate.
percentileSQL :: Double -> Text -> Text
percentileSQL pct sub = "approx_percentile(" <> show pct <> ", percentile_agg(CAST(" <> sub <> " AS DOUBLE)))::float"


-- | Convert AggFunction to SQL without the AS alias (for use inside other expressions)
aggToSqlNoAlias :: AggFunction -> Text
aggToSqlNoAlias (Count sub _) = "count(" <> display sub <> ")::float"
aggToSqlNoAlias (CountIf cond _) = "COUNT(*) FILTER (WHERE " <> display cond <> ")::float"
aggToSqlNoAlias (DCount sub _ _) = "COUNT(DISTINCT " <> display sub <> ")::float"
aggToSqlNoAlias (Sum sub _) = simpleAggSQL "sum" sub
aggToSqlNoAlias (Avg sub _) = simpleAggSQL "avg" sub
aggToSqlNoAlias (Min sub _) = simpleAggSQL "min" sub
aggToSqlNoAlias (Max sub _) = simpleAggSQL "max" sub
aggToSqlNoAlias (Median sub _) = simpleAggSQL "median" sub
aggToSqlNoAlias (Stdev sub _) = simpleAggSQL "stdev" sub
aggToSqlNoAlias (Range sub _) = "(max((" <> display sub <> ")::float) - min((" <> display sub <> ")::float))"
aggToSqlNoAlias (P50 sub _) = percentileSQL 0.50 (display sub)
aggToSqlNoAlias (P75 sub _) = percentileSQL 0.75 (display sub)
aggToSqlNoAlias (P90 sub _) = percentileSQL 0.90 (display sub)
aggToSqlNoAlias (P95 sub _) = percentileSQL 0.95 (display sub)
aggToSqlNoAlias (P99 sub _) = percentileSQL 0.99 (display sub)
aggToSqlNoAlias (P100 sub _) = percentileSQL 1 (display sub)
aggToSqlNoAlias (Percentile subExpr pct _) = percentileSQL (pct / 100.0) (subjectExprToSQL subExpr)
aggToSqlNoAlias (Percentiles subExpr pcts _) = percentileSQL (fromMaybe 50.0 (listToMaybe pcts) / 100.0) (subjectExprToSQL subExpr)
aggToSqlNoAlias (Coalesce exprs _) = "COALESCE(" <> T.intercalate ", " (map display exprs) <> ")"
aggToSqlNoAlias (Strcat exprs _) = "CONCAT(" <> T.intercalate ", " (map display exprs) <> ")"
aggToSqlNoAlias (Iff cond thenVal elseVal _) = "CASE WHEN " <> display cond <> " THEN " <> display thenVal <> " ELSE " <> display elseVal <> " END"
aggToSqlNoAlias (Case pairs defaultVal _) = "CASE " <> foldMap (\(c, v) -> "WHEN " <> display c <> " THEN " <> display v <> " ") pairs <> "ELSE " <> display defaultVal <> " END"
aggToSqlNoAlias (Round val decimals _) = "ROUND((" <> display val <> ")::numeric, " <> display decimals <> ")::float"
aggToSqlNoAlias (ToFloat val _) = "(" <> display val <> ")::float"
aggToSqlNoAlias (ToInt val _) = "(" <> display val <> ")::integer"
aggToSqlNoAlias (ToString val _) = "(" <> display val <> ")::text"
aggToSqlNoAlias (Plain sub _) = display sub
aggToSqlNoAlias (ArithExpr expr _) = "(" <> display expr <> ")::float"


data AggFunction
  = Count Subject (Maybe Text) -- Optional field and alias
  | CountIf Expr (Maybe Text) -- Count with condition (KQL countif)
  | DCount Subject (Maybe Int) (Maybe Text) -- Distinct count with optional accuracy (0-4)
  | P50 Subject (Maybe Text)
  | P75 Subject (Maybe Text)
  | P90 Subject (Maybe Text)
  | P95 Subject (Maybe Text)
  | P99 Subject (Maybe Text)
  | P100 Subject (Maybe Text)
  | Percentile SubjectExpr Double (Maybe Text) -- subject expr, single percentile value, optional alias
  | Percentiles SubjectExpr [Double] (Maybe Text) -- subject expr, percentile values, optional alias
  | Sum Subject (Maybe Text)
  | Avg Subject (Maybe Text)
  | Min Subject (Maybe Text)
  | Max Subject (Maybe Text)
  | Median Subject (Maybe Text)
  | Stdev Subject (Maybe Text)
  | Range Subject (Maybe Text)
  | -- Scalar functions that can be used in aggregations (Microsoft KQL compatible)
    Coalesce [Values] (Maybe Text) -- coalesce(expr1, expr2, ...) - variadic, returns first non-null
  | Strcat [Values] (Maybe Text) -- strcat(expr1, expr2, ...) - concatenate any scalar expressions
  | Iff Expr Values Values (Maybe Text) -- iff(condition, then_expr, else_expr)
  | Case [(Expr, Values)] Values (Maybe Text) -- case(pred1, val1, pred2, val2, ..., else)
  | -- Type casting and formatting functions (Microsoft KQL compatible)
    Round ScalarExpr Values (Maybe Text) -- round(value, decimals) - value can be nested agg
  | ToFloat ScalarExpr (Maybe Text) -- tofloat(value) - convert to float
  | ToInt ScalarExpr (Maybe Text) -- toint(value) - convert to integer
  | ToString ScalarExpr (Maybe Text) -- tostring(value) - convert to text
  | Plain Subject (Maybe Text)
  | ArithExpr ScalarExpr (Maybe Text) -- Arithmetic on aggregations: count() / 5.0
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data BinFunction
  = Bin Subject Text
  | BinAuto Subject -- uses defaultBinSize
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Items that can appear in a summarize by clause
data ByClauseItem
  = BySubject Subject -- Simple field like status_code
  | ByBinFunc BinFunction -- bin_auto(timestamp) or bin(timestamp, 60)
  | ByScalarFunc AggFunction -- Scalar expressions like coalesce(tostring(x), "default")
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


newtype SummarizeByClause = SummarizeByClause [ByClauseItem]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Sources = SSpans | SMetrics
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | TimeFusion stores metric JSON columns as variants, so metric queries wrap
-- them in @variant_to_json@ before KQL's JSON-path operators are applied.
rewriteSectionsForSource :: Maybe Sources -> [Section] -> [Section]
rewriteSectionsForSource (Just SMetrics) = mapSubjects \case
  Subject entire "attributes" keys -> Subject entire "variant_to_json(attributes)" keys
  Subject entire "resource" keys -> Subject entire "variant_to_json(resource)" keys
  subject -> subject
rewriteSectionsForSource _ = id


-- | Apply a Subject rewrite to every Subject reachable from a Section list.
-- Hand-rolled because generic-lens 'types' overflows GHC's reduction depth on
-- this AST (Values/Expr mutual recursion). Any new Subject-bearing constructor
-- needs its case added here.
mapSubjects :: (Subject -> Subject) -> [Section] -> [Section]
mapSubjects f = map goSec
  where
    goSec = \case
      Search e -> Search (goE e)
      WhereClause e -> WhereClause (goE e)
      SummarizeCommand aggs by -> SummarizeCommand (map goAgg aggs) (goBy <$> by)
      ExtendCommand kvs -> ExtendCommand [(k, goAgg a) | (k, a) <- kvs]
      ProjectCommand kvs -> ProjectCommand [(k, goAgg a) | (k, a) <- kvs]
      SortCommand fs -> SortCommand [SortField (f s) d | SortField s d <- fs]
      s@TakeCommand{} -> s
      s@Source{} -> s
    goE = \case
      Eq s v -> Eq (f s) (goV v)
      NotEq s v -> NotEq (f s) (goV v)
      GT s v -> GT (f s) (goV v)
      LT s v -> LT (f s) (goV v)
      GTEq s v -> GTEq (f s) (goV v)
      LTEq s v -> LTEq (f s) (goV v)
      Regex s t -> Regex (f s) t
      In s v -> In (f s) (goV v)
      NotIn s v -> NotIn (f s) (goV v)
      Has s v -> Has (f s) (goV v)
      NotHas s v -> NotHas (f s) (goV v)
      HasAny s v -> HasAny (f s) (goV v)
      HasAll s v -> HasAll (f s) (goV v)
      Contains s v -> Contains (f s) (goV v)
      NotContains s v -> NotContains (f s) (goV v)
      StartsWith s v -> StartsWith (f s) (goV v)
      NotStartsWith s v -> NotStartsWith (f s) (goV v)
      EndsWith s v -> EndsWith (f s) (goV v)
      NotEndsWith s v -> NotEndsWith (f s) (goV v)
      Matches s t -> Matches (f s) t
      Paren e -> Paren (goE e)
      And a b -> And (goE a) (goE b)
      Or a b -> Or (goE a) (goE b)
      ValEq a b -> ValEq (goV a) (goV b)
      ValNotEq a b -> ValNotEq (goV a) (goV b)
      ValGT a b -> ValGT (goV a) (goV b)
      ValLT a b -> ValLT (goV a) (goV b)
      ValGTEq a b -> ValGTEq (goV a) (goV b)
      ValLTEq a b -> ValLTEq (goV a) (goV b)
      BoolFunc v -> BoolFunc (goV v)
    goV = \case
      Field s -> Field (f s)
      List vs -> List (map goV vs)
      ScalarFunc n args -> ScalarFunc n (map goV args)
      v -> v
    goSE (SubjectExpr s p) = SubjectExpr (f s) p
    goScalar = \case
      SVal v -> SVal (goV v)
      SAgg a -> SAgg (goAgg a)
      SArith a o b -> SArith (goScalar a) o (goScalar b)
    goAgg = \case
      Count s t -> Count (f s) t
      CountIf e t -> CountIf (goE e) t
      DCount s a t -> DCount (f s) a t
      P50 s t -> P50 (f s) t
      P75 s t -> P75 (f s) t
      P90 s t -> P90 (f s) t
      P95 s t -> P95 (f s) t
      P99 s t -> P99 (f s) t
      P100 s t -> P100 (f s) t
      Percentile se d t -> Percentile (goSE se) d t
      Percentiles se ds t -> Percentiles (goSE se) ds t
      Sum s t -> Sum (f s) t
      Avg s t -> Avg (f s) t
      Min s t -> Min (f s) t
      Max s t -> Max (f s) t
      Median s t -> Median (f s) t
      Stdev s t -> Stdev (f s) t
      Range s t -> Range (f s) t
      Coalesce vs t -> Coalesce (map goV vs) t
      Strcat vs t -> Strcat (map goV vs) t
      Iff e a b t -> Iff (goE e) (goV a) (goV b) t
      Case ps el t -> Case [(goE e, goV v) | (e, v) <- ps] (goV el) t
      Round se v t -> Round (goScalar se) (goV v) t
      ToFloat se t -> ToFloat (goScalar se) t
      ToInt se t -> ToInt (goScalar se) t
      ToString se t -> ToString (goScalar se) t
      Plain s t -> Plain (f s) t
      ArithExpr se t -> ArithExpr (goScalar se) t
    goBy (SummarizeByClause items) = SummarizeByClause (map goItem items)
    goItem = \case
      BySubject s -> BySubject (f s)
      ByBinFunc (Bin s t) -> ByBinFunc (Bin (f s) t)
      ByBinFunc (BinAuto s) -> ByBinFunc (BinAuto (f s))
      ByScalarFunc a -> ByScalarFunc (goAgg a)


-- | Parse a numeric value (float or int) for percentile values
pNumericValue :: Parser Double
pNumericValue = try L.float <|> (fromIntegral <$> (L.decimal :: Parser Int))


-- | Parse a subject expression: field, field / number, field * number
-- >>> parse pSubjectExpr "" "duration"
-- Right (SubjectExpr (Subject "duration" "duration" []) Nothing)
-- >>> parse pSubjectExpr "" "duration / 1e6"
-- Right (SubjectExpr (Subject "duration" "duration" []) (Just ("/",1000000.0)))
pSubjectExpr :: Parser SubjectExpr
pSubjectExpr = SubjectExpr <$> pSubject <*> optional ((,) <$> (space *> (string "/" <|> string "*") <* space) <*> pNumericValue)


-- | Parse percentile(expr, percentile) - single percentile (Microsoft KQL syntax)
-- >>> parse pPercentileSingle "" "percentile(duration, 95)"
-- Right (Percentile (SubjectExpr (Subject "duration" "duration" []) Nothing) 95.0 Nothing)
-- >>> parse pPercentileSingle "" "percentile(duration / 1e6, 95)"
-- Right (Percentile (SubjectExpr (Subject "duration" "duration" []) (Just ("/",1000000.0))) 95.0 Nothing)
-- >>> parse pPercentileSingle "" "percentile(duration, 150)"
-- Left ...
pPercentileSingle :: Parser AggFunction
pPercentileSingle = do
  subExpr <- string "percentile(" *> pSubjectExpr <* comma
  pct <- pNumericValue <* string ")"
  when (pct < 0 || pct > 100) $ fail "percentile value must be between 0 and 100"
  pure $ Percentile subExpr pct Nothing


-- | Parse percentiles(expr, p1, p2, ...) - multiple percentiles (Microsoft KQL syntax)
-- >>> parse pPercentilesMulti "" "percentiles(duration, 50, 75, 90, 95)"
-- Right (Percentiles (SubjectExpr (Subject "duration" "duration" []) Nothing) [50.0,75.0,90.0,95.0] Nothing)
-- >>> parse pPercentilesMulti "" "percentiles(duration / 1e6, 50, 75, 90, 95)"
-- Right (Percentiles (SubjectExpr (Subject "duration" "duration" []) (Just ("/",1000000.0))) [50.0,75.0,90.0,95.0] Nothing)
-- >>> parse pPercentilesMulti "" "percentiles(duration, 50, 150)"
-- Left ...
pPercentilesMulti :: Parser AggFunction
pPercentilesMulti = do
  subExpr <- string "percentiles(" *> pSubjectExpr <* comma
  pcts <- pNumericValue `sepBy1` comma <* string ")"
  when (any (\p -> p < 0 || p > 100) pcts) $ fail "percentile value must be between 0 and 100"
  pure $ Percentiles subExpr pcts Nothing


-- | Parse countif(predicate) - count with a filter condition
-- Microsoft KQL: countif(predicate)
-- >>> parse pCountIf "" "countif(status_code == \"ERROR\")"
-- Right (CountIf (Eq (Subject "status_code" "status_code" []) (Str "ERROR")) Nothing)
pCountIf :: Parser AggFunction
pCountIf = withoutAlias $ CountIf <$> (string "countif(" *> pExpr <* string ")")


-- | Parse dcount(expr [, accuracy]) - distinct count with optional accuracy
-- Microsoft KQL: dcount(expr [, accuracy]) where accuracy is 0-4
-- >>> parse pDCount "" "dcount(user_id)"
-- Right (DCount (Subject "user_id" "user_id" []) Nothing Nothing)
-- >>> parse pDCount "" "dcount(user_id, 2)"
-- Right (DCount (Subject "user_id" "user_id" []) (Just 2) Nothing)
-- >>> parse pDCount "" "dcount(user_id, 5)"
-- Left ...
pDCount :: Parser AggFunction
pDCount = do
  sub <- string "dcount(" *> pSubject
  accM <- optional (comma *> (L.decimal :: Parser Int)) <* string ")"
  whenJust accM \acc -> when (acc < 0 || acc > 4) $ fail "dcount accuracy must be 0-4 per KQL spec"
  pure $ DCount sub accM Nothing


-- | Parse a scalar expression (field reference or literal value)
-- Used by iff, case, coalesce, strcat for flexible value parsing
-- Field references are wrapped in Field constructor to output as column names (not quoted strings)
-- Note: try is needed because pValues has a catch-all that may consume delimiters before failing
pScalarExpr :: Parser Values
pScalarExpr = try pValues <|> (Field <$> pSubject)


-- | Base aggregation parsers shared between aggFunctionParser and pScalarExprNested
-- Excludes type-casting functions (round/tofloat/toint/tostring) to allow nesting
baseAggParsers :: [Parser AggFunction]
baseAggParsers =
  [ pSimpleAgg "sum" Sum
  , pPercentilesMulti -- percentiles commits to enforce range validation (0-100)
  , pPercentileSingle -- percentile commits to enforce range validation (0-100)
  , pSimpleAgg "p50" P50
  , pSimpleAgg "p75" P75
  , pSimpleAgg "p90" P90
  , pSimpleAgg "p95" P95
  , pSimpleAgg "p99" P99
  , pSimpleAgg "p100" P100
  , try pCountIf -- countif must come before count
  , pDCount -- dcount commits to enforce accuracy validation (0-4)
  , try pCount
  , pCoalesce -- coalesce commits to enforce minimum 2 arguments validation
  , try pStrcat
  , try pIff
  , try pCase
  , pSimpleAgg "avg" Avg
  , pSimpleAgg "min" Min
  , pSimpleAgg "max" Max
  , pSimpleAgg "median" Median
  , pSimpleAgg "stdev" Stdev
  , pSimpleAgg "range" Range
  ]


-- | Left-associative * / (tighter) then + - operator table, parameterised by the
-- whitespace parser allowed around operators.
arithTable :: Parser () -> [[Operator Parser ScalarExpr]]
arithTable sp = [[infixL '*' Mul, infixL '/' Div], [infixL '+' Add, infixL '-' Sub]]
  where
    infixL c op = InfixL $ try ((`SArith` op) <$ (sp *> char c <* sp))


-- | Parse a nested scalar expression with arithmetic support
-- Precedence: * / binds tighter than + -
-- Enables: round(countif(status >= 400) * 100.0 / count(), 2)
--
-- >>> parse pScalarExprNested "" "countif(status >= 400) * 100.0 / count()"
-- Right (SArith (SArith (SAgg (CountIf (GTEq (Subject "status" "status" []) (Num "400")) Nothing)) Mul (SVal (Num "100.0"))) Div (SAgg (Count (Subject "*" "*" []) Nothing)))
pScalarExprNested :: Parser ScalarExpr
pScalarExprNested = makeExprParser pFactor (arithTable space)
  where
    -- No try on choice baseAggParsers: validation errors propagate after consuming function input
    pFactor =
      between (char '(' <* space) (space *> char ')') pScalarExprNested
        <|> (SAgg <$> choice baseAggParsers)
        <|> (SVal <$> pScalarExpr)


-- | Parse coalesce(expr1, expr2, ...) - return first non-null (variadic, 2-64 args)
-- Microsoft KQL: coalesce(arg, arg_2, [arg_3,...])
-- >>> parse pCoalesce "" "coalesce(method, \"unknown\")"
-- Right (Coalesce [Field (Subject "method" "method" []),Str "unknown"] Nothing)
-- >>> parse pCoalesce "" "coalesce(x)"
-- Left ...
pCoalesce :: Parser AggFunction
pCoalesce = do
  args <- string "coalesce(" *> pScalarExpr `sepBy1` comma <* string ")"
  when (length args < 2) $ fail "coalesce requires at least 2 arguments"
  pure $ Coalesce args Nothing


-- | Parse strcat(expr1, expr2, ...) - string concatenation (1-64 args)
-- Microsoft KQL: strcat(argument1, argument2 [, argument3 ... ])
-- >>> parse pStrcat "" "strcat(method, \" \", url_path)"
-- Right (Strcat [Field (Subject "method" "method" []),Str " ",Field (Subject "url_path" "url_path" [])] Nothing)
pStrcat :: Parser AggFunction
pStrcat = withoutAlias $ Strcat <$> (string "strcat(" *> pScalarExpr `sepBy1` comma <* string ")")


-- | Parse iff(condition, then_expr, else_expr) - conditional expression
-- Microsoft KQL: iff(if, then, else)
-- >>> parse pIff "" "iff(status_code == \"ERROR\", \"error\", \"ok\")"
-- Right (Iff (Eq (Subject "status_code" "status_code" []) (Str "ERROR")) (Str "error") (Str "ok") Nothing)
pIff :: Parser AggFunction
pIff =
  withoutAlias
    $ Iff
    <$> (string "iff(" *> pExpr)
    <*> (comma *> pScalarExpr)
    <*> (comma *> pScalarExpr <* string ")")


-- | Parse case(pred1, val1, [pred2, val2, ...] else) - multi-branch conditional
-- Microsoft KQL: case(predicate_1, then_1, [predicate_2, then_2, ...] else)
-- >>> parse pCase "" "case(status >= 500, \"5xx\", status >= 400, \"4xx\", \"ok\")"
-- Right (Case [(GTEq (Subject "status" "status" []) (Num "500"),Str "5xx"),(GTEq (Subject "status" "status" []) (Num "400"),Str "4xx")] (Str "ok") Nothing)
pCase :: Parser AggFunction
pCase =
  withoutAlias
    $ Case
    <$> (string "case(" *> many (try $ (,) <$> pExpr <*> (comma *> pScalarExpr <* comma)))
    <*> (pScalarExpr <* string ")")


-- | Parse round(value, decimals) - round to N decimal places
-- Microsoft KQL: round(value [, decimals])
-- Supports nested aggregations: round(countif(...), 2)
-- >>> parse pRound "" "round(3.14159, 2)"
-- Right (Round (SVal (Num "3.14159")) (Num "2") Nothing)
-- >>> parse pRound "" "round(countif(status_code == \"ERROR\"), 2)"
-- Right (Round (SAgg (CountIf (Eq (Subject "status_code" "status_code" []) (Str "ERROR")) Nothing)) (Num "2") Nothing)
pRound :: Parser AggFunction
pRound = withoutAlias $ do
  val <- string "round(" *> pScalarExprNested <* comma
  decimals <- pScalarExpr
  case decimals of
    Num n
      | Just d <- readMaybe @Int (toString n) -> when (d < 0 || d > 15) $ fail "round() decimals must be 0-15 per PostgreSQL limit"
      | otherwise -> fail "round() decimals must be a valid integer"
    _ -> fail "round() decimals must be numeric"
  Round val decimals <$ string ")"


-- | Helper for type casting parsers with keyword aliases
-- Supports nested aggregations: tofloat(count()), toint(sum(field))
pTypeCast :: [Text] -> (ScalarExpr -> Maybe Text -> AggFunction) -> Parser AggFunction
pTypeCast keywords ctor = withoutAlias $ ctor <$> (asum (map (\k -> string (k <> "(")) keywords) *> pScalarExprNested <* string ")")


-- | Parse tofloat(value) / todouble(value) - convert to float
-- Supports nested aggregations: tofloat(count())
-- >>> parse pToFloat "" "tofloat(count_)"
-- Right (ToFloat (SVal (Field (Subject "count_" "count_" []))) Nothing)
-- >>> parse pToFloat "" "tofloat(count())"
-- Right (ToFloat (SAgg (Count (Subject "*" "*" []) Nothing)) Nothing)
pToFloat :: Parser AggFunction
pToFloat = pTypeCast ["tofloat", "todouble"] ToFloat


-- | Parse toint(value) / tolong(value) - convert to integer
-- Supports nested aggregations: toint(sum(field))
-- >>> parse pToInt "" "toint(duration)"
-- Right (ToInt (SVal (Field (Subject "duration" "duration" []))) Nothing)
pToInt :: Parser AggFunction
pToInt = pTypeCast ["toint", "tolong"] ToInt


-- | Parse tostring(value) - convert to text
-- >>> parse pToString "" "tostring(status_code)"
-- Right (ToString (SVal (Field (Subject "status_code" "status_code" []))) Nothing)
pToString :: Parser AggFunction
pToString = pTypeCast ["tostring"] ToString


-- | Helper for simple function(subject) parsers
pSimpleAgg :: Text -> (Subject -> Maybe Text -> AggFunction) -> Parser AggFunction
pSimpleAgg name ctor = withoutAlias $ ctor <$> (string (name <> "(") *> pSubject <* string ")")


-- | Parse count() or count(field) - handles both empty parens and field reference
pCount :: Parser AggFunction
pCount =
  withoutAlias
    $ Count
    <$> (string "count(" *> ((pSubject <* string ")") <|> (Subject "*" "*" [] <$ string ")")))


-- | Handles different aggregation functions and their optional aliases.
--
-- >>> parse aggFunctionParser "" "count(field)"
-- Right (Count (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "count()"
-- Right (Count (Subject "*" "*" []) Nothing)
--
-- >>> parse aggFunctionParser "" "sum(field)"
-- Right (Sum (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "avg(field)"
-- Right (Avg (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "min(field)"
-- Right (Min (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "max(field)"
-- Right (Max (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "median(field)"
-- Right (Median (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "stdev(field)"
-- Right (Stdev (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "range(field)"
-- Right (Range (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "p50(field)"
-- Right (P50 (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "customFunc(field)"
-- Right (Plain (Subject "customFunc" "customFunc" []) Nothing)
aggFunctionParser :: Parser AggFunction
aggFunctionParser =
  choice @[]
    $ baseAggParsers
    ++ [ pRound -- round commits to enforce numeric decimals validation
       , try pToFloat
       , try pToInt
       , try pToString
       , withoutAlias $ Plain <$> pSubject
       ]


-- | Convert AggFunction to KQL syntax (NOT SQL - use Display for SQL)
instance ToQueryText AggFunction where
  toQText (Count (Subject "*" _ _) _) = "count()"
  toQText (Count sub _) = "count(" <> toQText sub <> ")"
  toQText (CountIf cond _) = "countif(" <> toQText cond <> ")"
  toQText (DCount sub Nothing _) = "dcount(" <> toQText sub <> ")"
  toQText (DCount sub (Just acc) _) = "dcount(" <> toQText sub <> ", " <> show acc <> ")"
  toQText (P50 sub _) = "p50(" <> toQText sub <> ")"
  toQText (P75 sub _) = "p75(" <> toQText sub <> ")"
  toQText (P90 sub _) = "p90(" <> toQText sub <> ")"
  toQText (P95 sub _) = "p95(" <> toQText sub <> ")"
  toQText (P99 sub _) = "p99(" <> toQText sub <> ")"
  toQText (P100 sub _) = "p100(" <> toQText sub <> ")"
  toQText (Percentile subExpr pct _) = "percentile(" <> toQText subExpr <> ", " <> show pct <> ")"
  toQText (Percentiles subExpr pcts _) = "percentiles(" <> toQText subExpr <> ", " <> T.intercalate ", " (map show pcts) <> ")"
  toQText (Sum sub _) = "sum(" <> toQText sub <> ")"
  toQText (Avg sub _) = "avg(" <> toQText sub <> ")"
  toQText (Min sub _) = "min(" <> toQText sub <> ")"
  toQText (Max sub _) = "max(" <> toQText sub <> ")"
  toQText (Median sub _) = "median(" <> toQText sub <> ")"
  toQText (Stdev sub _) = "stdev(" <> toQText sub <> ")"
  toQText (Range sub _) = "range(" <> toQText sub <> ")"
  toQText (Coalesce exprs _) = "coalesce(" <> T.intercalate ", " (map toQText exprs) <> ")"
  toQText (Strcat exprs _) = "strcat(" <> T.intercalate ", " (map toQText exprs) <> ")"
  toQText (Iff cond thenVal elseVal _) = "iff(" <> toQText cond <> ", " <> toQText thenVal <> ", " <> toQText elseVal <> ")"
  toQText (Case pairs defVal _) = "case(" <> T.intercalate ", " (concatMap (\(c, v) -> [toQText c, toQText v]) pairs) <> ", " <> toQText defVal <> ")"
  toQText (Round val decimals _) = "round(" <> toQText val <> ", " <> toQText decimals <> ")"
  toQText (ToFloat val _) = "tofloat(" <> toQText val <> ")"
  toQText (ToInt val _) = "toint(" <> toQText val <> ")"
  toQText (ToString val _) = "tostring(" <> toQText val <> ")"
  toQText (Plain sub _) = toQText sub
  toQText (ArithExpr expr _) = toQText expr


instance ToQueryText [AggFunction] where
  toQText xs = T.intercalate ", " $ map toQText xs


-- | Sanitize field name for use as SQL alias (replace dots with underscores)
sanitizeAlias :: Text -> Text
sanitizeAlias = T.replace "." "_"


-- | Get default alias for aggregation (e.g., count_, sum_field)
defaultAlias :: AggFunction -> Text
defaultAlias = \case
  Count (Subject n _ _) _ -> "count_" <> if n == "*" then "" else sanitizeAlias n
  CountIf{} -> "countif_"
  DCount (Subject n _ _) _ _ -> "dcount_" <> sanitizeAlias n
  P50 (Subject n _ _) _ -> "p50_" <> sanitizeAlias n
  P75 (Subject n _ _) _ -> "p75_" <> sanitizeAlias n
  P90 (Subject n _ _) _ -> "p90_" <> sanitizeAlias n
  P95 (Subject n _ _) _ -> "p95_" <> sanitizeAlias n
  P99 (Subject n _ _) _ -> "p99_" <> sanitizeAlias n
  P100 (Subject n _ _) _ -> "p100_" <> sanitizeAlias n
  Percentile (SubjectExpr (Subject n _ _) _) pct _ -> "percentile_" <> sanitizeAlias n <> "_" <> sanitizeAlias (show pct)
  Percentiles (SubjectExpr (Subject n _ _) _) _ _ -> "percentiles_" <> sanitizeAlias n
  Sum (Subject n _ _) _ -> "sum_" <> sanitizeAlias n
  Avg (Subject n _ _) _ -> "avg_" <> sanitizeAlias n
  Min (Subject n _ _) _ -> "min_" <> sanitizeAlias n
  Max (Subject n _ _) _ -> "max_" <> sanitizeAlias n
  Median (Subject n _ _) _ -> "median_" <> sanitizeAlias n
  Stdev (Subject n _ _) _ -> "stdev_" <> sanitizeAlias n
  Range (Subject n _ _) _ -> "range_" <> sanitizeAlias n
  Coalesce{} -> "coalesce_"
  Strcat{} -> "strcat_"
  Iff{} -> "iff_"
  Case{} -> "case_"
  Round{} -> "round_"
  ToFloat{} -> "tofloat_"
  ToInt{} -> "toint_"
  ToString{} -> "tostring_"
  Plain (Subject n _ _) _ -> sanitizeAlias n
  ArithExpr{} -> "arith_"


-- | Display with alias: SQL AS (explicit or default)
instance Display AggFunction where
  displayPrec _ agg = displayBuilder $ aggToSqlNoAlias agg <> " AS " <> fromMaybe (defaultAlias agg) (view (typed @(Maybe Text)) agg)


instance ToQueryText Section where
  toQText (Source source) = toQText source
  toQText (Search expr) = toQText expr
  toQText (WhereClause expr) = "where " <> toQText expr
  toQText (SummarizeCommand funcs byClauseM) =
    "summarize "
      <> T.intercalate "," (map toQText funcs)
      <> maybeToMonoid (toQText <$> byClauseM)
  toQText (ExtendCommand cols) =
    "extend " <> T.intercalate ", " [name <> " = " <> toQText expr | (name, expr) <- cols]
  toQText (ProjectCommand cols) =
    "project " <> T.intercalate ", " [name <> " = " <> toQText expr | (name, expr) <- cols]
  toQText (SortCommand fields) = "sort by " <> T.intercalate ", " (map toQText fields)
  toQText (TakeCommand limit) = "take " <> show limit


-- | Sort direction; encodes to "asc"/"desc".
data SortDir = Asc | Desc
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON, Display) via WrappedEnumSC 'Nothing "" SortDir


data SortField = SortField Subject (Maybe SortDir) -- field and optional direction
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Section
  = Search Expr
  | WhereClause Expr -- New constructor for where clause after pipe
  | SummarizeCommand [AggFunction] (Maybe SummarizeByClause)
  | ExtendCommand [(Text, AggFunction)] -- extend col = expr, col2 = expr2 (add computed columns)
  | ProjectCommand [(Text, AggFunction)] -- project col = expr, col2 = expr2 (select specific columns)
  | SortCommand [SortField] -- sort by multiple fields
  | TakeCommand Int -- limit/take number of results
  | Source Sources
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Parse a bin function call like 'bin(timestamp, 60)' or 'bin_auto(timestamp)'
--
-- >>> parse pBinFunction "" "bin(timestamp, 60)"
-- Right (Bin (Subject "timestamp" "timestamp" []) "60")
--
-- >>> parse pBinFunction "" "bin_auto(timestamp)"
-- Right (BinAuto (Subject "timestamp" "timestamp" []))
pBinFunction :: Parser BinFunction
pBinFunction =
  -- interval supports time duration units (30m, 1h, …) or plain numbers (seconds)
  try (Bin <$> (string "bin(" *> pSubject <* string "," <* space) <*> (toText <$> some (alphaNumChar <|> char '.') <* string ")"))
    <|> (BinAuto <$> (string "bin_auto(" *> pSubject <* string ")"))


instance ToQueryText BinFunction where
  toQText (Bin subj interval) = "bin(" <> toQText subj <> ", " <> interval <> ")"
  toQText (BinAuto subj) = "bin_auto(" <> toQText subj <> ")"


instance Display BinFunction where
  displayPrec _prec (Bin subj interval) = displayBuilder $ "time_bucket('" <> kqlTimespanToTimeBucket interval <> "', " <> display subj <> ")"
  -- No "as bin_timestamp" here: it causes syntax errors in GROUP BY
  displayPrec _prec (BinAuto subj) = displayBuilder $ "time_bucket('" <> defaultBinSize <> "', " <> display subj <> ")"


instance Display ByClauseItem where
  displayPrec p (BySubject s) = displayPrec p s
  displayPrec p (ByBinFunc b) = displayPrec p b
  displayPrec _ (ByScalarFunc f) = displayBuilder $ aggToSqlNoAlias f


-- | Parse a summarize by clause which can contain fields, bin functions, and scalar expressions
--
-- >>> parse pSummarizeByClause "" "by attributes.client, bin(timestamp, 60)"
-- Right (SummarizeByClause [BySubject (Subject "attributes.client" "attributes" [FieldKey "client"]),ByBinFunc (Bin (Subject "timestamp" "timestamp" []) "60")])
--
-- >>> parse pSummarizeByClause "" "by Computer, bin_auto(timestamp)"
-- Right (SummarizeByClause [BySubject (Subject "Computer" "Computer" []),ByBinFunc (BinAuto (Subject "timestamp" "timestamp" []))])
--
-- >>> parse pSummarizeByClause "" "by bin_auto(timestamp)"
-- Right (SummarizeByClause [ByBinFunc (BinAuto (Subject "timestamp" "timestamp" []))])
--
-- >>> parse pSummarizeByClause "" "by bin_auto(timestamp), coalesce(tostring(status), \"unknown\")"
-- Right (SummarizeByClause [ByBinFunc (BinAuto (Subject "timestamp" "timestamp" [])),ByScalarFunc (Coalesce [ScalarFunc "tostring" [Field (Subject "status" "status" [])],Str "unknown"] Nothing)])
pSummarizeByClause :: Parser SummarizeByClause
pSummarizeByClause = SummarizeByClause <$> (string "by" *> space *> pByClauseItem `sepBy` comma)
  where
    -- Order matters: bin functions, then actual scalar functions (with parens), then plain subjects
    pByClauseItem = choice @[] [try (ByBinFunc <$> pBinFunction), try (ByScalarFunc <$> pScalarFuncOnly), BySubject <$> pSubject]
    -- Scalar-only parsers for by clause (not aggregations like sum/count/avg)
    pScalarFuncOnly = choice @[] [try pCoalesce, try pStrcat, try pIff, try pCase, try pRound, try pToFloat, try pToInt, try pToString]


instance ToQueryText SummarizeByClause where
  toQText (SummarizeByClause items) = "by " <> T.intercalate ", " (map toQText items)


instance ToQueryText ByClauseItem where
  toQText (BySubject s) = toQText s
  toQText (ByBinFunc b) = toQText b
  toQText (ByScalarFunc f) = toQText f


-- | Parse a sort field that can include an optional direction
--
-- >>> parse pSortField "" "parent_id asc"
-- Right (SortField (Subject "parent_id" "parent_id" []) (Just Asc))
--
-- >>> parse pSortField "" "timestamp"
-- Right (SortField (Subject "timestamp" "timestamp" []) Nothing)
pSortField :: Parser SortField
pSortField = SortField <$> pSubject <*> optional (try (space *> ((Asc <$ string "asc") <|> (Desc <$ string "desc"))))


instance ToQueryText SortField where
  toQText (SortField field Nothing) = toQText field
  toQText (SortField field (Just dir)) = toQText field <> " " <> display dir


-- | Parser for 'sort by' command (also supports 'order by' synonym)
--
-- >>> parse pSortSection "" "sort by parent_id asc, timestamp desc"
-- Right (SortCommand [SortField (Subject "parent_id" "parent_id" []) (Just Asc),SortField (Subject "timestamp" "timestamp" []) (Just Desc)])
--
-- >>> parse pSortSection "" "order by parent_id asc"
-- Right (SortCommand [SortField (Subject "parent_id" "parent_id" []) (Just Asc)])
pSortSection :: Parser Section
pSortSection = SortCommand <$> ((string "sort by" <|> string "order by") *> space *> sepBy pSortField comma)


-- | Parser for 'take' command (also supports 'limit' synonym)
--
-- >>> parse pTakeSection "" "take 1000"
-- Right (TakeCommand 1000)
--
-- >>> parse pTakeSection "" "limit 500"
-- Right (TakeCommand 500)
pTakeSection :: Parser Section
pTakeSection = TakeCommand <$> ((string "take" <|> string "limit") *> space *> L.decimal)


-- | Parse 'name = expression' pattern, returns (name, aliased expression)
-- Used by extend/project commands and named aggregations in summarize
-- Allows dots in names which are sanitized to underscores for valid SQL aliases
-- NOTE: try is only on the prefix "name =" so validation errors propagate
pNamedExpr :: Parser (Text, AggFunction)
pNamedExpr = do
  name <- try $ toText <$> some (alphaNumChar <|> oneOf ("_." :: String)) <* space <* string "=" <* space
  let sanitized = sanitizeAlias name
  -- generic-lens sets the Maybe Text alias field of whichever constructor is parsed
  (sanitized,) . set (typed @(Maybe Text)) (Just sanitized) <$> aggFunctionParser


-- | Helper for column-assignment commands (extend/project)
pColumnCommand :: Text -> ([(Text, AggFunction)] -> Section) -> Parser Section
pColumnCommand keyword ctor = string keyword *> space *> (ctor <$> pNamedExpr `sepBy1` comma)


-- | Parser for 'extend' command - add computed columns
-- Microsoft KQL: extend ColumnName = Expression [, ...]
--
-- >>> parse pExtendSection "" "extend error_cat = case(status >= 500, \"5xx\", \"ok\")"
-- Right (ExtendCommand [("error_cat",Case [(GTEq (Subject "status" "status" []) (Num "500"),Str "5xx")] (Str "ok") (Just "error_cat"))])
pExtendSection :: Parser Section
pExtendSection = pColumnCommand "extend" ExtendCommand


-- | Parser for 'project' command - select specific columns
-- Microsoft KQL: project ColumnName = Expression [, ...]
--
-- >>> parse pProjectSection "" "project service = resource.service.name, count = count()"
-- Right (ProjectCommand [("service",Plain (Subject "resource.service.name" "resource" [FieldKey "service",FieldKey "name"]) (Just "service")),("count",Count (Subject "*" "*" []) (Just "count"))])
pProjectSection :: Parser Section
pProjectSection = pColumnCommand "project" ProjectCommand


-- | Parse a named aggregation like 'TotalCount = count()'
--
-- >>> parse namedAggregation "" "TotalCount=count()"
-- Right (Count (Subject "*" "*" []) (Just "TotalCount"))
--
-- >>> parse namedAggregation "" "error_count = countif(status_code == \"ERROR\")"
-- Right (CountIf (Eq (Subject "status_code" "status_code" []) (Str "ERROR")) (Just "error_count"))
namedAggregation :: Parser AggFunction
namedAggregation = snd <$> pNamedExpr


-- | Parse the summarize command.
--
-- Syntax: summarize [<name>=]<agg>"("<field>")" ["," ...] [<by-clause>]
-- for example ... | summarize sum_bytes=sum(bytes) by bin_auto(timestamp)
--
-- >>> parse pSummarizeSection "" "summarize sum(attributes.client) by attributes.client, bin(timestamp, 60)"
-- Right (SummarizeCommand [Sum (Subject "attributes.client" "attributes" [FieldKey "client"]) Nothing] (Just (SummarizeByClause [BySubject (Subject "attributes.client" "attributes" [FieldKey "client"]),ByBinFunc (Bin (Subject "timestamp" "timestamp" []) "60")])))
--
-- >>> parse pSummarizeSection "" "summarize TotalCount=count() by Computer"
-- Right (SummarizeCommand [Count (Subject "*" "*" []) (Just "TotalCount")] (Just (SummarizeByClause [BySubject (Subject "Computer" "Computer" [])])))
--
-- >>> parse pSummarizeSection "" "summarize count(*) by bin_auto(timestamp)"
-- Right (SummarizeCommand [Count (Subject "*" "*" []) Nothing] (Just (SummarizeByClause [ByBinFunc (BinAuto (Subject "timestamp" "timestamp" []))])))
--
-- >>> parse pSummarizeSection "" "summarize count() / 5.0 by bin_auto(timestamp)"
-- Right (SummarizeCommand [ArithExpr (SArith (SAgg (Count (Subject "*" "*" []) Nothing)) Div (SVal (Num "5.0"))) Nothing] (Just (SummarizeByClause [ByBinFunc (BinAuto (Subject "timestamp" "timestamp" []))])))
--
-- >>> parse pSummarizeSection "" "summarize count() * 100 / sum(total) by status"
-- Right (SummarizeCommand [ArithExpr (SArith (SArith (SAgg (Count (Subject "*" "*" []) Nothing)) Mul (SVal (Num "100"))) Div (SAgg (Sum (Subject "total" "total" []) Nothing))) Nothing] (Just (SummarizeByClause [BySubject (Subject "status" "status" [])])))
--
-- Operator precedence test: * binds tighter than +
-- count() + 5 * 10 should parse as count() + (5 * 10), not (count() + 5) * 10
-- >>> parse pSummarizeSection "" "summarize count() + 5 * 10 by status"
-- Right (SummarizeCommand [ArithExpr (SArith (SAgg (Count (Subject "*" "*" []) Nothing)) Add (SArith (SVal (Num "5")) Mul (SVal (Num "10")))) Nothing] (Just (SummarizeByClause [BySubject (Subject "status" "status" [])])))
--
-- Regression test: percentiles with by clause (requires try in arithmetic operators)
-- >>> parse pSummarizeSection "" "summarize percentiles(duration, 50, 75, 90, 95, 99) by bin_auto(timestamp)"
-- Right (SummarizeCommand [Percentiles (SubjectExpr (Subject "duration" "duration" []) Nothing) [50.0,75.0,90.0,95.0,99.0] Nothing] (Just (SummarizeByClause [ByBinFunc (BinAuto (Subject "timestamp" "timestamp" []))])))
--
-- >>> parse pSummarizeSection "" "summarize count()"
-- Right (SummarizeCommand [Count (Subject "*" "*" []) Nothing] Nothing)
pSummarizeSection :: Parser Section
pSummarizeSection = do
  _ <- string "summarize"
  space
  -- namedAggregation has try only on prefix "name =" so validation errors propagate
  funcs <- sepBy (namedAggregation <|> (unwrapAgg <$> pArithExpr)) (char ',' <* space)
  SummarizeCommand funcs <$> optional (try (space *> pSummarizeByClause))
  where
    unwrapAgg = \case
      SAgg agg -> agg
      expr -> ArithExpr expr Nothing
    pArithExpr :: Parser ScalarExpr
    pArithExpr = makeExprParser pArithTerm (arithTable hspace)

    -- try on pNumericOnly is fine; aggFunctionParser parsers use try on prefix only
    -- so validation errors propagate after consuming function input
    pArithTerm = choice @[] [SVal <$> try pNumericOnly, SAgg <$> aggFunctionParser, char '(' *> pArithExpr <* char ')']

    pNumericOnly = Num . toText <$> ((<>) <$> some digitChar <*> option "" (try ((:) <$> char '.' <*> some digitChar)))


-- | Parser for where clause section
pWhereSection :: Parser Section
pWhereSection = WhereClause <$> (string "where" *> space *> pExpr)


pSection :: Parser Section
pSection =
  space
    *> choice @[]
      [ pSummarizeSection
      , pSortSection
      , pTakeSection
      , try pExtendSection
      , try pProjectSection
      , try pWhereSection -- Try to parse 'where' clause first
      , try (Source <$> pSource) -- Try source before bare expressions
      , Search <$> (notFollowedBy reservedKeyword *> pExpr) -- Fall back to bare expression for backward compatibility
      ]
  where
    reservedKeyword = choice @[] $ map (\k -> string k *> space) ["project", "extend", "summarize", "sort", "take", "where"]


-- find what source to use when processing a query. By default, the requests source is used
--
-- >>> parse pSource "" "spans"
-- Right SSpans
--
-- >>> parse pSource "" "metrics"
-- Right SMetrics
--
pSource :: Parser Sources
pSource =
  choice @[]
    [ SSpans <$ (string "spans" <|> string "otlp_logs_and_spans")
    , SMetrics <$ (string "metrics" <|> string "telemetry.metrics")
    ]


instance ToQueryText Sources where
  toQText SSpans = "spans"
  toQText SMetrics = "metrics"


instance Display Sources where
  displayPrec _prec SSpans = "otel_logs_and_spans"
  displayPrec _prec SMetrics = "otel_metrics"


-- | Parse a full query: an optional leading pipe, then pipe-separated sections.
--
-- >>> parse parseQuery "" "| summarize count(*) by bin_auto(timestamp)"
-- Right [SummarizeCommand [Count (Subject "*" "*" []) Nothing] (Just (SummarizeByClause [ByBinFunc (BinAuto (Subject "timestamp" "timestamp" []))]))]
parseQuery :: Parser [Section]
parseQuery = do
  _ <- optional (space *> char '|' <* space)
  sections <- sepBy pSection (space *> char '|' <* space)
  space *> eof
  pure $ transformAST sections


-- | Transform AST to normalize and optimize the query structure.
-- Combines consecutive WHERE/Search clauses and reorders sections to canonical order.
--
-- >>> transformAST [Search (Eq (Subject "a" "a" []) (Str "1")), Search (Eq (Subject "b" "b" []) (Str "2"))]
-- [Search (And (Eq (Subject "a" "a" []) (Str "1")) (Eq (Subject "b" "b" []) (Str "2")))]
--
-- >>> transformAST [WhereClause (Eq (Subject "a" "a" []) (Str "1")), WhereClause (Eq (Subject "b" "b" []) (Str "2"))]
-- [WhereClause (And (Eq (Subject "a" "a" []) (Str "1")) (Eq (Subject "b" "b" []) (Str "2")))]
transformAST :: [Section] -> [Section]
transformAST = reorderSections . combineWheres
  where
    -- Combine consecutive Search or WhereClause sections into single And expressions
    combineWheres :: [Section] -> [Section]
    combineWheres [] = []
    combineWheres (Search e1 : Search e2 : rest) = combineWheres (Search (And e1 e2) : rest)
    combineWheres (WhereClause e1 : WhereClause e2 : rest) = combineWheres (WhereClause (And e1 e2) : rest)
    combineWheres (x : rest) = x : combineWheres rest

    -- Canonical order (respects state dependencies), stable within each group:
    -- Source -> filters -> extends -> summarize -> project -> sort -> take
    reorderSections :: [Section] -> [Section]
    reorderSections = sortOn \case
      Source{} -> 0 :: Int
      Search{} -> 1
      WhereClause{} -> 2
      ExtendCommand{} -> 3
      SummarizeCommand{} -> 4
      ProjectCommand{} -> 5
      SortCommand{} -> 6
      TakeCommand{} -> 7


instance ToQueryText [Section] where
  toQText secs = T.intercalate " | " $ map toQText secs


-- | Extract percentiles info from a list of sections
-- Returns (subjectExpr as SQL, percentiles) if a Percentile(s) aggregation is found
extractPercentilesInfo :: [Section] -> Maybe (Text, [Double])
extractPercentilesInfo secs = listToMaybe $ mapMaybe pcts [agg | SummarizeCommand aggs _ <- secs, agg <- aggs]
  where
    pcts = \case
      Percentile subExpr pct _ -> Just (subjectExprToSQL subExpr, [pct])
      Percentiles subExpr ps _ -> Just (subjectExprToSQL subExpr, ps)
      _ -> Nothing
