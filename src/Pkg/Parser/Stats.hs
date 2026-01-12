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
) where

import Control.Lens (set, view)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Aeson qualified as AE
import Data.Generics.Product (typed)
import Data.Text qualified as T
import Data.Text.Display (Display, display, displayBuilder, displayPrec)
import Pkg.Parser.Expr (Expr (..), Parser, Subject (..), ToQueryText (..), Values (..), kqlTimespanToTimeBucket, pExpr, pSubject, pValues)
import Relude hiding (Sum, many, some)
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


arithOpToSQL :: ArithOp -> Text
arithOpToSQL = \case Mul -> " * "; Div -> " / "; Add -> " + "; Sub -> " - "


instance Display ScalarExpr where
  displayPrec p (SVal v) = displayPrec p v
  displayPrec _ (SAgg agg) = displayBuilder $ aggToSqlNoAlias agg
  displayPrec _ (SArith l Div r) = displayBuilder $ "(" <> display l <> " / NULLIF(" <> display r <> ", 0))"
  displayPrec _ (SArith l op r) = displayBuilder $ "(" <> display l <> arithOpToSQL op <> display r <> ")"


arithOpToKQL :: ArithOp -> Text
arithOpToKQL = \case Mul -> " * "; Div -> " / "; Add -> " + "; Sub -> " - "


instance ToQueryText ScalarExpr where
  toQText (SVal v) = toQText v
  toQText (SAgg agg) = toQText agg
  toQText (SArith l op r) = toQText l <> arithOpToKQL op <> toQText r


-- | SQL pattern for simple aggregations: fn((subject)::float)
simpleAggSQL :: Text -> Subject -> Text
simpleAggSQL fn sub = fn <> "((" <> display sub <> ")::float)"


-- | SQL pattern for percentiles: approx_percentile(pct, percentile_agg((subject)::float))::float
percentileSQL :: Double -> Subject -> Text
percentileSQL pct sub = "approx_percentile(" <> show pct <> ", percentile_agg((" <> display sub <> ")::float))::float"


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
aggToSqlNoAlias (Range sub _) = simpleAggSQL "range" sub
aggToSqlNoAlias (P50 sub _) = percentileSQL 0.50 sub
aggToSqlNoAlias (P75 sub _) = percentileSQL 0.75 sub
aggToSqlNoAlias (P90 sub _) = percentileSQL 0.90 sub
aggToSqlNoAlias (P95 sub _) = percentileSQL 0.95 sub
aggToSqlNoAlias (P99 sub _) = percentileSQL 0.99 sub
aggToSqlNoAlias (P100 sub _) = percentileSQL 1 sub
aggToSqlNoAlias (Percentile subExpr pct _) = "approx_percentile(" <> show (pct / 100.0) <> ", percentile_agg((" <> subjectExprToSQL subExpr <> ")::float))::float"
aggToSqlNoAlias (Percentiles subExpr pcts _) = let firstPct = fromMaybe 50.0 (listToMaybe pcts) in "approx_percentile(" <> show (firstPct / 100.0) <> ", percentile_agg((" <> subjectExprToSQL subExpr <> ")::float))::float"
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


-- Modify Aggregation Functions to include optional aliases
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
  | -- | CustomAgg String [Field] (Maybe String)
    -- Scalar functions that can be used in aggregations (Microsoft KQL compatible)
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


-- Added to support bin() and bin_auto() functions in by clause
data BinFunction
  = Bin Subject Text
  | BinAuto Subject -- Will use fixed interval or legacy rollup calculation
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Items that can appear in a summarize by clause
data ByClauseItem
  = BySubject Subject -- Simple field like status_code
  | ByBinFunc BinFunction -- bin_auto(timestamp) or bin(timestamp, 60)
  | ByScalarFunc AggFunction -- Scalar expressions like coalesce(tostring(x), "default")
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Support for summarize by with bin() function and scalar expressions
newtype SummarizeByClause = SummarizeByClause [ByClauseItem]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Sources = SSpans | SMetrics
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Syntax:
--
-- summarize
-- <aggregation> ...
-- [<by-clause>]
--
-- aggregation
-- Syntax: [<name>=]<aggregate-function>"("<field>) ["," [<name>=]<aggregate-function>"("<field>)]...
-- for example ... | summarize sum_bytes=sum(bytes)

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

-- | Parse a numeric value (float or int) for percentile values
pNumericValue :: Parser Double
pNumericValue = try L.float <|> (fromIntegral <$> (L.decimal :: Parser Int))


-- | Parse a subject expression: field, field / number, field * number
-- >>> parse pSubjectExpr "" "duration"
-- Right (SubjectExpr (Subject "duration" "duration" []) Nothing)
-- >>> parse pSubjectExpr "" "duration / 1e6"
-- Right (SubjectExpr (Subject "duration" "duration" []) (Just ("/",1000000.0)))
pSubjectExpr :: Parser SubjectExpr
pSubjectExpr = do
  subj <- pSubject
  opM <- optional $ do
    space
    op <- (string "/" $> "/") <|> (string "*" $> "*")
    space
    val <- pNumericValue
    pure (op, val)
  pure $ SubjectExpr subj opM


-- | Parse percentile(expr, percentile) - single percentile (Microsoft KQL syntax)
-- >>> parse pPercentileSingle "" "percentile(duration, 95)"
-- Right (Percentile (SubjectExpr (Subject "duration" "duration" []) Nothing) 95.0 Nothing)
-- >>> parse pPercentileSingle "" "percentile(duration / 1e6, 95)"
-- Right (Percentile (SubjectExpr (Subject "duration" "duration" []) (Just ("/",1000000.0))) 95.0 Nothing)
-- >>> parse pPercentileSingle "" "percentile(duration, 150)"
-- Left ...
pPercentileSingle :: Parser AggFunction
pPercentileSingle = do
  _ <- string "percentile("
  subExpr <- pSubjectExpr
  _ <- comma
  pct <- pNumericValue
  _ <- string ")"
  if pct < 0 || pct > 100
    then fail "percentile value must be between 0 and 100"
    else pure $ Percentile subExpr pct Nothing


-- | Parse percentiles(expr, p1, p2, ...) - multiple percentiles (Microsoft KQL syntax)
-- >>> parse pPercentilesMulti "" "percentiles(duration, 50, 75, 90, 95)"
-- Right (Percentiles (SubjectExpr (Subject "duration" "duration" []) Nothing) [50.0,75.0,90.0,95.0] Nothing)
-- >>> parse pPercentilesMulti "" "percentiles(duration / 1e6, 50, 75, 90, 95)"
-- Right (Percentiles (SubjectExpr (Subject "duration" "duration" []) (Just ("/",1000000.0))) [50.0,75.0,90.0,95.0] Nothing)
-- >>> parse pPercentilesMulti "" "percentiles(duration, 50, 150)"
-- Left ...
pPercentilesMulti :: Parser AggFunction
pPercentilesMulti = do
  _ <- string "percentiles("
  subExpr <- pSubjectExpr
  _ <- comma
  pcts <- pNumericValue `sepBy1` comma
  _ <- string ")"
  if any (\p -> p < 0 || p > 100) pcts
    then fail "percentile value must be between 0 and 100"
    else pure $ Percentiles subExpr pcts Nothing


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
  _ <- string "dcount("
  sub <- pSubject
  accM <- optional (comma *> (L.decimal :: Parser Int))
  _ <- string ")"
  case accM of
    Just acc | acc < 0 || acc > 4 -> fail "dcount accuracy must be 0-4 per KQL spec"
    _ -> pure $ DCount sub accM Nothing


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


-- | Left-associative chain combinator for expression parsing
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest where rest x = (op <*> pure x <*> p >>= rest) <|> pure x


-- | Parse a nested scalar expression with arithmetic support
-- Precedence: * / binds tighter than + -
-- Enables: round(countif(status >= 400) * 100.0 / count(), 2)
--
-- >>> parse pScalarExprNested "" "countif(status >= 400) * 100.0 / count()"
-- Right (SArith (SArith (SAgg (CountIf (GTEq (Subject "status" "status" []) (Num "400")) Nothing)) Mul (SVal (Num "100.0"))) Div (SAgg (Count (Subject "*" "*" []) Nothing)))
pScalarExprNested :: Parser ScalarExpr
pScalarExprNested = pAddSub
  where
    pAddSub = chainl1 pMulDiv (space *> (pOp '+' Add <|> pOp '-' Sub) <* space)
    pMulDiv = chainl1 pFactor (space *> (pOp '*' Mul <|> pOp '/' Div) <* space)
    -- No try on choice baseAggParsers: validation errors propagate after consuming function input
    pFactor =
      between (char '(' <* space) (space *> char ')') pAddSub
        <|> SAgg
        <$> choice baseAggParsers
        <|> SVal
        <$> pScalarExpr
    pOp c op = (`SArith` op) <$ char c


-- | Helper for variadic scalar functions like coalesce/strcat
pVariadicAgg :: Text -> ([Values] -> Maybe Text -> AggFunction) -> Parser AggFunction
pVariadicAgg name ctor =
  withoutAlias
    $ ctor
    <$> (string (name <> "(") *> pScalarExpr `sepBy1` comma <* string ")")


-- | Parse coalesce(expr1, expr2, ...) - return first non-null (variadic, 2-64 args)
-- Microsoft KQL: coalesce(arg, arg_2, [arg_3,...])
-- >>> parse pCoalesce "" "coalesce(method, \"unknown\")"
-- Right (Coalesce [Field (Subject "method" "method" []),Str "unknown"] Nothing)
-- >>> parse pCoalesce "" "coalesce(x)"
-- Left ...
pCoalesce :: Parser AggFunction
pCoalesce = do
  _ <- string "coalesce("
  args <- pScalarExpr `sepBy1` comma
  _ <- string ")"
  when (length args < 2) $ fail "coalesce requires at least 2 arguments"
  pure $ Coalesce args Nothing


-- | Parse strcat(expr1, expr2, ...) - string concatenation (1-64 args)
-- Microsoft KQL: strcat(argument1, argument2 [, argument3 ... ])
-- >>> parse pStrcat "" "strcat(method, \" \", url_path)"
-- Right (Strcat [Field (Subject "method" "method" []),Str " ",Field (Subject "url_path" "url_path" [])] Nothing)
pStrcat :: Parser AggFunction
pStrcat = pVariadicAgg "strcat" Strcat


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
  _ <- string "round("
  val <- pScalarExprNested
  _ <- comma
  decimals <- pScalarExpr
  case decimals of
    Num n -> case readMaybe (toString n) :: Maybe Int of
      Just d | d >= 0 && d <= 15 -> pass
      Just _ -> fail "round() decimals must be 0-15 per PostgreSQL limit"
      Nothing -> fail "round() decimals must be a valid integer"
    _ -> fail "round() decimals must be numeric"
  _ <- string ")"
  pure $ Round val decimals


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


-- | Extract the explicit alias from an AggFunction (if set)
-- Uses generic-lens to access the Maybe Text alias field in each constructor
getAlias :: AggFunction -> Maybe Text
getAlias = view typed


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
  Percentile (SubjectExpr (Subject n _ _) _) pct _ -> "percentile_" <> sanitizeAlias n <> "_" <> T.replace "." "_" (show pct)
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
  displayPrec _ agg = displayBuilder $ aggToSqlNoAlias agg <> " AS " <> fromMaybe (defaultAlias agg) (getAlias agg)


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
  toQText (TakeCommand limit) = "take " <> toText (show (limit :: Int))


----------------------------------------------------------------------
----

-- Example queries
-- request_body.v1.v2 == "abc" AND (request_body.v3.v4 == 123 OR request_body.v5[].v6==ANY[1,2,3] OR request_body[1].v7 OR NOT request_body[-1].v8 )
-- request_body[1].v7 | {.v7, .v8} |

-- Sort field can include optional direction
data SortField = SortField Subject (Maybe Text) -- field and optional direction (asc/desc)
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
pBinFunction = try binParser <|> binAutoParser
  where
    binParser = do
      _ <- string "bin("
      subj <- pSubject
      _ <- string ","
      space
      -- Support time duration units or just plain numbers (seconds)
      interval <- toText <$> some (alphaNumChar <|> char '.' <|> char 'm' <|> char 's' <|> char 'h' <|> char 'd' <|> char 'w')
      _ <- string ")"
      return $ Bin subj interval

    binAutoParser = do
      _ <- string "bin_auto("
      subj <- pSubject
      _ <- string ")"
      return $ BinAuto subj


instance ToQueryText BinFunction where
  toQText (Bin subj interval) = "bin(" <> toQText subj <> ", " <> interval <> ")"
  toQText (BinAuto subj) = "bin_auto(" <> toQText subj <> ")"


instance Display BinFunction where
  displayPrec _prec (Bin subj interval) = displayBuilder $ "time_bucket('" <> kqlTimespanToTimeBucket interval <> "', " <> display subj <> ")"
  -- Use default bin size for auto binning (defined in Parser.hs)
  -- Don't include "as bin_timestamp" here as it causes syntax errors in GROUP BY
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
pSummarizeByClause = do
  _ <- string "by"
  space
  SummarizeByClause <$> pByClauseItem `sepBy` comma
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
-- Right (SortField (Subject "parent_id" "parent_id" []) (Just "asc"))
--
-- >>> parse pSortField "" "timestamp"
-- Right (SortField (Subject "timestamp" "timestamp" []) Nothing)
pSortField :: Parser SortField
pSortField = do
  field <- pSubject
  dirM <- optional $ try (space *> (string "asc" <|> string "desc"))
  return $ SortField field dirM


instance ToQueryText SortField where
  toQText (SortField field Nothing) = toQText field
  toQText (SortField field (Just dir)) = toQText field <> " " <> dir


-- | Parser for 'sort by' command (also supports 'order by' synonym)
--
-- >>> parse pSortSection "" "sort by parent_id asc, timestamp desc"
-- Right (SortCommand [SortField (Subject "parent_id" "parent_id" []) (Just "asc"),SortField (Subject "timestamp" "timestamp" []) (Just "desc")])
--
-- >>> parse pSortSection "" "order by parent_id asc"
-- Right (SortCommand [SortField (Subject "parent_id" "parent_id" []) (Just "asc")])
pSortSection :: Parser Section
pSortSection = do
  _ <- string "sort by" <|> string "order by"
  space
  fields <- sepBy pSortField comma
  return $ SortCommand fields


-- | Parser for 'take' command (also supports 'limit' synonym)
--
-- >>> parse pTakeSection "" "take 1000"
-- Right (TakeCommand 1000)
--
-- >>> parse pTakeSection "" "limit 500"
-- Right (TakeCommand 500)
pTakeSection :: Parser Section
pTakeSection = do
  _ <- string "take" <|> string "limit"
  space
  limitStr <- some digitChar
  let limit = readMaybe (toString limitStr) :: Maybe Int
  return $ TakeCommand (fromMaybe 1000 limit) -- Default to 1000 if parsing fails


-- | Parse 'name = expression' pattern, returns (name, aliased expression)
-- Used by extend/project commands and named aggregations in summarize
-- Allows dots in names which are sanitized to underscores for valid SQL aliases
-- NOTE: try is only on the prefix "name =" so validation errors propagate
pNamedExpr :: Parser (Text, AggFunction)
pNamedExpr = do
  name <- try $ toText <$> some (alphaNumChar <|> oneOf ("_." :: String)) <* space <* string "=" <* space
  let sanitized = sanitizeAlias name
  (sanitized,) . setAlias sanitized <$> aggFunctionParser


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


-- | Set alias on an aggregation function
-- Uses generic-lens to set the Maybe Text alias field in each constructor
setAlias :: Text -> AggFunction -> AggFunction
setAlias n = set typed (Just n)


-- | Parse a named aggregation like 'TotalCount = count()'
--
-- >>> parse namedAggregation "" "TotalCount=count()"
-- Right (Count (Subject "*" "*" []) (Just "TotalCount"))
--
-- >>> parse namedAggregation "" "error_count = countif(status_code == \"ERROR\")"
-- Right (CountIf (Eq (Subject "status_code" "status_code" []) (Str "ERROR")) (Just "error_count"))
namedAggregation :: Parser AggFunction
namedAggregation = snd <$> pNamedExpr


-- | Parse the summarize command
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
  funcs <- sepBy (namedAggregation <|> pAggWithOptionalArith) (char ',' <* space)
  byClause <- optional $ try (space *> pSummarizeByClause)
  return $ SummarizeCommand funcs byClause
  where
    -- Parse arithmetic expression with proper operator precedence
    -- \* and / bind tighter than + and -
    -- Example: count() + 5 * 10 parses as count() + (5 * 10)
    pAggWithOptionalArith = do
      expr <- pArithExpr
      pure $ case expr of
        SAgg agg -> agg -- Plain aggregation, return as-is
        _ -> ArithExpr expr Nothing -- Arithmetic expression
    pArithExpr :: Parser ScalarExpr
    pArithExpr = makeExprParser pArithTerm arithOperatorTable

    arithOperatorTable :: [[Operator Parser ScalarExpr]]
    arithOperatorTable =
      [ [infixL '*' Mul, infixL '/' Div]
      , [infixL '+' Add, infixL '-' Sub]
      ]

    infixL :: Char -> ArithOp -> Operator Parser ScalarExpr
    infixL c op = InfixL $ try ((`SArith` op) <$ (hspace *> char c <* hspace))

    -- try on pNumericOnly is fine; aggFunctionParser parsers use try on prefix only
    -- so validation errors propagate after consuming function input
    pArithTerm :: Parser ScalarExpr
    pArithTerm = choice [SVal <$> try pNumericOnly, SAgg <$> aggFunctionParser, char '(' *> pArithExpr <* char ')']

    pNumericOnly = Num . toText <$> (try ((\a c -> a ++ "." ++ c) <$> some digitChar <* char '.' <*> some digitChar) <|> some digitChar)


-- | Parser for where clause section
pWhereSection :: Parser Section
pWhereSection = do
  _ <- string "where"
  space
  WhereClause <$> pExpr


pSection :: Parser Section
pSection = do
  _ <- space
  choice @[]
    [ pSummarizeSection
    , pSortSection
    , pTakeSection
    , try pExtendSection
    , try pProjectSection
    , try pWhereSection -- Try to parse 'where' clause first
    , Search <$> pExpr -- Fall back to bare expression for backward compatibility
    , Source <$> pSource
    ]


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
    [ SSpans <$ string "spans"
    , SSpans <$ string "otlp_logs_and_spans"
    , SMetrics <$ string "metrics"
    , SMetrics <$ string "telemetry.metrics"
    ]


instance ToQueryText Sources where
  toQText SSpans = "spans"
  toQText SMetrics = "metrics"


instance Display Sources where
  displayPrec _prec SSpans = "otel_logs_and_spans"
  displayPrec _prec SMetrics = "telemetry.metrics"


--- >>> parse parseQuery "" "method// = bla "
-- Right []
--
-- >>> parse parseQuery "" "| summarize count(*) by bin_auto(timestamp)"
-- Right [SummarizeCommand [Count (Subject "*" "*" []) Nothing] (Just (SummarizeByClause [Right (BinAuto (Subject "timestamp" "timestamp" []))]))]
--
parseQuery :: Parser [Section]
parseQuery = do
  -- Optionally parse a leading pipe character and ignore it
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

    -- Reorder sections to canonical order (respects state dependencies)
    -- Order: Source -> filters -> extends -> summarize -> project -> sort -> take
    reorderSections :: [Section] -> [Section]
    reorderSections secs = sources <> filters <> extends <> summarizes <> projects <> sorts <> takes
      where
        sources = [s | s@(Source _) <- secs]
        filters = [s | s@(Search _) <- secs] <> [s | s@(WhereClause _) <- secs]
        extends = [s | s@(ExtendCommand _) <- secs]
        summarizes = [s | s@(SummarizeCommand _ _) <- secs]
        projects = [s | s@(ProjectCommand _) <- secs]
        sorts = [s | s@(SortCommand _) <- secs]
        takes = [s | s@(TakeCommand _) <- secs]


instance ToQueryText [Section] where
  toQText secs = T.intercalate " | " $ map toQText secs


-- | Extract percentiles info from a list of sections
-- Returns (subjectExpr as SQL, percentiles) if a Percentile(s) aggregation is found
extractPercentilesInfo :: [Section] -> Maybe (Text, [Double])
extractPercentilesInfo = foldr go Nothing
  where
    go (SummarizeCommand (findPercentiles -> Just info) _) _ = Just info
    go _ acc = acc
    findPercentiles [] = Nothing
    findPercentiles (Percentile subExpr pct _ : _) = Just (subjectExprToSQL subExpr, [pct])
    findPercentiles (Percentiles subExpr pcts _ : _) = Just (subjectExprToSQL subExpr, pcts)
    findPercentiles (_ : rest) = findPercentiles rest
