module Pkg.Parser.Stats where

import Control.Monad.Combinators.Expr
import Data.Foldable (foldl)
import Data.Text.Display (Display, display, displayBuilder, displayParen, displayPrec)
import Data.Text.Lazy.Builder (Builder)
import Pkg.Parser.Expr
import Pkg.Parser.Types
import Relude hiding (GT, LT, Sum, many, some)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L


-- Syntax:
--
-- stats
-- <aggregation> ...
-- ( [<by-clause>] [<time-span>] )
--
-- aggregation
-- Syntax: <aggregate-function>"("<field> [AS <field>] ) ["," <aggregate-function> "("<field> [AS <field>] ) ]...
-- for example ... | stats sum(bytes) AS 'sum of bytes'.

-- | Handles different aggregation functions and their optional aliases.
--
-- >>> parse aggFunctionParser "" "count(field)"
-- Right (Count (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "count(*)"
-- Right (Count (Subject "*" "*" []) Nothing)
--
-- >>> parse aggFunctionParser "" "sum(field) as total"
-- Right (Sum (Subject "field" "field" []) (Just "total"))
--
-- >>> parse aggFunctionParser "" "avg(field) as average"
-- Right (Avg (Subject "field" "field" []) (Just "average"))
--
-- >>> parse aggFunctionParser "" "min(field)"
-- Right (Min (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "max(field) as maxVal"
-- Right (Max (Subject "field" "field" []) (Just "maxVal"))
--
-- >>> parse aggFunctionParser "" "median(field) as medianVal"
-- Right (Median (Subject "field" "field" []) (Just "medianVal"))
--
-- >>> parse aggFunctionParser "" "stdev(field)"
-- Right (Stdev (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "range(field) as rangeVal"
-- Right (Range (Subject "field" "field" []) (Just "rangeVal"))
--
-- >>> parse aggFunctionParser "" "customFunc(field) as custom"
-- Right (Plain (Subject "customFunc" "customFunc" []) Nothing)
aggFunctionParser :: Parser AggFunction
aggFunctionParser =
  choice
    [ Sum <$> (string "sum(" *> pSubject <* string ")") <*> optional aliasParser
    , Count <$> (string "count(" *> pSubject <* string ")") <*> optional aliasParser
    , Avg <$> (string "avg(" *> pSubject <* string ")") <*> optional aliasParser
    , Min <$> (string "min(" *> pSubject <* string ")") <*> optional aliasParser
    , Max <$> (string "max(" *> pSubject <* string ")") <*> optional aliasParser
    , Median <$> (string "median(" *> pSubject <* string ")") <*> optional aliasParser
    , Stdev <$> (string "stdev(" *> pSubject <* string ")") <*> optional aliasParser
    , Range <$> (string "range(" *> pSubject <* string ")") <*> optional aliasParser
    , Plain <$> pSubject <*> optional aliasParser
    ]


-- | Utility Parser: Parses an alias
--
-- >>> parse "" aliasParser "as min_price"
-- "min_price"
aliasParser :: Parser Text
aliasParser = toText <$> (string " as" *> space *> some (alphaNumChar <|> oneOf "_-") <* space)


-- | Parses the 'by' clause, which can include multiple fields.
--
-- >>> parse "" byClauseParser "by field1,field2"
-- ByClause [Subject "field1" [],Subject "field2" []]
byClauseParser :: Parser ByClause
byClauseParser = ByClause <$> (string "by" *> space *> sepBy pSubject (char ','))


-- StatsCommand [Count Nothing Nothing] (Just (ByClause [FieldName "field1"]))

-- | Combines the above parsers to parse the entire 'stats' command.
--
-- >>> parse pStatsSection "" "stats count by field1"
-- Right (StatsCommand [Plain (Subject "count" "count" []) Nothing] (Just (ByClause [Subject "field1" "field1" []])))
--
-- >>> parse pStatsSection "" "stats sum(field) as total,avg(field2)"
-- Right (StatsCommand [Sum (Subject "field" "field" []) (Just "total"),Avg (Subject "field2" "field2" []) Nothing] Nothing)
--
-- >>> parse pStatsSection "" "stats max(field) by field1,field2"
-- Right (StatsCommand [Max (Subject "field" "field" []) Nothing] (Just (ByClause [Subject "field1" "field1" [],Subject "field2" "field2" []])))
pStatsSection :: Parser Section
pStatsSection = do
  _ <- string "stats"
  space
  funcs <- sepBy aggFunctionParser (char ',')
  byClause <- optional $ try (space *> byClauseParser)
  return $ StatsCommand funcs byClause


-- TimeChartSection  (Count Nothing Nothing) (Just (ByClause [FieldName "field1"])) (Rollup "1w")
--

-- | parses the timechart command which is used to describe timeseries charts based off the request log data.
--
-- >>> parse pTimeChartSection "" "timechart count(*) by field1 [1d]"
-- Right (TimeChartCommand (Count (Subject "*" "*" []) Nothing) (Just (ByClause [Subject "field1" "field1" []])) (Just (Rollup "1d")))
--
-- >>> parse pTimeChartSection "" "timechart sum(field1) by field2,field3 [1d]"
-- Right (TimeChartCommand (Sum (Subject "field1" "field1" []) Nothing) (Just (ByClause [Subject "field2" "field2" [],Subject "field3" "field3" []])) (Just (Rollup "1d")))
--
-- >>> parse pTimeChartSection "" "timechart sum(field1) [1d]"
-- Right (TimeChartCommand (Sum (Subject "field1" "field1" []) Nothing) Nothing (Just (Rollup "1d")))
--
-- >>> parse pTimeChartSection "" "timechart count(*) [1d]"
-- Right (TimeChartCommand (Count (Subject "*" "*" []) Nothing) Nothing (Just (Rollup "1d")))
--
-- >>> parse pTimeChartSection "" "timechart [1d]"
-- Right (TimeChartCommand (Count (Subject "*" "*" []) Nothing) Nothing (Just (Rollup "1d")))
--
-- >>> parse pTimeChartSection "" "timechart"
-- Right (TimeChartCommand (Count (Subject "*" "*" []) Nothing) Nothing Nothing)
pTimeChartSection :: Parser Section
pTimeChartSection = do
  _ <- string "timechart"
  space
  agg <- optional (try aggFunctionParser)
  byClauseM <- optional $ try (space *> byClauseParser)
  rollupM <- optional $ try (space *> rollupParser)
  return $ TimeChartCommand (fromMaybe (Count (Subject "*" "*" []) Nothing) agg) byClauseM rollupM


rollupParser :: Parser Rollup
rollupParser = Rollup <$> toText <$> (string "[" *> some alphaNumChar <* string "]")
