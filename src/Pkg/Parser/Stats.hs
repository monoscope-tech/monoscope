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
-- >>> parseTest aggFunctionParser "count(field)"
-- Count Nothing Nothing
--
-- >>> parseTest aggFunctionParser "sum(field) as total"
-- Sum (Subject "field" []) (Just "total")
--
-- >>> parseTest aggFunctionParser "avg(field) as average"
-- Avg (Subject "field" []) (Just "average")
--
-- >>> parseTest aggFunctionParser "min(field)"
-- Min (FieldName "field") Nothing
--
-- >>> parseTest aggFunctionParser "max(field) as maxVal"
-- Max (FieldName "field") (Just "maxVal")
--
-- >>> parseTest aggFunctionParser "median(field) as medianVal"
-- Median (FieldName "field") (Just "medianVal")
--
-- >>> parseTest aggFunctionParser "stdev(field)"
-- Stdev (FieldName "field") Nothing
--
-- >>> parseTest aggFunctionParser "range(field) as rangeVal"
-- Range (FieldName "field") (Just "rangeVal")
--
-- >>> parseTest aggFunctionParser "customFunc(field) as custom"
-- CustomAgg "customFunc" [FieldName "field"] (Just "custom")
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
-- >>> parseTest aliasParser "as min_price"
-- "min_price"
aliasParser :: Parser String
aliasParser = string " as" *> space *> some (alphaNumChar <|> oneOf "_-") <* space


-- | Parses the 'by' clause, which can include multiple fields.
--
-- >>> parseTest byClauseParser "by field1,field2"
-- ByClause [Subject "field1" [],Subject "field2" []]
byClauseParser :: Parser ByClause
byClauseParser = ByClause <$> (string "by" *> space *> sepBy pSubject (char ','))


-- StatsCommand [Count Nothing Nothing] (Just (ByClause [FieldName "field1"]))

-- | Combines the above parsers to parse the entire 'stats' command.
--
-- >>> parseTest pStatsSection "stats count by field1"
-- StatsCommand [Count Nothing Nothing] (Just (ByClause [FieldName "field1"]))
--
-- >>> parseTest pStatsSection "stats sum(field) as total, avg(field2)"
-- StatsCommand [Sum (FieldName "field") (Just "total"),Avg (FieldName "field2") Nothing] Nothing
--
-- >>> parseTest pStatsSection "stats max(field) by field1,field2"
-- StatsCommand [Max (FieldName "field") Nothing] (Just (ByClause [FieldName "field1", FieldName "field2"]))
pStatsSection :: Parser Section 
pStatsSection = do
  _ <- string "stats"
  space
  funcs <- sepBy aggFunctionParser (char ',')
  byClause <- optional (space *> byClauseParser)
  return $ StatsCommand funcs byClause
