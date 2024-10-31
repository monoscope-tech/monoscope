module Pkg.Parser.Types (Parser, Values (..), Subject (..), FieldKey (..), Expr (..), AggFunction (..), ByClause (..), Rollup (..), Section (..), Sources (..), symbol, lexeme, sc)
where

import Relude
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L
import qualified Data.Aeson as AE


-- Example queries
-- request_body.v1.v2 == "abc" AND (request_body.v3.v4 == 123 OR request_body.v5[].v6==ANY[1,2,3] OR request_body[1].v7 OR NOT request_body[-1].v8 )
-- request_body[1].v7 | {.v7, .v8} |

type Parser = Parsec Void Text


-- Values is an enum of the list of supported value types.
-- Num is a text  that represents a float as float covers ints in a lot of cases. But its basically the json num type.
data Values = Num Text | Str Text | Boolean Bool | Null | List [Values]
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- A subject consists of the primary key, and then the list of fields keys which are delimited by a .
-- To support jsonpath, we will have more powerfule field keys, so instead of a text array, we could have an enum field key type?
data Subject = Subject Text Text [FieldKey]
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data FieldKey = FieldKey Text | ArrayIndex Text Int | ArrayWildcard Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Expr
  = Eq Subject Values
  | NotEq Subject Values
  | GT Subject Values
  | LT Subject Values
  | GTEq Subject Values
  | LTEq Subject Values
  | Regex Subject Text
  | Paren Expr
  | And Expr Expr
  | Or Expr Expr
  --  | Not Expr
  --  | JSONPathExpr Expr
  --  | FunctionCall String [Expr] -- For functions like ANY
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Modify Aggregation Functions to include optional aliases
data AggFunction
  = Count Subject (Maybe Text) -- Optional field and alias
  | Sum Subject (Maybe Text)
  | Avg Subject (Maybe Text)
  | Min Subject (Maybe Text)
  | Max Subject (Maybe Text)
  | Median Subject (Maybe Text)
  | Stdev Subject (Maybe Text)
  | Range Subject (Maybe Text)
  | -- | CustomAgg String [Field] (Maybe String)
    Plain Subject (Maybe Text)
  deriving stock (Show, Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Define an optional 'by' clause
data ByClause = ByClause [Subject] -- List of fields to group by
  deriving stock (Show, Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Rollup = Rollup Text
  deriving stock (Show, Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Sources = SRequests | SLogs | STraces | SSpans | SMetrics
  deriving stock (Show, Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Section
  = Search Expr
  | -- Define the AST for the 'stats' command
    StatsCommand [AggFunction] (Maybe ByClause)
  | TimeChartCommand AggFunction (Maybe ByClause) (Maybe Rollup)
  | Source Sources
  deriving stock (Show, Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


sc :: Parser ()
sc =
  L.space
    space1 -- (2)
    (L.skipLineComment "//") -- (3)
    (L.skipBlockComment "/*" "*/") -- (4)


-- lexeme is a wrapper for lexemes that picks up all trailing white space using the supplied space consumer.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


-- symbol is a parser that matches given text using string internally and then similarly picks up all trailing white space.
symbol :: Text -> Parser Text
symbol = L.symbol sc
