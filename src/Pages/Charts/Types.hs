{-# LANGUAGE NoFieldSelectors #-}

module Pages.Charts.Types (MetricsData (..), MetricsStats (..), DataType (..)) where

import Control.Lens ((?~))
import Data.Aeson qualified as AE
import Data.Default
import Data.OpenApi (NamedSchema (..), ToParamSchema (..), ToSchema (..), enum_, type_)
import Data.OpenApi qualified as OpenApi
import Data.Semigroup (Max (Max))
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Language.Haskell.TH.Syntax qualified as THS
import Pkg.DeriveUtils (SnakeSchema (..))
import Relude
import Servant (FromHttpApiData (..))
import Utils (JSONHttpApiData (..))


data MetricsStats = MetricsStats
  { min :: Double
  , max :: Double
  , sum :: Double
  , count :: Int
  , mean :: Double
  , mode :: Double
  , maxGroupSum :: Double
  }
  deriving (Generic, Show, THS.Lift)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake MetricsStats
  deriving (ToSchema) via SnakeSchema MetricsStats


data MetricsData = MetricsData
  { dataset :: V.Vector (V.Vector (Maybe Double))
  , dataFloat :: Maybe Double
  , dataJSON :: V.Vector (V.Vector AE.Value)
  , dataText :: V.Vector (V.Vector Text)
  , headers :: V.Vector Text
  , rowsCount :: Double
  , rowsPerMin :: Maybe Double
  , from :: Maybe Int
  , to :: Maybe Int
  , stats :: Maybe MetricsStats
  }
  deriving (Generic, Show)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake MetricsData
  deriving (ToSchema) via SnakeSchema MetricsData


data DataType = DTMetric | DTJson | DTFloat | DTText
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show, THS.Lift)
  deriving anyclass (NFData)
  deriving (Monoid, Semigroup) via (Max DataType)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "DT", DAE.CamelToSnake]] DataType
  deriving (FromHttpApiData) via Utils.JSONHttpApiData DataType


instance ToSchema DataType where
  declareNamedSchema _ =
    pure
      $ NamedSchema (Just "DataType")
      $ mempty
      & type_
      ?~ OpenApi.OpenApiString
        & enum_
      ?~ [AE.toJSON @DataType v | v <- [minBound .. maxBound]]


instance ToParamSchema DataType where
  toParamSchema _ = mempty & type_ ?~ OpenApi.OpenApiString
