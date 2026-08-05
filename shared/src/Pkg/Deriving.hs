-- | Database-free half of 'Pkg.DeriveUtils': the @DerivingVia@ wrappers and
-- orphan 'Default' instances that need nothing beyond aeson and text.
--
-- It exists so the @monoscope-cli@ package can reuse the wrappers the server
-- uses without linking what the server links. That means no postgresql-simple
-- or hasql (those instances stay in 'Pkg.DeriveUtils', which re-exports this
-- module so no call site has to care) and no openapi3 either — a CLI has no use
-- for schema generation, and it is one of the larger dependencies in the tree.
-- The @ToSchema@ instances live in 'Pkg.DeriveUtils' and 'Web.WireSchemas'.
module Pkg.Deriving (
  KnownMaybeSymbol (..),
  WrappedEnumSC (..),
  decodeEnumSC,
  encodeEnumSC,
  escapeRegex,
  splitQualType,
) where

import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.CaseInsensitive (CI, FoldCase)
import Data.CaseInsensitive qualified as CI (mk)
import Data.Default (Default (..))
import Data.Text qualified as T
import Data.Text.Display (Display (..))
import Data.Text.Lazy qualified as TL
import Data.Time (UTCTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Relude
import Relude.Unsafe qualified as Unsafe
import Web.HttpApiData (FromHttpApiData (..))
import System.Envy (Var (..))
import Text.Casing (fromSnake, quietSnake, toPascal)


-- | DerivingVia wrapper for snake-case-stringified enums.
--
-- The first parameter pins the backing Postgres column type for the hasql
-- instances (defined in 'Pkg.DeriveUtils'):
--
--   * @''Nothing@ — column is plain @TEXT@; hasql decodes via @D.text@.
--   * @('Just "schema.type_name")@ — column is a real @CREATE TYPE … AS ENUM@;
--     hasql decodes via @D.enum@ with the matching OID lookup. The leading
--     @schema.@ is required (use @"public.foo"@ if it lives in the default
--     schema).
--
-- The second @Symbol@ is the Haskell-side constructor prefix to strip (e.g.
-- @"P"@ for @PAdmin/PView@ → @admin/view@).
newtype WrappedEnumSC (qualType :: Maybe Symbol) (prefix :: Symbol) a = WrappedEnumSC a
  deriving (Generic)


-- | Reflect a type-level @Maybe Symbol@ to a runtime @Maybe Text@.
class KnownMaybeSymbol (m :: Maybe Symbol) where
  maybeSymbolVal :: Proxy m -> Maybe Text


instance KnownMaybeSymbol 'Nothing where
  maybeSymbolVal _ = Nothing


instance KnownSymbol s => KnownMaybeSymbol ('Just s) where
  maybeSymbolVal _ = Just (toText (symbolVal (Proxy @s)))


-- | Split @"schema.type_name"@ into @(Just schema, type_name)@.
--
-- >>> splitQualType "apis.severity"
-- (Just "apis","severity")
-- >>> splitQualType "severity"
-- (Nothing,"severity")
splitQualType :: Text -> (Maybe Text, Text)
splitQualType qn = case T.break (== '.') qn of
  (sch, dotTyp) | Just typ <- T.stripPrefix "." dotTyp -> (Just sch, typ)
  _ -> (Nothing, qn)


encodeEnumSC :: forall prefix a. (KnownSymbol prefix, Show a) => a -> String
encodeEnumSC = quietSnake . fromString . drop (length $ symbolVal (Proxy @prefix)) . show


decodeEnumSC :: forall prefix a. (KnownSymbol prefix, Read a) => String -> Maybe a
decodeEnumSC s = readMaybe $ symbolVal (Proxy @prefix) <> toPascal (fromSnake s)


instance (KnownSymbol prefix, Show a) => Display (WrappedEnumSC qualType prefix a) where
  displayBuilder (WrappedEnumSC a) = fromString $ encodeEnumSC @prefix a


instance (KnownSymbol prefix, Show a) => AE.ToJSON (WrappedEnumSC qualType prefix a) where
  toJSON (WrappedEnumSC a) = AE.String . toText $ encodeEnumSC @prefix a


instance (KnownSymbol prefix, Read a, Show a) => AE.FromJSON (WrappedEnumSC qualType prefix a) where
  parseJSON = AE.withText "WrappedEnumSC" \t ->
    maybe (fail $ "Invalid value: " <> toString t) (pure . WrappedEnumSC) $ decodeEnumSC @prefix (toString t)


instance (KnownSymbol prefix, Read a, Show a) => FromHttpApiData (WrappedEnumSC qualType prefix a) where
  parseUrlPiece t = maybe (Left $ "Invalid " <> fromString (symbolVal (Proxy @prefix)) <> " value: " <> t) (Right . WrappedEnumSC) $ decodeEnumSC @prefix (toString @Text t)


-- | envy env-var (de)serialization. 'decodeEnumSC' normalizes case via
-- @toPascal . fromSnake@, so @ENVIRONMENT=DEV@ / @PROD@ / @staging@ all parse.
instance (KnownSymbol prefix, Read a, Show a, Typeable a, Typeable qualType) => Var (WrappedEnumSC qualType prefix a) where
  toVar (WrappedEnumSC a) = encodeEnumSC @prefix a
  fromVar = fmap WrappedEnumSC . decodeEnumSC @prefix


-- | Backslash-escape POSIX-regex metacharacters so a literal substring can be
-- embedded safely in a regex pattern.
escapeRegex :: Text -> Text
escapeRegex = T.concatMap \c -> if c `elem` (".^$*+?()[]{}|\\" :: [Char]) then "\\" <> one c else one c


-- Default / schema orphans (DB-free half; the postgresql-simple and hasql
-- instances stay in Pkg.DeriveUtils).

instance Default ZonedTime where
  def = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
  {-# INLINE def #-}


instance Default UTCTime where
  def = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
  {-# INLINE def #-}


instance Default UUID.UUID where
  def = UUID.nil
  {-# INLINE def #-}


instance Default AET.Value where
  def = AET.emptyObject
  {-# INLINE def #-}


instance (Default s, FoldCase s) => Default (CI s) where
  def = CI.mk def
  {-# INLINE def #-}


instance Default T.Text where
  def = T.empty
  {-# INLINE def #-}


instance Default TL.Text where
  def = TL.empty
  {-# INLINE def #-}


instance Default (V.Vector a) where
  def = V.empty
  {-# INLINE def #-}
