{-# LANGUAGE OverloadedStrings #-}

-- | Instance-wide configuration stored in the @system.app_config@ table.
-- One row per logical config ('smtp', 'telegram', …). Values are JSONB so
-- we can evolve the shape without migrations. Senders fetch a config on each
-- send (cheap single-row lookup) so admins can rotate secrets without a
-- restart.
module Models.System.AppConfig
  ( SmtpConfig (..)
  , TelegramConfig (..)
  , getSmtpConfig
  , setSmtpConfig
  , getTelegramConfig
  , setTelegramConfig
  , getRaw
  , setRaw
  ) where

import Data.Aeson qualified as AE
import Data.Effectful.Hasql qualified as Hasql
import Deriving.Aeson qualified as DAE
import Effectful
import Hasql.Interpolate qualified as HI
import Pkg.DeriveUtils (DB)
import Relude


-- | SMTP credentials. When @host@ is empty, callers should fall back to env
-- vars (legacy) or to the Postmark branch.
data SmtpConfig = SmtpConfig
  { host :: Text
  , port :: Int
  , tls :: Bool
  , username :: Text
  , password :: Text
  , sender :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] SmtpConfig


-- | Telegram bot token + a free-form description (so the admin remembers
-- which @BotFather bot this is when there are many monoscope instances).
data TelegramConfig = TelegramConfig
  { botToken :: Text
  , description :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] TelegramConfig


-- | Raw row lookup. Returns @Nothing@ when the key has never been set.
getRaw :: DB es => Text -> Eff es (Maybe AE.Value)
getRaw key = Hasql.interpOne [HI.sql| SELECT value FROM system.app_config WHERE key = #{key} LIMIT 1 |]


-- | Upsert a raw JSON value under a key.
setRaw :: DB es => Text -> AE.Value -> Eff es ()
setRaw key val =
  Hasql.interpExecute_
    [HI.sql|
      INSERT INTO system.app_config (key, value, updated_at)
      VALUES (#{key}, #{HI.AsJsonb val}, now())
      ON CONFLICT (key) DO UPDATE SET value = EXCLUDED.value, updated_at = now()
    |]


-- | Decode an Aeson Value via FromJSON. 'Nothing' when the persisted shape
-- doesn't match the current type (e.g. we added a field and an old row is
-- still around).
decodeValue :: AE.FromJSON a => AE.Value -> Maybe a
decodeValue v = case AE.fromJSON v of
  AE.Success x -> Just x
  AE.Error _ -> Nothing


getSmtpConfig :: DB es => Eff es (Maybe SmtpConfig)
getSmtpConfig = (>>= decodeValue) <$> getRaw "smtp"


setSmtpConfig :: DB es => SmtpConfig -> Eff es ()
setSmtpConfig = setRaw "smtp" . AE.toJSON


getTelegramConfig :: DB es => Eff es (Maybe TelegramConfig)
getTelegramConfig = (>>= decodeValue) <$> getRaw "telegram"


setTelegramConfig :: DB es => TelegramConfig -> Eff es ()
setTelegramConfig = setRaw "telegram" . AE.toJSON
