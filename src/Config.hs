{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config
  ( loadConfig
  , Config (..)
  , StorageConfig (..)
  , NetworkConfig (..)
  ) where

import Toml (TomlCodec, (.=))
import qualified Toml

import Log (Severity(Warn), emit)
import Control.Exception (try, IOException)

-- Data

-- | General application settings.
data StorageConfig = StorageConfig
  { mailRoot :: FilePath -- ^ Root directory for user mailboxes
  , passName :: String   -- ^ Service name or port number (e.g. "pop3" or "110")
  , lockName :: String   -- ^ Service name or port number (e.g. "pop3" or "110")
  }

-- | Network and I/O tuning settings.
data NetworkConfig = NetworkConfig
  { port          :: String -- ^ Service name or port number (e.g. "pop3" or "110")
  , idleTimeout   :: Int    -- ^ autologout timeout for idle clients
  , listenBacklog :: Int    -- ^ TCP listen backlog queue depth
  , backoffMin    :: Int    -- ^ Minimum retry backoff in microseconds
  , backoffMax    :: Int    -- ^ Maximum retry backoff in microseconds
  , readChunk     :: Int    -- ^ Bytes per read syscall
  }

-- | Top-level config, composed of named sections.
data Config = Config
  { storage :: StorageConfig
  , network :: NetworkConfig
  }


-- Codecs

storageCodec :: TomlCodec StorageConfig
storageCodec = StorageConfig
  <$> Toml.string "mail_root" .= mailRoot
  <*> Toml.string "pass_name" .= passName
  <*> Toml.string "lock_name" .= lockName

networkCodec :: TomlCodec NetworkConfig
networkCodec = NetworkConfig
  <$> Toml.string "port"        .= port
  <*> Toml.int "idle_timeout"   .= idleTimeout
  <*> Toml.int "listen_backlog" .= listenBacklog
  <*> Toml.int "backoff_min"    .= backoffMin
  <*> Toml.int "backoff_max"    .= backoffMax
  <*> Toml.int "read_chunk"     .= readChunk

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.table storageCodec "storage" .= storage
  <*> Toml.table networkCodec "network" .= network


-- Loading

defaultConfig :: Config
defaultConfig = Config
  { storage = StorageConfig
      { passName = "shadow"
      , lockName = ".lock"
      , mailRoot = "/var/npop"
      }
  , network = NetworkConfig
      { port          = "pop3"
      , idleTimeout   = 600000000
      , listenBacklog = 16
      , backoffMin    = 62500
      , backoffMax    = 16000000
      , readChunk     = 4096
      }
  }

loadConfig :: FilePath -> IO Config
loadConfig path = do
  result <- try $ Toml.decodeFileEither configCodec path

  case result of
    Left (_ :: IOException) -> do
      emit Warn "no config file, using defaults"
      pure defaultConfig
    Right (Left err)  -> do
      emit Warn $ "config err, using defaults: " <> show err
      pure defaultConfig
    Right (Right config) -> pure config
