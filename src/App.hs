module App
  ( AppEnv (..)
  , App
  , runApp
  , BuildErr (..)
  , buildEnv
  ) where

import Control.Monad.Reader (ReaderT (runReaderT))

import Config (Config (storage), loadConfig, StorageConfig (mailRoot, passName))
import Shadow (Shadow, loadShadow)
import System.Directory (getXdgDirectory, XdgDirectory (XdgConfig))
import Constant (progName, configName)
import System.FilePath ((</>))
import Control.Exception (SomeException, catch)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)

data AppEnv = AppEnv
  { config :: Config
  , shadow :: Shadow
  }

type App = ReaderT AppEnv IO

buildConfig :: IO Config
buildConfig = do
  root <- getXdgDirectory XdgConfig progName
  loadConfig $ root </> configName

buildShadow :: Config -> IO Shadow
buildShadow conf =
  let root = mailRoot $ storage conf
      name = passName $ storage conf
  in  loadShadow (root </> name)
  
data BuildErr
  = ConfigErr SomeException
  | ShadowErr SomeException

buildEnv :: IO (Either BuildErr AppEnv)
buildEnv = runExceptT $ do
  conf <- ExceptT $ catch (Right <$> buildConfig)      (pure . Left . ConfigErr)
  shdw <- ExceptT $ catch (Right <$> buildShadow conf) (pure . Left . ShadowErr)
  pure $ AppEnv conf shdw

runApp :: App a -> AppEnv -> IO a
runApp = runReaderT
