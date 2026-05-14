{-# LANGUAGE LambdaCase #-}

module Main where

import Crypto.BCrypt
import qualified Data.ByteString.Char8 as BS
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hSetEcho, stdin, hFlush, stdout, stderr, hPutStr, isEOF)
import App (buildEnv, AppEnv (shadow, config), BuildErr (ConfigErr, ShadowErr))
import Shadow (userExists)
import Control.Monad (when, unless)
import Config (Config(storage), StorageConfig (mailRoot, passName))
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import Constant (curName, newName)
import qualified Data.Map.Strict as Map
import System.Posix (getProcessID, epochTime, getSystemID, SystemID (nodeName))
import Data.List (intercalate)

promptPass :: IO String
promptPass = do
  hPutStr stderr "Password: "
  hFlush stdout
  hSetEcho stdin False
  pass <- getLine
  hSetEcho stdin True
  hPutStr stderr "\n"
  pure pass

readUntilEOF :: IO String
readUntilEOF = do
  eof <- isEOF
  if eof
    then pure ""
    else do
      line <- getLine
      rest <- readUntilEOF
      pure (line <> "\n" <> rest)

genHash :: String -> IO String
genHash pass =
  hashPasswordUsingPolicy slowerBcryptHashingPolicy (BS.pack pass)
    >>= maybe (die "hashing failed") (pure . BS.unpack)

shadowPath :: AppEnv -> FilePath
shadowPath env =
  let conf = (storage . config) env
  in  mailRoot conf </> passName conf

userDir :: AppEnv -> String -> FilePath
userDir env user =
  let conf = (storage . config) env
  in  mailRoot conf </> user

mailDirs :: AppEnv -> String -> (FilePath, FilePath)
mailDirs env user =
  let root = userDir env user
  in (root </> newName, root </> curName)

mailUID :: IO String
mailUID = do
  time <- epochTime
  pid <- getProcessID
  host <- nodeName <$> getSystemID
  pure $ intercalate "." [show time, show pid, host]

addUser :: String -> AppEnv -> IO ()
addUser user env = do
  when (userExists (shadow env) user) $ die "user already exists"

  pass <- promptPass
  hash <- genHash pass

  let entry = user <> ":" <> hash <> "\n"
  appendFile (shadowPath env) entry

  let (newDir, curDir) = mailDirs env user
  createDirectoryIfMissing True newDir
  createDirectoryIfMissing True curDir

delUser :: String -> AppEnv -> IO ()
delUser user env = do
  unless (userExists (shadow env) user) $ die "user doesn't exist"

  let deleted = Map.delete user (shadow env)
  let contents = unlines $ map (\(u, h) -> u <> ":" <> h) (Map.toList deleted)

  writeFile (shadowPath env) contents
  removeDirectoryRecursive (userDir env user)

putUser :: String -> AppEnv -> IO ()
putUser user env = do
  unless (userExists (shadow env) user) $ die "user doesn't exist"
  path <- (fst (mailDirs env user) </>) <$> mailUID
  readUntilEOF >>= writeFile path

getEnv :: IO AppEnv
getEnv = buildEnv >>= \case
  Left (ConfigErr err) -> die $ "npop-adduser: failed to build config: " <> show err
  Left (ShadowErr err) -> die $ "npop-adduser: failed to build shadow: " <> show err
  Right env            -> pure env

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["add", user] -> getEnv >>= addUser user
    ["del", user] -> getEnv >>= delUser user
    ["put", user] -> getEnv >>= putUser user
    _             -> die "usage: npop-tools add|del|put <username>"
