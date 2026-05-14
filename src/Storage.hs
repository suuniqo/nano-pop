{-# LANGUAGE ScopedTypeVariables #-}

module Storage
  ( StorageErr (..)
  , Message (..)
  , Flag (..)
  , Lock
  , withLock
  , fetchMailbox
  , updateMailbox
  ) where

import qualified UnliftIO as UIO
import Control.Exception (catch, throwIO)
import Data.List (stripPrefix)
import Data.Set (Set)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.IO.Exception (IOErrorType (AlreadyExists))
import System.Directory (listDirectory, renameFile)
import System.FilePath
import System.Posix
  ( Fd, FileOffset, FileStatus
  , OpenFileFlags (creat, exclusive)
  , OpenMode (ReadWrite)
  , closeFd, defaultFileFlags, fileSize
  , getFileStatus, isRegularFile
  , openFd, removeLink
  )
import Text.Read (readMaybe)

import Error
  ( Oper (OpClose, OpListDir, OpOpen, OpStatFile, OpUnlink, OpMove)
  , SysErr, annotate, classify, corruptMailErr
  )

import Types (Username (unUser), UID (unUID), readUID)
import App (App, AppEnv (config))
import Control.Monad.RWS (asks, MonadIO (liftIO))
import Config (StorageConfig(mailRoot, lockName), Config (storage))
import Constant (mailSep, curName, newName)

-- Data

data Flag
  = Draft
  | Passed
  | Replied
  | Seen
  | Trashed
  deriving (Show, Eq, Ord)

data Message = Message
  { msgPath  :: !FilePath
  , msgSize  :: !Integer
  , msgTime  :: !POSIXTime
  , msgFlags :: !(Set Flag)
  , msgUid   :: !UID
  }
  deriving Show

data Lock = Lock
  { lockPath :: !FilePath
  , lockFd   :: !Fd
  }
  deriving Show

data StorageErr
  = UserLocked

instance Show StorageErr where
  show err = case err of
    UserLocked -> "mailbox in use"

-- Syscalls

tryOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO Fd
tryOpen path mode flags = annotate (OpOpen path) call
  where call = openFd path mode flags

tryClose :: FilePath -> Fd -> IO ()
tryClose path fd = annotate (OpClose path) call
  where call = closeFd fd

tryOpenExcl :: FilePath -> IO Fd
tryOpenExcl path = tryOpen path ReadWrite flags
  where
    flags = defaultFileFlags
      { creat = Just 0o600
      , exclusive = True
      }

tryUnlink :: FilePath -> IO ()
tryUnlink path = annotate (OpUnlink path) call
  where call = removeLink path

tryListDir :: FilePath -> IO [FilePath]
tryListDir path = annotate OpListDir call
  where call = map (path </>) <$> listDirectory path

tryStatFile :: FilePath -> IO FileStatus
tryStatFile path = annotate (OpStatFile path) call
  where call = getFileStatus path

tryMove :: FilePath -> FilePath -> IO ()
tryMove src dst = annotate (OpMove src dst) call
  where call = renameFile src dst

-- Layout

userMailbox :: Username -> App FilePath
userMailbox user = do
  root <- asks (mailRoot . storage . config)
  pure (root </> unUser user)

userLockPath :: Username -> App FilePath
userLockPath user = do
  name    <- asks (lockName . storage . config)
  mailbox <- userMailbox user
  pure (mailbox </> name)

-- Lock

releaseLock :: Lock -> IO ()
releaseLock lock = do
  tryClose (lockPath lock) (lockFd lock)
  tryUnlink (lockPath lock)

acquireLock :: FilePath -> IO Lock
acquireLock path = Lock path <$> tryOpenExcl path

-- Filesystem

fileWithStat :: FilePath -> IO [(FilePath, FileStatus)]
fileWithStat path = do
  files <- tryListDir path
  mapM (\fp -> (,) fp <$> tryStatFile fp) files

fileWithSizes :: FilePath -> IO [(FilePath, FileOffset)]
fileWithSizes path = do
  files <- fileWithStat path
  pure [(fp, fileSize st) | (fp, st) <- files, isRegularFile st]

maildirFiles :: Username -> App [(FilePath, FileOffset)]
maildirFiles user = do
  userPath <- userMailbox user

  curFiles <- liftIO $ fileWithSizes (userPath </> curName)
  newFiles <- liftIO $ fileWithSizes (userPath </> newName)

  pure (curFiles <> newFiles)

-- Maildir Parsing

flagFromChar :: Char -> Maybe Flag
flagFromChar c = case c of
  'S' -> Just Seen
  'R' -> Just Replied
  'T' -> Just Trashed
  'D' -> Just Draft
  'P' -> Just Passed
  _   -> Nothing

readFlags :: String -> Maybe (Set Flag)
readFlags info = case stripPrefix mailSep info of
  Nothing -> Just Set.empty
  Just fs -> Set.fromList <$> mapM flagFromChar fs

readTime :: String -> Maybe POSIXTime
readTime = (toTime <$>) . readMaybe . takeWhile (/= '.')
  where toTime = fromIntegral :: Integer -> POSIXTime

buildMessage :: (FilePath, FileOffset) -> Maybe Message
buildMessage (path, size) = do
  let (base, info) = break (== ':') (takeFileName path)

  flags <- readFlags info
  time  <- readTime base
  uid   <- readUID base

  pure $ Message path (fromIntegral size) time flags uid

-- Maildir Dumping

flagToChar :: Flag -> Char
flagToChar flag = case flag of
  Seen    -> 'S'
  Replied -> 'R'
  Trashed -> 'T'
  Draft   -> 'D'
  Passed  -> 'P'

flagged :: Flag -> Message -> Bool
flagged flag = Set.member flag . msgFlags

infoMessage :: Message -> FilePath
infoMessage = map flagToChar . Set.toList . msgFlags

dirMessage :: Message -> FilePath
dirMessage msg
  | flagged Seen msg = curName
  | otherwise        = newName

deleMessage :: Message -> IO ()
deleMessage = tryUnlink . msgPath

moveMessage :: FilePath -> Message -> IO ()
moveMessage root msg = tryMove src (root </> dir </> dst)
  where
    src = msgPath msg
    dir = dirMessage msg
    dst = (unUID . msgUid) msg <> mailSep <> infoMessage msg

-- Methods

withLock :: Username -> (Lock -> App a) -> App (Either StorageErr a)
withLock user action = do
  path   <- userLockPath user
  result <- liftIO $ (Right <$> acquireLock path)
    `catch` \(err :: SysErr) -> case classify err of
      AlreadyExists -> pure $ Left UserLocked
      _             -> throwIO err

  case result of
    Left e     -> pure $ Left e
    Right lock -> Right <$> UIO.bracket (pure lock) (liftIO . releaseLock) action

fetchMailbox :: Lock -> Username -> App (Seq Message)
fetchMailbox _ user = do
  maybeMsgs <- mapM buildMessage <$> maildirFiles user

  case maybeMsgs of
    Nothing   -> liftIO $ throwIO corruptMailErr
    Just msgs -> pure $ Seq.fromList msgs

updateMailbox :: Lock -> Username -> [Message] -> [Message] -> App ()
updateMailbox _ user trash seen = do
  mailbox <- userMailbox user

  liftIO $ mapM_ (moveMessage mailbox) seen
  liftIO $ mapM_ deleMessage trash
