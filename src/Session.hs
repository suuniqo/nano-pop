{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Session
  ( SessionErr (..)
  , Reply
  , startSession
  , processQuery
  , withAuth
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)

import Query (Query (..), QueryErr)
import Storage (Message, Username, userValidate, StorageErr, UID, withLock, fetchMailbox, Lock)
import Data.ByteString (ByteString)
import Error (SysErr)

-- Data

data StatReply = StatResponse
  { statCount :: Word
  , statSize  :: Word
  }

data ListEntry = ListEntry
  { listId   :: Word
  , listSize :: Word64
  }

data ListReply
  = ListOne ListEntry
  | ListAll [ListEntry]

data UidlEntry = UidlEntry
  { uidlId  :: Word
  , uidlUID :: UID
  }

data UidlReply
  = UidlOne UidlEntry
  | UidlAll [UidlEntry]

newtype RetrReply = RetrResponse { retrContent :: ByteString }

data Reply
  = RepUser Username
  | RepPass Username
  | RepDele Word
  | RepNoop
  | RepRset
  | RepQuit (Maybe Username)
  | RepStat StatReply
  | RepList ListReply
  | RepUidl UidlReply
  | RepRetr RetrReply

data SessionErr
  = Sys SysErr
  | Query QueryErr
  | Storage StorageErr
  | InvalidPhase
  | AlreadyDele
  | NoSuchMsg

-- Phases

data Auth
data Authed
data Trans
data Update

data Phase s where
  AuthPhase   :: Maybe Username -> Phase Auth
  AuthedPhase :: Username -> Phase Authed
  TransPhase  :: Username -> Lock -> [Message] -> Set Word -> Phase Trans
  UpdatePhase :: Username -> Lock -> Set Word -> Phase Update

data Transition s
  = Stay (Phase s)
  | Next (Phase (Next s))
  | Term

class Process s where
  type Next s

  processQuery :: Phase s -> Either QueryErr Query -> Either SessionErr (Transition s, Reply)
  processQuery session query =
    case query of
      Left err -> Left (Query err)
      Right ok -> process session ok

  process :: Phase s -> Query -> Either SessionErr (Transition s, Reply)

-- Authentication Phase

startSession :: Phase Auth
startSession = AuthPhase Nothing

processUser :: ByteString -> Either SessionErr (Transition Auth, Reply)
processUser user = case userValidate user of
  Right user' -> Right
    ( Stay $ AuthPhase (Just user')
    , RepUser user'
    )
  Left err -> Left $ Storage err

processPass :: Username -> ByteString -> Either SessionErr (Transition Auth, Reply)
processPass user _ = Right (Next (AuthedPhase user), RepPass user)

processQuitAuth :: Maybe Username -> Either SessionErr (Transition Auth, Reply)
processQuitAuth user = Right (Term, RepQuit user)

instance Process Auth where
  type Next Auth = Authed

  process (AuthPhase Nothing) query = case query of
    User name -> processUser name
    Quit      -> processQuitAuth Nothing
    _         -> Left InvalidPhase

  process (AuthPhase (Just user)) query = case query of
    User name -> processUser name
    Pass pass -> processPass user pass
    Quit      -> processQuitAuth (Just user)
    _         -> Left InvalidPhase

-- Transaction Phase

wrapLock :: Username -> (Phase Trans -> IO a) -> Lock -> IO (Either SessionErr a)
wrapLock user action lock = do
  maildrop <- fetchMailbox lock user

  case maildrop of
    Left err   -> pure $ Left (Storage err)
    Right msgs -> Right <$> action (TransPhase user lock msgs Set.empty)

withAuth :: Phase Authed -> (Phase Trans -> IO a) -> IO (Either SessionErr a)
withAuth (AuthedPhase user) action = do
  result <- withLock user (wrapLock user action)

  case result of
    Left err  -> pure $ Left (Storage err)
    Right res -> pure res

instance Process Trans where
  type Next Trans = Update

  process (TransPhase user lock msgs dels) query = case query of
    Stat     -> undefined
    List opt -> undefined
    Uidl opt -> undefined
    Retr msg -> undefined
    Dele msg -> undefined
    Noop     -> undefined
    Rset     -> undefined
    Quit     -> undefined
    _        -> Left InvalidPhase
