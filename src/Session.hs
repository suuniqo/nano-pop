{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Session
  ( startSession
  , processQuery
  , processUpdate
  ) where

import Data.Time.Clock.POSIX (POSIXTime)
import Data.ByteString (ByteString)
import Data.Word (Word64)
import Data.Set (Set, empty)
import Query (Query(..), QueryErr)
import Storage (UID, Message)

data StatResponse = StatResponse
  { statCount :: Word
  , statSize  :: Word64
  }

data ListEntry = ListEntry
  { listId   :: Word
  , listSize :: Word64
  }

data ListResponse
  = ListOne ListEntry
  | ListAll [ListEntry]

data UidlEntry = UidlEntry
  { uidlId  :: Word
  , uidlUID :: UID
  }

data UidlResponse
  = UidlOne UidlEntry
  | UidlAll [UidlEntry]

newtype RetrResponse = RetrResponse { retrContent :: ByteString}

data ErrType
  = NoSuchUser
  | InvalidPwd
  | FailedLock
  | FailedScan 
  | NoSuchMsg
  | FailedDel
  | AlreadyDel { deleErrId :: Word }

data OkType
  = OkUser { userUsername :: ByteString }
  | OkPass
  | OkDele { deleOkId :: Word }
  | OkNoop
  | OkRset
  | OkQuit

data Response
  = StatResp StatResponse
  | ListResp ListResponse
  | UidlResp UidlResponse
  | RetrResp RetrResponse
  | Ok  OkType
  | Err ErrType

data SessionErr 
  = Query QueryErr
  | InvalidState

data Auth
data Trans
data Update

data Session s where
  AuthSession   :: Session Auth
  TransSession  :: ByteString -> [Message] -> Set Word -> Session Trans
  UpdateSession :: ByteString -> Set Word -> Session Update

data Transition s
  = Stay (Session s)
  | Next (Session (Next s))

class Phase s where
  type Next s

  processQuery :: Session s -> Either QueryErr Query -> Either SessionErr (Transition s, Response)
  processQuery session query =
    case query of
      Left err -> Left (Query err)
      Right ok -> process session ok

  process :: Session s -> Query -> Either SessionErr (Transition s, Response)

startSession :: Session Auth
startSession = AuthSession

instance Phase Auth where
  type Next Auth = Trans

  process AuthSession query = case query of
    User user -> Right (Next (TransSession user [] empty), Ok (OkUser user))
    _         -> Left InvalidState

instance Phase Trans where
  type Next Trans = Update

  process (TransSession user msgs dels) query = undefined

processUpdate :: Session Update -> IO Response
processUpdate (UpdateSession user dels) = undefined
