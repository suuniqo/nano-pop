module Storage
  ( Message(..), Flag(..), UID(..)
  , maildropUser
  ) where

import Data.Word (Word64)
import Data.Set (Set)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.ByteString (ByteString)

data Flag
  = Seen
  | Replied
  | Trashed
  | Draft
  | Passed

newtype UID = UID ByteString

data Message = Message
  { msgPath  :: !FilePath
  , msgSize  :: !Word64
  , msgFlags :: !(Set Flag)
  , msgTime  :: !POSIXTime
  , msgId    :: !UID
  }

userExists :: ByteString -> IO Bool
userExists = undefined

maildropUser :: ByteString -> IO (Maybe [Message])
maildropUser = undefined
