module Log
  ( Severity(..)
  , emit
  ) where

import System.IO (hPrint, stderr)
import Data.List (intercalate)
import Config (progName)
import Control.Concurrent (ThreadId, myThreadId)

data Severity
  = Info
  | Warn
  | Fatal

instance Show Severity where
  show Info  = "info"
  show Warn  = "warning"
  show Fatal = "fatal"

data LogData = LogData Severity String ThreadId

instance Show LogData where
  show (LogData sever msg thid) = intercalate ": " [progName, show thid, show sever, msg]

emitEntry :: LogData -> IO ()
emitEntry = hPrint stderr

emit :: Show a => Severity -> a -> IO ()
emit sever a = entry >>= emitEntry
  where entry = LogData sever (show a) <$> myThreadId
