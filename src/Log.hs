module Log
  ( Severity(..)
  , emit
  ) where

import System.IO (hPrint, stderr)
import Data.List (intercalate)
import Control.Concurrent (ThreadId, myThreadId)

import Constant (progName)

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

emit :: Severity -> String -> IO ()
emit sever msg = entry >>= emitEntry
  where entry = LogData sever msg <$> myThreadId
