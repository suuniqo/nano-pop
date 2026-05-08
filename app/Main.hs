module Main where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.ByteString.Char8 (pack)

import Server
    ( Connection(connPeer),
      Listener,
      acquireClient,
      recvClient,
      sendClient,
      withClient',
      withListener,
      Connection,
      Listener )

import Query (buildQuery)

import Log (emit, Severity(..))

sendMsg :: Connection -> IO ()
sendMsg conn = do
  line <- recvClient conn

  let query = buildQuery line
  let response = show query <> "\r\n"

  sendClient conn (pack response)

  case query of
    Left err -> emit Fatal err
    Right _  -> sendMsg conn

serverLoop :: Listener -> IO ()
serverLoop listener = do
  conn <- acquireClient listener
  emit Info $ "new client: " ++ show (connPeer conn)

  void . forkIO $ withClient' conn sendMsg
  serverLoop listener

main :: IO ()
main = withListener "pop3" serverLoop
