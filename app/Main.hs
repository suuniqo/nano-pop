{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (forever)
import Control.Monad.Reader (MonadIO (liftIO))
import Control.Exception (SomeException, catch)

import Server
  ( Connection
  , Listener
  , recvClient
  , sendClient
  , withListener
  , withClient
  )

import Log (emit, Severity(..))
import Query (buildQuery, Query, QueryErr)
import Serialize (serialize, excpResponse)

import Session
  ( Phase
  , Auth
  , Authed
  , Trans
  , Update
  , Reply
  , Transition (..)
  , processQuery
  , startSession
  , withAuth
  , finishSession, greeting
  )

import App (App, runApp, buildEnv, BuildErr (..))

sendReply :: Connection -> Reply -> App ()
sendReply conn reply = liftIO $ serialize reply >>= sendClient conn

recvQuery :: Connection -> App (Either QueryErr Query)
recvQuery conn = buildQuery <$> recvClient conn

handleExcp :: Connection -> Either SomeException () -> App ()
handleExcp conn result =
  case result of
    Right ()  -> pure ()
    Left err  -> do
      liftIO $ emit Fatal (show err)
      liftIO $ sendClient conn excpResponse
        `catch` \(err' :: SomeException) ->
          emit Warn ("failed to send error response: " <> show err')

runUpdate :: Connection -> Phase Update -> App ()
runUpdate conn phase = do
  reply <- finishSession phase
  sendReply conn reply

runTrans :: Connection -> Phase Trans -> Reply -> App ()
runTrans conn phase reply = do
  sendReply conn reply

  query <- recvQuery conn

  case processQuery phase query of
    Stay phase' reply' -> runTrans  conn phase' reply'
    Next phase'        -> runUpdate conn phase'
    Term        reply' -> sendReply conn reply'
    Abrt               -> liftIO $ emit Warn "client disconnected"

runAuthed :: Connection -> Phase Authed -> App ()
runAuthed conn auth = do
  result <- withAuth auth (runTrans conn)
  mapM_ (runAuth conn startSession) result

runAuth :: Connection -> Phase Auth -> Reply -> App ()
runAuth conn phase reply = do
  sendReply conn reply

  query <- recvQuery conn

  case processQuery phase query of
    Stay phase' reply' -> runAuth   conn phase' reply'
    Next phase'        -> runAuthed conn phase'
    Term        reply' -> sendReply conn reply'
    Abrt               -> liftIO $ emit Warn "client disconnected"

runSession :: Connection -> App ()
runSession conn = do
  liftIO $ emit Info $ "new client:" <> show conn
  runAuth conn startSession greeting
  liftIO $ emit Info $ "bye client:" <> show conn

listenLoop :: Listener -> App a
listenLoop listener = forever (withClient listener runSession handleExcp)

startServer :: App ()
startServer = do
  liftIO $ emit Info "server ready"
  withListener listenLoop
  
main :: IO ()
main = buildEnv >>= \case
  Left (ConfigErr err) -> emit Fatal $ "failed to build config: " <> show err
  Left (ShadowErr err) -> emit Fatal $ "failed to build shadow: " <> show err
  Right env            -> runApp startServer env
