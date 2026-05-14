{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Server
  ( Listener
  , withListener
  , Connection (..)
  , withClient
  , ClientErr (..)
  , sendClient
  , recvClient
  ) where

import GHC.IO.Exception (IOErrorType(Interrupted, ResourceVanished, ResourceExhausted))

import Network.Socket
  ( withSocketsDo
  , AddrInfo (addrFlags, addrSocketType, addrAddress)
  , defaultHints
  , AddrInfoFlag (AI_PASSIVE)
  , SocketType (Stream)
  , getAddrInfo
  , Socket
  , openSocket
  , ServiceName
  , setSocketOption
  , SocketOption (ReuseAddr)
  , bind
  , close
  , listen
  , SockAddr
  , accept
  )

import Control.Concurrent (threadDelay)

import Control.Exception (throwIO, bracketOnError, catch, handle, SomeException)
import qualified UnliftIO.Concurrent as UIOC
import qualified UnliftIO as UIO

import System.Random

import Error
  ( noCandidatesErr
  , annotate
  , classify
  , Oper (..)
  , SysErr (seOper)
  )
import Log (emit, Severity (Warn))
import Network.Socket.ByteString (sendAll, recv)
import Data.ByteString (ByteString)
import Constant (queryMaxLen)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import qualified Data.ByteString.Char8 as BS
import System.Timeout (timeout)
import Data.Functor
import App (App, AppEnv (config))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Config (Config(network), NetworkConfig (backoffMin, readChunk, backoffMax, listenBacklog, port, idleTimeout))
import Control.Monad.RWS (asks)

tryGetAddrInfo :: AddrInfo -> ServiceName -> IO [AddrInfo]
tryGetAddrInfo hints host = annotate OpAddrInfo call
  where call = getAddrInfo (Just hints) Nothing (Just host)

tryOpenSock :: AddrInfo -> IO Socket
tryOpenSock addr = annotate (OpOpenSock $ addrAddress addr) call
  where call = openSocket addr

trySetSockOpt :: Socket -> SocketOption -> IO ()
trySetSockOpt sock opt = annotate OpSockOpt call
  where call = setSocketOption sock opt 1

tryBindSock :: AddrInfo -> Socket -> IO ()
tryBindSock addr sock = annotate (OpBind $ addrAddress addr) call
  where call = bind sock (addrAddress addr)

tryListenSock :: AddrInfo -> Socket -> Int -> IO ()
tryListenSock addr sock queue = annotate (OpListen $ addrAddress addr) call
  where call = listen sock queue

tryAcceptClient :: Socket -> IO (Socket, SockAddr)
tryAcceptClient listener = annotate OpAccept call
  where call = accept listener

trySendAll :: SockAddr -> Socket -> ByteString -> IO ()
trySendAll addr sock msg = annotate (OpSend addr) call
  where call = sendAll sock msg

tryRecv :: SockAddr -> Socket -> Int -> IO ByteString
tryRecv addr sock chunkSize = annotate (OpRecv addr) call
  where call = recv sock chunkSize

tryTimeout :: Int -> IO a -> IO (Maybe a)
tryTimeout time action = annotate OpTimeout call
  where call = timeout time action

resolve :: ServiceName -> IO [AddrInfo]
resolve = tryGetAddrInfo
  defaultHints
    { addrFlags = [AI_PASSIVE]
    , addrSocketType = Stream
    }

tryCandidate :: AddrInfo -> IO (Socket, AddrInfo)
tryCandidate addr = bracketOnError (tryOpenSock addr) close setupSock
  where
    setupSock sock = do
      trySetSockOpt sock ReuseAddr
      tryBindSock addr sock
      pure (sock, addr)

bindCandidate :: [AddrInfo] -> IO (Socket, AddrInfo)
bindCandidate []     = throwIO noCandidatesErr
bindCandidate [a]    = tryCandidate a
bindCandidate (a:as) = tryCandidate a `catch` \(err :: SysErr) -> do
  case seOper err of
    OpSockOpt -> throwIO err
    _         -> do
      emit Warn (show err)
      bindCandidate as

type Listener = Socket

acquireListener :: ServiceName -> Int -> IO Listener
acquireListener host backlog = withSocketsDo $ do
  addrs        <- resolve host
  (sock, addr) <- bindCandidate addrs

  tryListenSock addr sock backlog

  pure sock

releaseListener :: Listener -> IO ()
releaseListener = close

withListener :: (Listener -> App a) -> App a
withListener action = do
  host    <- asks (port . network . config)
  backlog <- asks (listenBacklog . network . config)
  UIO.bracket (liftIO $ acquireListener host backlog) (liftIO . releaseListener) action

data Connection = Connection
  { connSock :: Socket
  , connPeer :: SockAddr
  , connBuff :: IORef ByteString
  }

instance Show Connection where
  show = show . connPeer

data RetryType
  = Immediate
  | Backoff
  | Stop

retryBackoff :: Int -> Int -> (SysErr -> RetryType) -> IO a -> IO a
retryBackoff delayMin delayMax shouldRetry action = go delayMin
  where
    go delay = action `catch` \(err :: SysErr) ->
      case shouldRetry err of
        Stop -> throwIO err
        Immediate -> do
          emit Warn (show err)
          go delay
        Backoff -> do
          emit Warn (show err)
          jitter <- randomRIO (0, delay `div` 2)
          threadDelay (delay `div` 2 + jitter)
          go (min (delay*2) delayMax)

shouldRetryAccept :: SysErr -> RetryType
shouldRetryAccept err =
  case classify err of
    Interrupted        -> Immediate
    ResourceVanished   -> Backoff
    ResourceExhausted  -> Backoff
    _                  -> Stop

acquireClient :: Listener -> App Connection
acquireClient listener =
  do
    delayMin     <- asks (backoffMin . network . config)
    delayMax     <- asks (backoffMax . network . config)

    (sock, addr) <- liftIO (retryAccept delayMin delayMax)
    buff         <- liftIO (newIORef BS.empty)

    pure $ Connection sock addr buff
  where
    retryAccept bmin bmax =
      retryBackoff bmin bmax
      shouldRetryAccept
      (tryAcceptClient listener)

releaseClient :: Connection -> IO ()
releaseClient = close . connSock

type Handler = Connection -> Either SomeException () -> App ()

withClient :: Listener -> (Connection -> App ()) -> Handler -> App ()
withClient listener action handleExcp = do
  conn <- acquireClient listener

  let release = liftIO (releaseClient conn)
  let handleExcp' excp = handleExcp conn excp `UIO.finally` release

  void $ UIOC.forkFinally (action conn) handleExcp'

sendClient :: Connection -> ByteString -> IO ()
sendClient conn = trySendAll (connPeer conn) (connSock conn)

shouldRetryRecv :: SysErr -> RetryType
shouldRetryRecv err =
  case classify err of
    Interrupted -> Immediate
    _           -> Stop

data ClientErr
  = TooLong
  | Timeout
  | Disconn

instance Show ClientErr where
  show err = case err of
    TooLong -> "query is too long"
    Timeout -> "autologout timeout expired"
    Disconn -> "client disconnected"

data FetchResult
  = Chunk ByteString
  | PeerTimeout
  | PeerDisconn

fetchChunk :: Connection -> App FetchResult
fetchChunk conn = do
  chunkSize <- asks (readChunk   . network . config)
  delayMin  <- asks (backoffMin  . network . config)
  delayMax  <- asks (backoffMax  . network . config)
  time      <- asks (idleTimeout . network . config)

  liftIO $ handle recvErr $
    tryTimeout time (retryRecv delayMin delayMax chunkSize)
    <&> maybe PeerTimeout Chunk
  where
    retryRecv bmin bmax = retryBackoff bmin bmax shouldRetryRecv . tryRecv (connPeer conn) (connSock conn)
    recvErr err = case classify err of
      ResourceVanished -> pure PeerDisconn
      _                -> throwIO err

data SplitResult
  = Complete ByteString ByteString
  | Incomplete
  | Overflow (Maybe ByteString)

stripCRLF :: ByteString -> ByteString
stripCRLF = BS.drop 2

splitLine :: ByteString -> SplitResult
splitLine buff =
  case BS.breakSubstring "\r\n" buff of
    (line, rest)
      | BS.length line + 2 > queryMaxLen -> evalOverflow rest
      | BS.null rest                      -> Incomplete
      | otherwise                         -> Complete line (stripCRLF rest)
  where
    evalOverflow rest
      | BS.null rest = Overflow Nothing
      | otherwise    = Overflow $ Just (stripCRLF rest)

drainLine :: Connection -> App (Either ClientErr ByteString)
drainLine conn = fetchChunk conn >>= \case
  PeerDisconn -> pure (Left Disconn)
  PeerTimeout -> pure (Left Timeout)
  Chunk bytes ->
    case BS.breakSubstring "\r\n" bytes of
      (_, rest) | not (BS.null rest) -> liftIO $ writeIORef (connBuff conn) (stripCRLF rest) $> Left TooLong
      _                              -> drainLine conn

recvClient :: Connection -> App (Either ClientErr ByteString)
recvClient conn = liftIO (readIORef (connBuff conn)) >>= go
  where
    go buff = do
      case splitLine buff of
        Overflow Nothing     -> drainLine conn
        Overflow (Just rest) -> liftIO $ writeIORef (connBuff conn) rest $> Left TooLong
        Complete line rest   -> liftIO $ writeIORef (connBuff conn) rest $> Right line
        Incomplete -> fetchChunk conn >>= \case
          PeerDisconn -> pure (Left Disconn)
          PeerTimeout -> pure (Left Timeout)
          Chunk bytes -> go (buff <> bytes)
