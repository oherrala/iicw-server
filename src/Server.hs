{-# LANGUAGE OverloadedStrings #-}

module Server where

import           Control.Concurrent               (forkIO)
import           Control.Exception                (bracket)
import           Control.Monad                    (when)

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as BS
import           Data.Serialize
import           Data.Word                        (Word16)

import           Network.Socket                   hiding (recv, recvFrom, send,
                                                   sendTo)
import           Network.Socket.ByteString

import           Prelude                          hiding (getContents, take)

import           System.IO                        hiding (hGetContents)


debugFlag :: Bool
debugFlag = True

debug :: String -> IO ()
debug str = when debugFlag . putStrLn $ "DEBUG: " ++ str


mainHandler :: Socket -> SockAddr -> IO ()
mainHandler sock _ = withSocketHandle sock $ \handle -> do
  debug "In mainHandler"
  result <- parseWith (BS.hGetSome handle 128) heloParser BS.empty
  case eitherResult result of
    Left err         -> print $ "Parse error: " ++ err
    Right clientPort -> spawnUDP clientPort

  where
    spawnUDP port = do
      udpSock <- udpListener
      -- FIXME ThreadId lost here
      _       <- forkIO $ datagramHandler udpSock port
      return ()


-- | Handle UDP connection with client
datagramHandler :: Socket -> Word16 -> IO ()
datagramHandler sock clientPort = do
  debug "In udpHandler"
  (bytes, address) <- recvFrom sock 4096

  -- Validate UDP ports
  let (SockAddrInet6 (PortNum clientPort') _ _ _) = address
    in when (clientPort /= clientPort') $
       error $ "Client UDP port " ++ show clientPort' ++ " not what was agreed"

  case runGet getUDPMessage bytes of
    Left err -> error $ "Parse error: " ++ err
    Right (UDPMessage eom ack bytesLeft content) ->
      if content == "Ekki-ekki-ekki-ekki-PTANG."
      then print "UDP secret handshake received"
      else error "Invalid UDP secret handshake"

  return ()


-- | Create TCP socket and start listening on given port
tcpListener :: PortNumber -> IO Socket
tcpListener = _listener Stream


-- | Create UDP socket and start listening on next available port
udpListener :: IO Socket
udpListener = _listener Datagram aNY_PORT


-- | Helper to create listening socket
_listener :: SocketType -> PortNumber -> IO Socket
_listener type' port = do
  sock <- socket AF_INET6 type' defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet6 port 0 iN6ADDR_ANY 0)
  when (type' == Stream) $ listen sock 5

  port' <- socketPort sock
  when (type' == Stream) $
    putStrLn $ "Listening TCP port " ++ show port'
  when (type' == Datagram) $
    putStrLn $ "Listening UDP port " ++ show port'

  return sock


-- | Infinite loop to accept connections on given socket.
--
-- Accepted connections are given to 'handler'
acceptLoop :: Socket -> (Socket -> SockAddr -> IO ()) -> IO ()
acceptLoop sock handler = do
  (conn, addr) <- accept sock
  putStrLn $ "Connection from: " ++ show addr
  -- FIXME: We lose ThreadId here
  _ <- forkIO $ handler conn addr
  acceptLoop sock handler


-- | Use socket as a file handle
--
-- File handle and socket are closed after returning from this
-- function.
withSocketHandle :: Socket -> (Handle -> IO c) -> IO c
withSocketHandle sock = bracket hOpen hClose
  where
    hOpen = socketToHandle sock ReadWriteMode


-- | Parse "HELO" string
--
-- Returns parsed port number as Word16
heloParser :: Parser Word16
heloParser = do
  -- FIXME be more liberal with whitespace?
  _    <- string "HELO "
  port <- decimal
  _    <- string "\r\n"
  return port


data UDPMessage = UDPMessage {
  -- | True if End of Message
  isEOM        :: Bool
  -- | True if last message was received OK
  , isACK      :: Bool
  -- | Bytes remaining in multipart message
  , remaining  :: Word16
  -- | Received Message
  , udpContent :: BS.ByteString
  } deriving (Eq, Show)


-- | Parse UDP message
getUDPMessage :: Get UDPMessage
getUDPMessage = do
  eom       <- getBool
  ack       <- getBool
  bytesLeft <- getWord16be
  str       <- getByteString 60
  return $! UDPMessage eom ack bytesLeft str

-- | Convert UDP message to ByteString
putUDPMessage :: Putter UDPMessage
putUDPMessage (UDPMessage eom ack bytesLeft content) = do
  putBool eom
  putBool ack
  putWord16be bytesLeft
  putByteString . BS.take 60 $ content


-- | Parse C-style booleans
getBool :: Get Bool
getBool = do
  value <- getWord8
  case value of
    1 -> return True
    _ -> return False

putBool :: Putter Bool
putBool bool = if bool then putWord8 1 else putWord8 0
