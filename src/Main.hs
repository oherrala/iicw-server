{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent               (forkIO)
import           Control.Exception                (bracket)
import           Control.Monad                    (when)

import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  hiding (putStrLn)
import           Data.Serialize
import           Data.Word                        (Word16)

import           Network.Socket                   hiding (recv, recvFrom, send,
                                                   sendTo)
import           Network.Socket.ByteString

import           Prelude                          hiding (getContents)

import           System.IO                        hiding (hGetContents)


debugFlag :: Bool
debugFlag = True

debug :: String -> IO ()
debug str = when debugFlag $ putStrLn str


main :: IO ()
main = do
  sock <- tcpListener 9999
  acceptLoop sock mainHandler


mainHandler :: Socket -> SockAddr -> IO ()
mainHandler sock _ = withSocketHandle sock $ \handle -> do
  debug "In mainHandler"
  result <- parseWith (hGetSome handle 128) heloParser empty
  case eitherResult result of
    Left err -> print $ "Parsing error: " ++ err
    Right clientPort -> do
      udpSock <- udpListener
      _       <- forkIO $ datagramHandler udpSock clientPort
      return ()


-- | Handle UDP connection with client
datagramHandler :: Socket -> Word16 -> IO ()
datagramHandler sock port = do
  debug "In udpHandler"
  (bytes, address) <- recvFrom sock 4096
  -- FIXME: validate ports

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
  , udpContent :: ByteString
  }


-- | Parse UDP message
udpParser :: Get UDPMessage
udpParser = do
  eom       <- getBool
  ack       <- getBool
  bytesLeft <- getWord16be
  str       <- getByteString (64 - (8+8+16))
  return $! UDPMessage eom ack bytesLeft str

  where
    getBool = do
      value <- getWord8
      case value of
        1 -> return True
        _ -> return False
