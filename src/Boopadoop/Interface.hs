{-# LANGUAGE OverloadedStrings #-}
module Boopadoop.Interface where

import Boopadoop
import Boopadoop.Plot

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets
import Network.WebSockets

import qualified Data.ByteString.Lazy as BSL

import Data.Word
import Data.Binary.Get
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Concurrent
import Data.IORef

hostInterface :: IO ()
hostInterface = do
    putStrLn $ "http://localhost:8080/"
    let numControls = 10
    m <- newMVar $ V.replicate numControls 0
    _ <- forkIO . run 8080 $ websocketsOr defaultConnectionOptions (wsServe m) $ \req resp -> resp $ case rawPathInfo req of
      "/" -> client
      "/client.html" -> client
      "/client.js" -> js
      _ -> notFound
    _ <- forkIO $ spewValues m
    ks <- newIORef False
    startCoord <- newEmptyMVar
    examp <- fmap fromIntegral <$> dependOnKnob m
    e <- example examp
    _ <- forkIO $ playWavestream startCoord ks (playByReading e)
    _ <- getLine
    writeIORef ks True
    pure ()
  where
    client = responseFile status200 [("Content-Type", "text/html")] "client.html" Nothing
    js = responseFile status200 [("Content-Type", "text/javascript")] "client.js" Nothing
    notFound = responseLBS status404 [("Content-Type", "text/plain")] "404 - Not Found"

data WebSocketPacket = WebSocketPacket
  {controlIndex :: Word32
  ,controlValue :: Word32
  } deriving Show

parseWebSocketPacket :: BSL.ByteString -> Maybe WebSocketPacket
parseWebSocketPacket bs = if BSL.length bs == 8
  then Just $ runGet (WebSocketPacket <$> getWord32le <*> getWord32le) bs
  else Nothing

wsServe :: MVar (V.Vector Word32) -> ServerApp
wsServe m pending = acceptRequest pending >>= takeData m

dependOnKnob :: MVar (V.Vector a) -> IO (IOLacedStream a)
dependOnKnob m = do
  v <- readMVar m
  pure $ IOLacedStream (v V.! 0) (dependOnKnob m)

spewValues :: MVar (V.Vector Word32) -> IO ()
spewValues m = do
  v <- readMVar m
  print v
  threadDelay 100000
  spewValues m

takeData :: MVar (V.Vector Word32) -> Connection -> IO ()
takeData m conn = do
  fromClient <- receiveDataMessage conn
  case fromClient of
    Binary bs -> do
      let mwsp = parseWebSocketPacket bs
      case mwsp of
        Just wsp -> do
          cs <- takeMVar m
          putMVar m $ V.modify (\v -> MV.write v (fromIntegral $ controlIndex wsp) (controlValue wsp)) cs
        Nothing -> pure ()
      print mwsp
    Text _ _ -> putStrLn "Got unexpected Text message from websocket"
  takeData m conn
