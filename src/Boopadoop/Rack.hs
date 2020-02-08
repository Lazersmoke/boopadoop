{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
module Boopadoop.Rack where

import Boopadoop
import GHC.Generics
import Data.Aeson
import Data.Text(Text)
import Data.Foldable
import Data.Word
import Control.Monad

import Foreign.C
import Foreign.Ptr
--import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Alloc

{-
modules": [
      "slug": "VCO",
      "name": "VCO-1",
      "description": "Voltage-controlled oscillator",
      "tags": [
        "VCO",
        "Polyphonic"
      ]
-}

data RackStruct (a :: k) where
  RackStruct :: RackStruct a

foreign import ccall "sin" c_sin :: CDouble -> CDouble
foreign import ccall "windows.h LoadLibraryW" c_windowsLoadLibrary :: CWString -> IO (Ptr ())
foreign import ccall "hs_GetDataPointer" c_GetDataPointer :: Ptr (RackStruct "Plugin") -> IO CLLong
foreign import ccall unsafe "hs_LoadRackPlugin" c_LoadRackPlugin :: CWString -> IO (Ptr (RackStruct "Plugin"))
foreign import ccall unsafe "hs_GetInitCallback" c_GetInitCallback :: Ptr (RackStruct "Plugin") -> IO (FunPtr (Ptr (RackStruct "Plugin") -> IO ()))
--foreign import ccall unsafe "hs_GetModelBySlug" c_GetModelBySlug :: Ptr (RackStruct "Plugin") -> IO (Ptr (RackStruct "Model"))
--foreign import ccall "windows.h SetDefaultDllDirectories" c_windowsSetDefaultDllDirectories :: CUInt -> IO CBool
--foreign import ccall "windows.h AddDllDirectory" c_windowsAddDLLDirectory :: CWString -> IO CBool
foreign import ccall "windows.h GetProcAddress" c_windowsGetProcAddress :: Ptr () -> CString -> IO (FunPtr (Ptr () -> IO ()))
foreign import ccall "windows.h GetLastError" c_windowsGetLastError :: IO CInt
foreign import ccall "windows.h SetErrorMode" c_windowsSetErrorMode :: CUInt -> IO CUInt
foreign import ccall "windows.h strlen" c_strlen :: CString -> IO CInt
--foreign import ccall unsafe "dynamic" callInitCallback :: FunPtr (Ptr (RackStruct "Plugin") -> IO ()) -> Ptr (RackStruct "Plugin") -> IO ()

windowsLoadLibrary :: String -> IO ()
windowsLoadLibrary s = do
  plugin <- withCWString s c_LoadRackPlugin
  putStrLn $ "Got plugin hask: " ++ show plugin
  putStrLn =<< fmap (concatMap hexify) (forM [0..128] (peekElemOff (castPtr plugin :: Ptr Word8)))
  initCallback <- c_GetInitCallback plugin
  putStrLn $ "Got initCallback hask: " ++ show initCallback
  --putStrLn =<< fmap (concatMap hexify) (forM [0..128] (peekElemOff (castPtr initCallback :: Ptr Word8)))
  dataPtr <- c_GetDataPointer plugin
  putStrLn $ "Got datapointer hask: " ++ show dataPtr
  --putStrLn =<< fmap (concatMap hexify) (forM [0..128] (peekElemOff (castPtr dataPtr :: Ptr Word8)))
  --callInitCallback initCallback plugin
  putStrLn $ "all done hask"
  where
    hexify x = [hex !! (fromIntegral x `div` 16), hex !! (fromIntegral x `mod` 16),' ']
    hex = "0123456789abcdef"
{-
  memForPlugin <- mallocBytes 2048
  mkFun fp memForPlugin
  print =<< Prelude.reverse <$> foldM (\xs o -> (:xs) <$> peekByteOff memForPlugin o) ([] :: [Word32]) [0,4..(2048`div`4)]
  memForFirstModule <- peekByteOff memForPlugin 8
  print =<< Prelude.reverse <$> foldM (\xs o -> (:xs) <$> peekByteOff memForFirstModule o) ([] :: [Word8]) [0,4..(2048`div`4)]
  putStrLn "All Done!"
  --free memForPlugin
  --putStrLn "Freed!"
-}

windowsStrLen :: String -> IO CInt
windowsStrLen s = withCString s c_strlen

testPluginJSON :: IO (Maybe (PluginManifest,[Result Module]))
testPluginJSON = do
  manifest <- decodeFileStrict $ "plugin.json"
  pure $ fmap (\mani -> (mani,toList . fmap fromJSON $ modules mani)) manifest

data PluginManifest = PluginManifest
  {slug :: Text
  ,version :: Text
  ,license :: Text
  ,name :: Text
  ,brand :: Text
  ,author :: Text
  ,authorEmail :: Text
  ,pluginUrl :: Text
  ,authorUrl :: Text
  ,manualUrl :: Text
  ,sourceUrl :: Text
  ,changelogUrl :: Text
  ,modules :: Array
  } deriving (Generic,Show)

instance ToJSON PluginManifest where
instance FromJSON PluginManifest where

data Module = Module
  {slug :: Text
  ,name :: Text
  ,description :: Text
  ,tags :: Array
  } deriving (Generic,Show)

instance ToJSON Module where
instance FromJSON Module where
