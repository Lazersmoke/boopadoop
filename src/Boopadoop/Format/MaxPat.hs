{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
module Boopadoop.Format.MaxPat where

import GHC.Generics
--import GHC.TypeLits
import Data.Aeson
import Data.Aeson.Types
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Data.Maybe
import qualified Data.Vector as V
--import Data.Graph
import qualified Control.Monad.Fail as Fail

maxPatIdToIndex :: MaxPat -> PatchBoxId -> Int
maxPatIdToIndex mp pbId' = fromJust $ V.findIndex (\pb -> pbId' == pbId pb) (mpBoxes mp)

getInlets :: PatchBox -> [PortSpec]
getInlets pb = fmap (PortSpec (pbId pb)) [0.. numinlets pb - 1]

getOutlets :: PatchBox -> [PortSpec]
getOutlets pb = fmap (PortSpec (pbId pb)) [0.. numoutlets pb - 1]

data Vec (is :: [*]) where
  VecNil :: Vec '[]
  VecCons :: a -> Vec xs -> Vec (a ': xs)

data WireBox (is :: [*]) (os :: [*]) = WireBox
  {process :: Vec is -> (WireBox is os,Vec os)
  }

constWireBox :: a -> WireBox '[] '[a]
constWireBox x = WireBox $ \VecNil -> (constWireBox x,VecCons x VecNil)

emptyWireBox :: WireBox '[] '[]
emptyWireBox = WireBox (emptyWireBox,)

composeWireBox :: WireBox b c -> WireBox a b -> WireBox a c
composeWireBox two one = WireBox $ \a -> let (one',b) = process one a in let (two',c) = process two b in (composeWireBox two' one',c)

pureWireBox :: (a -> b) -> WireBox '[a] '[b]
pureWireBox f = WireBox $ \(VecCons a VecNil) -> (pureWireBox f,VecCons (f a) VecNil)

data MaxAtom = MaxBang | MaxFloat Float | MaxInt Int | MaxList [MaxAtom]

data MaxBoxImpl i o = MaxBoxImpl
  {implementedMaxclass :: Text
  ,wireBoxImplementation :: WireBox i o
  }

maxDeltaWave :: Num a => MaxBoxImpl '[a] '[a]
maxDeltaWave = MaxBoxImpl "delta~" (deltaWB 0)
  where
    deltaWB :: Num a => a -> WireBox '[a] '[a]
    deltaWB l = WireBox $ \(VecCons x VecNil) -> (deltaWB x,VecCons (x - l) VecNil)

data MaxPat = MaxPat
  {mpBoxes :: V.Vector PatchBox
  ,mpLines :: V.Vector PatchLine
  }deriving Show

instance FromJSON MaxPat where
  parseJSON = doubleNestedObjectNamed "patcher" (\p -> MaxPat <$> p .: "boxes" <*> p .: "lines")

doubleNestedObjectNamed :: FromJSON a => Text -> (Object -> Parser a) -> Value -> Parser a
doubleNestedObjectNamed objName f = withObject ("Outer " <> show objName) (\outer -> outer .: objName >>= withObject ("Inner " <> show objName) f)

type PatchBoxId = Int

data PatchBox = PatchBox
  {fullInfo :: Object
  ,maxclass :: Text
  ,numinlets :: Int
  ,numoutlets :: Int
  ,pbId :: PatchBoxId
  } deriving (Generic,Show)

--instance ToJSON PatchBox where
instance FromJSON PatchBox where
  parseJSON = doubleNestedObjectNamed "box" (\p -> PatchBox <$> pure p <*> p .: "maxclass" <*> p .: "numinlets" <*> p .: "numoutlets" <*> (p .: "id" >>= parseObjectNumberFromName)) 

data PortSpec = PortSpec
  {boxId :: PatchBoxId
  ,portNumber :: Int
  }deriving Show

instance FromJSON PortSpec where
  parseJSON = withArray "PortSpec" (\v -> PortSpec <$> parseJSON (v V.! 0) <*> parseJSON (v V.! 1))

--instance ToJSON PortSpec where
  --toJSON ps = Array $ fromList [String $ boxId ps,Number . fromIntegral $ portNumber ps]

data PatchLine = PatchLine
  {source :: PortSpec
  ,destination :: PortSpec
  } deriving (Generic,Show)

--instance ToJSON PatchLine where
instance FromJSON PatchLine where
  parseJSON = doubleNestedObjectNamed "patchline" (\p -> PatchLine <$> p .: "source" <*> p .: "destination") 

loadPatch :: IO (Either String MaxPat)
loadPatch = eitherDecodeFileStrict @MaxPat "pat.maxpat"

parseObjectNumberFromName :: Fail.MonadFail m => Text -> m PatchBoxId
parseObjectNumberFromName t = case Text.stripPrefix "obj-" t of
  Nothing -> Fail.fail "Not obj- prefixed"
  Just x -> case Text.decimal x of
    Left s -> Fail.fail s
    Right (i,_) -> pure i
