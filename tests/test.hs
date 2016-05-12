module Main where

import           Test.Tasty
import           Test.Tasty.QuickCheck     as QC

import           Server

import           Data.ByteString.Arbitrary
import           Data.Serialize


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [
    QC.testProperty "putUDPMessage <-> getUDPMessage" $
    \msg ->
      (msg :: UDPMessage) == fromEither (runGet getUDPMessage $ runPut (putUDPMessage msg))
  ]


fromEither :: Either a b -> b
fromEither (Right v) = v
fromEither _         = error "fromEither"


instance Arbitrary UDPMessage where
  arbitrary = do
    eom       <- arbitrary
    ack       <- arbitrary
    bytesLeft <- arbitrary
    bytes     <- fastRandBs 60 -- It's stypid to limit this to 60..
    return $ UDPMessage eom ack bytesLeft bytes
