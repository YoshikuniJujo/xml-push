{-# LANGUAGE TupleSections, TypeFamilies, FlexibleContexts #-}

module TestPusher (
	XmlPusher(..), Zero(..), One(..), Two(..),
	testPusher, checkFingerprint) where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.Maybe
import Data.List
import Data.Char
import Data.HandleLike
import Data.Pipe
import Data.Pipe.ByteString
import Data.X509
import Data.X509.Validation
import System.IO
import Text.XML.Pipe
import Numeric
import Network.PeyoTLS.Client (ValidateHandle)

import qualified Data.ByteString as BS

import Network.XmlPush

testPusher :: (XmlPusher xp, ValidateHandle h, HandleMonad h ~ IO) =>
	xp h -> NumOfHandle xp h -> PusherArgs xp h -> IO ()
testPusher tp hs as = do
	xp <- (`asTypeOf` tp) <$> generate hs as
	void . forkIO . runPipe_ $ readFrom xp
		=$= convert (xmlString . (: []))
		=$= toHandle stdout
	runPipe_ $ fromHandle stdin
		=$= xmlEvent
		=$= convert fromJust
		=$= xmlNode []
		=$= writeTo xp

checkFingerprint :: [String] -> SignedCertificate -> Bool
checkFingerprint fps c = cutFingerprint (getFingerprint c HashSHA256) `elem` fps

cutFingerprint :: Fingerprint -> String
cutFingerprint (Fingerprint bs) = lastN 29 .
	intercalate ":" . map (map toUpper . flip showHex "") $ BS.unpack bs

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs
