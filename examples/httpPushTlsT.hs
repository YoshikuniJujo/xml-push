{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.List
import Data.Char
import Data.X509
import Data.X509.Validation
import System.IO
import Text.XML.Pipe
import Network
import Network.XmlPush.HttpPush.Tls
import Network.PeyoTLS.ReadFile
import Numeric

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import TestPusher

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 80
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k' <- readKey "certs/localhost.sample_key"
	c' <- readCertificateChain ["certs/localhost.sample_crt"]
	forever  $ do
		(sh, _, _) <- accept soc
		testPusher (undefined :: HttpPushTls Handle) (Two Nothing $ Just sh)
			(HttpPushTlsArgs
				(HttpPushArgs getClientHandle Nothing
					Nothing gtPth wntRspns)
				(tlsArgsCl "Yoshikuni" checkCertXml
					["TLS_RSA_WITH_AES_128_CBC_SHA"]
						ca [(k', c')])
				(tlsArgsSv gtNm (const Nothing)
					["TLS_RSA_WITH_AES_128_CBC_SHA"]
					(Just ca) [(k', c')]) )

checkCertXml :: XmlNode -> Maybe (SignedCertificate -> Bool)
checkCertXml = const $ Just checkCert

checkCert :: SignedCertificate -> Bool
checkCert c = cutFingerprint (getFingerprint c HashSHA256) `elem` [
	"81:9A:16:4A:57:AE:82:92:78:E0" ]

cutFingerprint :: Fingerprint -> String
cutFingerprint (Fingerprint bs) = lastN 29 .
	intercalate ":" . map (map toUpper . flip showHex "") $ BS.unpack bs

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

getClientHandle :: XmlNode -> Maybe (IO Handle, String, Int, FilePath)
getClientHandle (XmlNode (_, "client") [] [] [XmlCharData hn]) = Just (
	connectTo (BSC.unpack hn) $ PortNumber 8080,
	"Yoshikuni",
	8080,
	"" )
getClientHandle _ = Nothing

wntRspns :: XmlNode -> Bool
wntRspns (XmlNode (_, "monologue") _ [] []) = False
wntRspns _ = True

gtPth :: XmlNode -> FilePath
gtPth (XmlNode (_, "father") _ [] []) = "family"
gtPth _ = "others"

gtNm :: XmlNode -> Maybe String
gtNm (XmlNode (_, "name") _ _ [XmlCharData n]) = Just $ BSC.unpack n
gtNm _ = Nothing
