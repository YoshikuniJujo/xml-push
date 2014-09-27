{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import Data.X509
import Data.X509.CertificateStore
import System.IO
import Text.XML.Pipe
import Network
import Network.XmlPush.Http.Tls.Server
import Network.PeyoTLS.ReadFile

import qualified Data.ByteString.Char8 as BSC

import TestPusher

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 443
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/localhost.sample_key"
	c <- readCertificateChain ["certs/localhost.sample_crt"]
	forever $ do
		(h, _, _) <- accept soc
		void . forkIO $ testPusher
			(undefined :: HttpTlsSv Handle) (Two Nothing $ Just h)
			(HttpTlsSvArgs sel
				(httpPullTlsSvArgs ca k c)
				(httpPushTlsArgs ca k c))

sel :: XmlNode -> Mechanism
sel (XmlNode (_, "pull") _ _ _) = Pull
sel (XmlNode (_, "push") _ _ _) = Push
sel _ = error "bad"

httpPullTlsSvArgs ::
	CertificateStore -> CertSecretKey -> CertificateChain -> HttpPullTlsSvArgs h
httpPullTlsSvArgs ca k c = HttpPullTlsSvArgs
	(HttpPullSvArgs isPll endPoll needResponse)
	(tlsArgsSv gtNm (const Nothing)
		["TLS_RSA_WITH_AES_128_CBC_SHA"]
		(Just ca) [(k, c)])

isPll :: XmlNode -> Bool
isPll (XmlNode (_, "poll") _ _ _) = True
isPll _ = False

endPoll :: XmlNode
endPoll = XmlNode (nullQ "nothing") [] [] []

needResponse :: XmlNode -> Bool
needResponse (XmlNode (_, "monologue") _ _ _) = False
needResponse _ = True

gtNm :: XmlNode -> Maybe String
gtNm (XmlNode (_, "name") _ _ [XmlCharData n]) = Just $ BSC.unpack n
gtNm _ = Nothing

httpPushTlsArgs :: CertificateStore ->
	CertSecretKey -> CertificateChain -> HttpPushTlsArgs Handle
httpPushTlsArgs ca k c = HttpPushTlsArgs
	(HttpPushArgs getClientHandle Nothing Nothing gtPth wntRspns)
	(tlsArgsCl "Yoshikuni" True checkCertXml
		["TLS_RSA_WITH_AES_128_CBC_SHA"]
		ca [(k, c)])
	(tlsArgsSv gtNm (const Nothing)
		["TLS_RSA_WITH_AES_128_CBC_SHA"]
		(Just ca) [(k, c)])
		
getClientHandle :: XmlNode -> Maybe (IO Handle, String, Int, FilePath)
getClientHandle (XmlNode (_, "client") [] [] [XmlCharData hn]) = Just (
	connectTo (BSC.unpack hn) $ PortNumber 8080,
	"Yoshikuni",
	8080,
	"/" )
getClientHandle _ = Nothing

wntRspns :: XmlNode -> Bool
wntRspns (XmlNode (_, "monologue") _ [] []) = False
wntRspns _ = True

gtPth :: XmlNode -> FilePath
gtPth (XmlNode (_, "father") _ [] []) = "family"
gtPth _ = "others"

checkCertXml :: XmlNode -> Maybe (SignedCertificate -> Bool)
checkCertXml = const $ Just checkCert

checkCert :: SignedCertificate -> Bool
checkCert = checkFingerprint ["81:9A:16:4A:57:AE:82:92:78:E0"]
