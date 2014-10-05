{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS

import Control.Monad
import Control.Concurrent
import Data.X509
import System.IO
import Text.XML.Pipe
import Network
import Network.PeyoTLS.ReadFile

import Network.XmlPush
import Network.XmlPush.Xmpp.Tls.Server
import TestPusherT

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 5222
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/localhost.sample_key"
	c <- readCertificateChain ["certs/localhost.sample_crt"]
	forever $ do
		(h, _, _) <- accept soc
		void . forkIO $ testPusher (undefined :: XmppTlsServer Handle)
			(One h) (XmppTlsServerArgs
				(XmppServerArgs "localhost" pswds
					(const True) (const True))
				(TlsArgs (const Nothing) checkCertXml
					["TLS_RSA_WITH_AES_128_CBC_SHA"]
					(Just ca) [(k, c)]))

pswds :: [(BS.ByteString, BS.ByteString)]
pswds = [
	("yoshikuni", "password"),
	("yoshio", "password")
	]

checkCertXml :: XmlNode -> Maybe (SignedCertificate -> Bool)
checkCertXml = const $ Just checkCert

checkCert :: SignedCertificate -> Bool
checkCert = checkFingerprint ["81:9A:16:4A:57:AE:82:92:78:E0"]
