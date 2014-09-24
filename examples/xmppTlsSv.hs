{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS

import Control.Monad
import Control.Concurrent
import System.IO
import Network
import Network.PeyoTLS.ReadFile

import Network.XmlPush
import Network.XmlPush.Xmpp.Tls.Server
import TestPusher

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
				(TlsArgs (const Nothing) (const Nothing)
					["TLS_RSA_WITH_AES_128_CBC_SHA"]
					(Just ca) [(k, c)]))

pswds :: [(BS.ByteString, BS.ByteString)]
pswds = [
	("yoshikuni", "password"),
	("yoshio", "password")
	]
