{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import System.IO
import Text.XML.Pipe
import Network
import Network.XmlPush.HttpPush.Tls
import Network.PeyoTLS.ReadFile

import TestPusher

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 80
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k' <- readKey "certs/localhost.sample_key"
	c' <- readCertificateChain ["certs/localhost.sample_crt"]
	forever  $ do
		(sh, _, _) <- accept soc
		ch <- connectTo "localhost" $ PortNumber 8080
		testPusher (undefined :: HttpPushTls Handle) (Two ch sh) (
			HttpPushArgs "localhost" 8080 "" gtPth wntRspns,
			tlsArgsCl ["TLS_RSA_WITH_AES_128_CBC_SHA"] ca [],
			tlsArgsSv Nothing [(k', c')] )

wntRspns :: XmlNode -> Bool
wntRspns (XmlNode (_, "monologue") _ [] []) = False
wntRspns _ = True

gtPth :: XmlNode -> FilePath
gtPth (XmlNode (_, "father") _ [] []) = "family"
gtPth _ = "others"
