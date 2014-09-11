{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Text.XML.Pipe
import Network
import Network.PeyoTLS.ReadFile

import Network.XmlPush.HttpPush.Tls
import TestPusher

main :: IO ()
main = do
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k' <- readKey "certs/localhost.sample_key"
	c' <- readCertificateChain ["certs/localhost.sample_crt"]
	ch <- connectTo "localhost" $ PortNumber 80
	soc <- listenOn $ PortNumber 8080
	(sh, _, _) <- accept soc
	testPusher (undefined :: HttpPushTls Handle) (Two ch sh) (
		HttpPushArgs "localhost" 80 "" gtPth wntRspns,
		tlsArgsCl ca [], tlsArgsSv Nothing [(k', c')] )

wntRspns :: XmlNode -> Bool
wntRspns (XmlNode (_, "monologue") _ [] []) = False
wntRspns _ = True

gtPth :: XmlNode -> FilePath
gtPth (XmlNode (_, "father") _ [] []) = "family"
gtPth _ = "others"
