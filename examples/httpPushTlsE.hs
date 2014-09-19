{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.STM
import System.IO
import Text.XML.Pipe
import Network
import Network.PeyoTLS.ReadFile
import Network.XmlPush.HttpPush.Tls

import qualified Data.ByteString.Char8 as BSC

import TestPusher

main :: IO ()
main = do
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k' <- readKey "certs/yoshikuni.sample_key"
	c' <- readCertificateChain ["certs/yoshikuni.sample_crt"]
	ch <- connectTo "localhost" $ PortNumber 80
	vch <- atomically . newTVar $ Just ch
	soc <- listenOn $ PortNumber 8080
	(sh, _, _) <- accept soc
	testPusher (undefined :: HttpPushTls Handle) (Two vch sh) (HttpPushTlsArgs
		(HttpPushArgs "localhost" 80 "" gtPth wntRspns)
		(tlsArgsCl "localhost" ["TLS_RSA_WITH_AES_128_CBC_SHA"] ca
			[(k', c')])
		(tlsArgsSv gtNm (const Nothing)
			["TLS_RSA_WITH_AES_128_CBC_SHA"]
			(Just ca) [(k', c')]) )

wntRspns :: XmlNode -> Bool
wntRspns (XmlNode (_, "monologue") _ [] []) = False
wntRspns _ = True

gtPth :: XmlNode -> FilePath
gtPth (XmlNode (_, "father") _ [] []) = "family"
gtPth _ = "others"

gtNm :: XmlNode -> Maybe String
gtNm (XmlNode (_, "name") _ _ [XmlCharData n]) = Just $ BSC.unpack n
gtNm _ = Nothing
