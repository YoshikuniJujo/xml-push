{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import System.IO
import Text.XML.Pipe
import Network
import Network.XmlPush.HttpPush.Tls
import Network.PeyoTLS.ReadFile

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
				(tlsArgsCl "Yoshikuni" (const Nothing)
					["TLS_RSA_WITH_AES_128_CBC_SHA"]
						ca [(k', c')])
				(tlsArgsSv gtNm (const Nothing)
					["TLS_RSA_WITH_AES_128_CBC_SHA"]
					(Just ca) [(k', c')]) )

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
