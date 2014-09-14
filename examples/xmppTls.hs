{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import System.IO
import System.Environment
import Text.XML.Pipe
import Network
import Network.XMPiPe.Core.C2S.Client
import Network.PeyoTLS.ReadFile

import qualified Data.ByteString.Char8 as BSC

import Network.XmlPush.Xmpp.Tls
import TestPusher

main :: IO ()
main = do
	me : ps : you : _ <- map BSC.pack <$> getArgs
	h <- connectTo "localhost" $ PortNumber 5222
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/yoshikuni.sample_key"
	c <- readCertificateChain ["certs/yoshikuni.sample_crt"]
	testPusher (undefined :: XmppTls Handle) (One h) (XmppTlsArgs
		(XmppArgs ["EXTERNAL", "SCRAM-SHA-1", "DIGEST-MD5", "PLAIN"]
			(toJid me) ps (toJid you) iNdRspns wntRspns)
		(TlsArgs "localhost" ["TLS_RSA_WITH_AES_128_CBC_SHA"] ca [(k, c)]) )

wntRspns :: XmlNode -> Bool
wntRspns (XmlNode (_, "monologue") _ [] []) = False
wntRspns (XmlNode (_, "message") _ [] []) = False
wntRspns _ = True

iNdRspns :: XmlNode -> Bool
iNdRspns (XmlNode (_, "message") _ [] []) = False
iNdRspns _ = True
