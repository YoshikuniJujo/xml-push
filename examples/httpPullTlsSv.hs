{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import System.IO
import Text.XML.Pipe
import Network
import Network.PeyoTLS.ReadFile

import Network.XmlPush.HttpPull.Tls.Server
import TestPusher

import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 443
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/localhost.sample_key"
	c <- readCertificateChain ["certs/localhost.sample_crt"]
	forever $ do
		(h, _, _) <- accept soc
		void . forkIO $ testPusher (undefined :: HttpPullTlsSv Handle)
			(One h) (isPoll, endPoll, TlsArgs gtNm
				["TLS_RSA_WITH_AES_128_CBC_SHA"] (Just ca) [(k, c)])

isPoll :: XmlNode -> Bool
isPoll (XmlNode (_, "poll") _ _ _) = True
isPoll _ = False

endPoll :: XmlNode
endPoll = XmlNode (nullQ "nothing") [] [] []

gtNm :: XmlNode -> Maybe String
gtNm (XmlNode (_, "name") _ _ [XmlCharData n]) = Just $ BSC.unpack n
gtNm _ = Nothing
