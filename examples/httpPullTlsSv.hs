{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import System.IO
import Text.XML.Pipe
import Network
import Network.PeyoTLS.ReadFile

import Network.XmlPush.HttpPull.Tls.Server
import TestPusher

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 443
	k <- readKey "certs/localhost.sample_key"
	c <- readCertificateChain ["certs/localhost.sample_crt"]
	forever $ do
		(h, _, _) <- accept soc
		void . forkIO $ testPusher (undefined :: HttpPullTlsSv Handle)
			(One h) (isPoll, endPoll, TlsArgs Nothing [(k, c)])

isPoll :: XmlNode -> Bool
isPoll (XmlNode (_, "poll") _ _ _) = True
isPoll _ = False

endPoll :: XmlNode
endPoll = XmlNode (nullQ "nothing") [] [] []
