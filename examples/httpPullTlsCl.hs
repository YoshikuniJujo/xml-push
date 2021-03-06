{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Text.XML.Pipe
import Network
import Network.PeyoTLS.ReadFile

import Network.XmlPush.HttpPull.Tls.Client
import TestPusher

main :: IO ()
main = do
	h <- connectTo "localhost" $ PortNumber 443
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/yoshikuni.sample_key"
	c <- readCertificateChain ["certs/yoshikuni.sample_crt"]
	testPusher (undefined :: HttpPullTlsCl Handle) (One h) (HttpPullTlsClArgs
		(HttpPullClArgs "localhost" 443 "/" gtPth mkPoll
			pendingQ (Just 10000000) drtn)
		(TlsArgs "localhost" True (const Nothing)
			["TLS_RSA_WITH_AES_128_CBC_SHA"] ca [(k, c)]) )

mkPoll :: IO XmlNode
mkPoll = return $ XmlNode (nullQ "poll") [] [] []

pendingQ :: XmlNode -> Bool
pendingQ (XmlNode (_, "nothing") _ [] []) = False
pendingQ _ = True

drtn :: XmlNode -> Maybe Int
drtn (XmlNode (_, "slow") _ [] []) = Just 20000000
drtn (XmlNode (_, "medium") _ [] []) = Just 10000000
drtn (XmlNode (_, "fast") _ [] []) = Just 5000000
drtn (XmlNode (_, "very_fast") _ [] []) = Just 1000000
drtn (XmlNode (_, "fastest") _ [] []) = Just 100000
drtn _ = Nothing

gtPth :: XmlNode -> FilePath
gtPth (XmlNode (_, "father") _ [] []) = "family"
gtPth _ = "others"
