{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Text.XML.Pipe
import Network

import Network.XmlPush.HttpPull.Client
import TestPusher

main :: IO ()
main = do
	h <- connectTo "localhost" $ PortNumber 80
	testPusher (undefined :: HttpPullCl Handle) (One h) (HttpPullClArgs
		"localhost" 80 "/" gtPth mkPoll pendingQ (Just 10000000) drtn)

mkPoll :: IO XmlNode
mkPoll = do
	return $ XmlNode (nullQ "poll") [] [] []

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
gtPth (XmlNode (_, "mother") _ [] []) = "family"
gtPth (XmlNode (_, "children") _ [] []) = "family"
gtPth _ = "others"
