{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.STM
import System.IO
import Text.XML.Pipe
import Network

import Network.XmlPush.HttpPush
import TestPusher

main :: IO ()
main = do
	ch <- connectTo "localhost" $ PortNumber 80
	vch <- atomically . newTVar $ Just ch
	soc <- listenOn $ PortNumber 8080
	(sh, _, _) <- accept soc
	testPusher (undefined :: HttpPush Handle) (Two vch sh)
		(HttpPushArgs "localhost" 80 "/" gtPth wntRspns)

wntRspns :: XmlNode -> Bool
wntRspns (XmlNode (_, "monologue") _ [] []) = False
wntRspns _ = True

gtPth :: XmlNode -> FilePath
gtPth (XmlNode (_, "father") _ [] []) = "family"
gtPth _ = "others"
