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
--	soc <- listenOn $ PortNumber 8080
--	(sh, _, _) <- accept soc
	vsh <- atomically $ newTVar Nothing
	testPusher (undefined :: HttpPush Handle) (Two vch vsh)
		(HttpPushArgs (const Nothing) getServerHandle
			"localhost" 80 "/" gtPth wntRspns)

getServerHandle :: Maybe (IO Handle)
getServerHandle = Just $ do
	soc <- listenOn $ PortNumber 8080
	(h, _, _) <- accept soc
	return h

wntRspns :: XmlNode -> Bool
wntRspns (XmlNode (_, "monologue") _ [] []) = False
wntRspns _ = True

gtPth :: XmlNode -> FilePath
gtPth (XmlNode (_, "father") _ [] []) = "family"
gtPth _ = "others"
