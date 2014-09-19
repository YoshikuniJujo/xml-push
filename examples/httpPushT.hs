{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent.STM
import System.IO
import Text.XML.Pipe
import Network

import Network.XmlPush.HttpPush
import TestPusher

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 80
	forever  $ do
		(sh, _, _) <- accept soc
		ch <- connectTo "localhost" $ PortNumber 8080
		vch <- atomically . newTVar $ Just ch
		testPusher (undefined :: HttpPush Handle) (Two vch sh)
			(HttpPushArgs "localhost" 8080 "/" gtPth wntRspns)

wntRspns :: XmlNode -> Bool
wntRspns (XmlNode (_, "monologue") _ [] []) = False
wntRspns _ = True

gtPth :: XmlNode -> FilePath
gtPth (XmlNode (_, "father") _ [] []) = "family"
gtPth _ = "others"
