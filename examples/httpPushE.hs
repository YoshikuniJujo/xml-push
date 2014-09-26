{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Text.XML.Pipe
import Network

import Network.XmlPush.HttpPush
import TestPusher

main :: IO ()
main = do
	ch <- connectTo "localhost" $ PortNumber 80
	testPusher (undefined :: HttpPush Handle) (Two (Just ch) Nothing)
		(HttpPushArgs (const Nothing) getServerHandle
			(Just ("localhost", 80, "/")) gtPth wntRspns)

getServerHandle :: Maybe (IO Handle)
getServerHandle = Just $ do
	putStrLn "getServerHandle begin"
	soc <- listenOn $ PortNumber 8080
	(h, _, _) <- accept soc
	putStrLn "getServerHandle: accepted"
	return h

wntRspns :: XmlNode -> Bool
wntRspns (XmlNode (_, "monologue") _ [] []) = False
wntRspns _ = True

gtPth :: XmlNode -> FilePath
gtPth (XmlNode (_, "father") _ [] []) = "family"
gtPth _ = "others"
