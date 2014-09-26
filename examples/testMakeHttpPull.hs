{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import System.IO
import Text.XML.Pipe
import Network

import Network.XmlPush.HttpPull.Server.Body
import TestPusher

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 80
	forever $ do
		(h, _, _) <- accept soc
		void . forkIO $ testPusher
			(undefined :: HttpPullSvTest Handle) (One h)
			(HttpPullSvTestArgs
				(HttpPullSvArgs isPll endPoll needResponse)
				pre)

pre :: [XmlNode]
pre = [	XmlNode (nullQ "pre") [] [] []
	]

isPll :: XmlNode -> Bool
isPll (XmlNode (_, "poll") _ _ _) = True
isPll _ = False

endPoll :: XmlNode
endPoll = XmlNode (nullQ "nothing") [] [] []

needResponse :: XmlNode -> Bool
needResponse (XmlNode (_, "monologue") _ _ _) = False
needResponse _ = True
