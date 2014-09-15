{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import System.IO
import Text.XML.Pipe
import Network

import Network.XmlPush.HttpPull.Server
import TestPusher

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 80
	forever $ do
		(h, _, _) <- accept soc
		void . forkIO $ testPusher
			(undefined :: HttpPullSv Handle) (One h)
			(HttpPullSvArgs isPll endPoll needResponse)

isPll :: XmlNode -> Bool
isPll (XmlNode (_, "poll") _ _ _) = True
isPll _ = False

endPoll :: XmlNode
endPoll = XmlNode (nullQ "nothing") [] [] []

needResponse :: XmlNode -> Bool
needResponse (XmlNode (_, "monologue") _ _ _) = False
needResponse _ = True
