{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import System.IO
import Text.XML.Pipe
import Network

import qualified Data.ByteString.Char8 as BSC

import Network.XmlPush.Http.Server
import TestPusher

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 80
	forever $ do
		(h, _, _) <- accept soc
		void . forkIO $ testPusher
			(undefined :: HttpSv Handle) (Two Nothing $ Just h)
			(HttpSvArgs sel httpPullSvArgs httpPushArgs)

sel :: XmlNode -> Mechanism
sel (XmlNode (_, "pull") _ _ _) = Pull
sel (XmlNode (_, "push") _ _ _) = Push
sel _ = error "bad"

httpPullSvArgs :: HttpPullSvArgs h
httpPullSvArgs = HttpPullSvArgs isPll endPoll needResponse

isPll :: XmlNode -> Bool
isPll (XmlNode (_, "poll") _ _ _) = True
isPll _ = False

endPoll :: XmlNode
endPoll = XmlNode (nullQ "nothing") [] [] []

needResponse :: XmlNode -> Bool
needResponse (XmlNode (_, "monologue") _ _ _) = False
needResponse _ = True

httpPushArgs :: HttpPushArgs Handle
httpPushArgs = HttpPushArgs getClientHandle Nothing Nothing gtPth wntRspns

getClientHandle :: XmlNode -> Maybe (IO Handle, String, Int, FilePath)
getClientHandle (XmlNode (_, "client") [] [] [XmlCharData hn]) = Just (
	connectTo (BSC.unpack hn) $ PortNumber 8080,
	"localhost",
	8080,
	"/" )
getClientHandle _ = Nothing

wntRspns :: XmlNode -> Bool
wntRspns (XmlNode (_, "monologue") _ [] []) = False
wntRspns _ = True

gtPth :: XmlNode -> FilePath
gtPth (XmlNode (_, "father") _ [] []) = "family"
gtPth _ = "others"
