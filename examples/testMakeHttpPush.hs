{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import System.IO
import Text.XML.Pipe
import Network

import qualified Data.ByteString.Char8 as BSC

import Network.XmlPush.HttpPush.Body
import TestPusher

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 80
	forever  $ do
		(sh, _, _) <- accept soc
		testPusher (undefined :: HttpPushTest Handle)
			(Two Nothing $ Just sh)
			(HttpPushTestArgs
				(HttpPushArgs getClientHandle Nothing
					Nothing gtPth wntRspns)
				pre)

pre :: [XmlNode]
pre = [	XmlNode (nullQ "pre") [] [] [] ]

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
