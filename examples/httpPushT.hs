{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent.STM
import System.IO
import Text.XML.Pipe
import Network

import qualified Data.ByteString.Char8 as BSC

import Network.XmlPush.HttpPush
import TestPusher

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 80
	forever  $ do
		(sh, _, _) <- accept soc
--		ch <- connectTo "localhost" $ PortNumber 8080
--		vch <- atomically . newTVar $ Just ch
		vsh <- atomically . newTVar $ Just sh
		vch <- atomically $ newTVar Nothing
		testPusher (undefined :: HttpPush Handle) (Two vch vsh)
			(HttpPushArgs getClientHandle Nothing
				"localhost" 8080 "/" gtPth wntRspns)

getClientHandle :: XmlNode -> Maybe (IO Handle)
getClientHandle (XmlNode (_, "client") [] [] [XmlCharData hn]) = Just $ do
--	print "hoge"
	connectTo (BSC.unpack hn) $ PortNumber 8080
getClientHandle _ = Nothing

wntRspns :: XmlNode -> Bool
wntRspns (XmlNode (_, "monologue") _ [] []) = False
wntRspns _ = True

gtPth :: XmlNode -> FilePath
gtPth (XmlNode (_, "father") _ [] []) = "family"
gtPth _ = "others"
