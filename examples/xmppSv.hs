{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import System.IO
import Text.XML.Pipe
import Network

import Network.XmlPush
import Network.XmlPush.Xmpp.Server
import TestPusher

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 5222
	forever $ do
		(h, _, _) <- accept soc
		void . forkIO $ testPusher (undefined :: XmppServer Handle) (One h)
			(XmppServerArgs "localhost" iNdRspns yNdRspns)

iNdRspns :: XmlNode -> Bool
iNdRspns (XmlNode (_, "i_don_t_need_response") _ _ _) = False
iNdRspns _ = True

yNdRspns :: XmlNode -> Bool
yNdRspns (XmlNode (_, "no_response") _ _ _) = False
yNdRspns _ = True
