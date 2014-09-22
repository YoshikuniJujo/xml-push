import Control.Monad
import Control.Concurrent
import System.IO
import Network

import Network.XmlPush
import Network.XmlPush.Xmpp.Server
import TestPusher

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 5222
	forever $ do
		(h, _, _) <- accept soc
		void . forkIO $ testPusher
			(undefined :: XmppServer Handle) (One h) Null
