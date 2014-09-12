{-# LANGUAGE ScopedTypeVariables #-}

module TestSimple (testSimple) where

import Control.Monad
import Control.Concurrent
import Data.Maybe
import Data.Pipe
import Data.Pipe.ByteString
import System.IO
import Text.XML.Pipe
import Network.XmlPush
import Network.XmlPush.Simple

testSimple :: Handle -> IO ()
testSimple h = do
	(sp :: SimplePusher Handle) <- generate (One h) ()
	void . forkIO . runPipe_ $ readFrom sp
		=$= convert (xmlString . (: []))
		=$= toHandle stdout
	runPipe_ $ fromHandle stdin
		=$= xmlEvent
		=$= convert fromJust
		=$= xmlNode []
		=$= writeTo sp
