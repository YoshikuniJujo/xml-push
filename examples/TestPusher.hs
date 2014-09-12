{-# LANGUAGE TupleSections, TypeFamilies, FlexibleContexts, PackageImports #-}

module TestPusher (XmlPusher(..), Zero(..), One(..), Two(..), testPusher) where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.Maybe
import Data.Pipe
import Data.Pipe.ByteString
import System.IO
import Text.XML.Pipe

import Network.XmlPush

testPusher :: XmlPusher xp =>
	xp Handle -> NumOfHandle xp Handle -> PusherArgs xp -> IO ()
testPusher tp hs as = do
	xp <- (`asTypeOf` tp) <$> generate hs as
	void . forkIO . runPipe_ $ readFrom xp
		=$= convert (xmlString . (: []))
		=$= toHandle stdout
	runPipe_ $ fromHandle stdin
		=$= xmlEvent
		=$= convert fromJust
		=$= xmlNode []
		=$= writeTo xp
