{-# LANGUAGE TupleSections, TypeFamilies, FlexibleContexts #-}

module FilePusher (SimplePusher(..)) where

import Control.Monad
import Data.Maybe
import Data.HandleLike
import Control.Monad.Trans.Control
import Data.Pipe
import Data.Pipe.ByteString
import Text.XML.Pipe

import Network.XmlPush

data SimplePusher h = SimplePusher
	(Pipe () XmlNode (HandleMonad h) ())
	(Pipe XmlNode () (HandleMonad h) ())

instance XmlPusher SimplePusher where
	type PusherArgs SimplePusher = (FilePath, FilePath)
	type NumOfHandle SimplePusher = Zero
	generate = const $ uncurry simplePusher
	readFrom (SimplePusher r _) = r
	writeTo (SimplePusher _ w) = w

simplePusher :: MonadBaseControl IO (HandleMonad h) =>
	FilePath -> FilePath -> HandleMonad h (SimplePusher h)
simplePusher rf wf = return $ SimplePusher (readXml rf) (writeXml wf)

readXml :: MonadBaseControl IO m => FilePath -> Pipe () XmlNode m ()
readXml rf = fromFile rf
	=$= xmlEvent
	=$= convert fromJust
	=$= void (xmlNode [])

writeXml :: MonadBaseControl IO m => FilePath -> Pipe XmlNode () m ()
writeXml wf = convert (xmlString . (: []))  =$= toFile wf
