{-# LANGUAGE TypeFamilies, PackageImports #-}

module Network.XmlPush.Simple (SimplePusher) where

import "monads-tf" Control.Monad.Trans
import Control.Monad
import Data.Maybe
import Data.HandleLike
import Data.Pipe
import Text.XML.Pipe
import Network.XmlPush

import qualified Data.ByteString as BS

data SimplePusher h = SimplePusher
	(Pipe () XmlNode (HandleMonad h) ())
	(Pipe XmlNode () (HandleMonad h) ())

data SimplePusherArgs h = SimplePusherArgsNull

instance XmlPusher SimplePusher where
	type NumOfHandle SimplePusher = One
	type PusherArgs SimplePusher = SimplePusherArgs
	generate (One h) _ = simplePusher h
	readFrom (SimplePusher r _) = r
	writeTo (SimplePusher _ w) = w

simplePusher :: HandleLike h => h -> HandleMonad h (SimplePusher h)
simplePusher h = return $ SimplePusher (readXml h) (writeXml h)

readXml :: HandleLike h => h -> Pipe () XmlNode (HandleMonad h) ()
readXml h = fromHandleLike h
	=$= xmlEvent
	=$= convert fromJust
	=$= void (xmlNode [])

writeXml :: HandleLike h => h -> Pipe XmlNode () (HandleMonad h) ()
writeXml h = convert (xmlString . (: [])) =$= toHandleLike h

fromHandleLike :: HandleLike h => h -> Pipe () BS.ByteString (HandleMonad h) ()
fromHandleLike h = lift (hlGetContent h) >>= yield >> fromHandleLike h

toHandleLike :: HandleLike h => h -> Pipe BS.ByteString () (HandleMonad h) ()
toHandleLike h = await >>= maybe (return ()) ((>> toHandleLike h) . lift . hlPut h)
