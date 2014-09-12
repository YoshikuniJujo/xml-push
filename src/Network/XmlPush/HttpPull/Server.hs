{-# LANGUAGE TypeFamilies, FlexibleContexts,
	PackageImports #-}

module Network.XmlPush.HttpPull.Server (HttpPullSv) where

import Prelude hiding (filter)

import Control.Monad.Trans.Control
import Data.HandleLike
import Data.Pipe
import Data.Pipe.TChan
import Text.XML.Pipe

import Network.XmlPush
import Network.XmlPush.HttpPull.Server.Common

data HttpPullSv h = HttpPullSv
	(Pipe () XmlNode (HandleMonad h) ())
	(Pipe XmlNode () (HandleMonad h) ())

instance XmlPusher HttpPullSv where
	type NumOfHandle HttpPullSv = One
	type PusherArg HttpPullSv = (XmlNode -> Bool, XmlNode)
	generate = makeHttpPull
	readFrom (HttpPullSv r _) = r
	writeTo (HttpPullSv _ w) = w

makeHttpPull :: (HandleLike h, MonadBaseControl IO (HandleMonad h)) =>
	One h -> (XmlNode -> Bool, XmlNode) -> HandleMonad h (HttpPullSv h)
makeHttpPull (One h) (ip, ep) = do
	(inc, otc) <- runXml h ip ep (convert id)
	return $ HttpPullSv (fromTChan inc) (toTChan otc)
