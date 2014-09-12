{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Network.XmlPush.HttpPull.Server (HttpPullSv, HttpPullSvArgs(..)) where

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
	type PusherArgs HttpPullSv = HttpPullSvArgs
	generate = makeHttpPull
	readFrom (HttpPullSv r _) = r
	writeTo (HttpPullSv _ w) = w

makeHttpPull :: (HandleLike h, MonadBaseControl IO (HandleMonad h)) =>
	One h -> HttpPullSvArgs -> HandleMonad h (HttpPullSv h)
makeHttpPull (One h) (HttpPullSvArgs ip ep) = do
	(inc, otc) <- runXml h ip ep (convert id)
	return $ HttpPullSv (fromTChan inc) (toTChan otc)
