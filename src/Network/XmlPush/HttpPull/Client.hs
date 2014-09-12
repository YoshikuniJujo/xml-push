{-# LANGUAGE TypeFamilies, FlexibleContexts,
	PackageImports #-}

module Network.XmlPush.HttpPull.Client (HttpPullCl, HttpPullClArgs(..)) where

import Prelude hiding (filter)

import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Concurrent.STM
import Data.HandleLike
import Data.Pipe
import Data.Pipe.TChan
import Text.XML.Pipe

import Network.XmlPush
import Network.XmlPush.HttpPull.Client.Common

data HttpPullCl h = HttpPullCl
	(Pipe () XmlNode (HandleMonad h) ())
	(Pipe XmlNode () (HandleMonad h) ())

instance XmlPusher HttpPullCl where
	type NumOfHandle HttpPullCl = One
	type PusherArgs HttpPullCl = HttpPullClArgs
	generate = makeHttpPull
	readFrom (HttpPullCl r _) = r
	writeTo (HttpPullCl _ w) = w

makeHttpPull :: (HandleLike h, MonadBaseControl IO (HandleMonad h)) =>
	One h -> HttpPullClArgs -> HandleMonad h (HttpPullCl h)
makeHttpPull (One h) (HttpPullClArgs hn pn fp pl ip gdr gp) = do
	dr <- liftBase . atomically $ newTVar Nothing
	(inc, otc) <- talkC h hn pn fp gp pl ip dr gdr
	return $ HttpPullCl (fromTChan inc) (toTChan otc)
