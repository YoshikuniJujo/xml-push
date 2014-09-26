{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts #-}

module Network.XmlPush.HttpPull.Server.Body (
	HttpPullSv(..), HttpPullSvArgs(..), makeHttpPull,
	HttpPullSvTest(..), HttpPullSvTestArgs(..),
	) where

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
	generate = makeHttpPull []
	readFrom (HttpPullSv r _) = r
	writeTo (HttpPullSv _ w) = w

makeHttpPull :: (HandleLike h, MonadBaseControl IO (HandleMonad h)) =>
	[XmlNode] -> One h -> HttpPullSvArgs h -> HandleMonad h (HttpPullSv h)
makeHttpPull pre (One h) (HttpPullSvArgs ip ep ynr) = do
	(inc, otc) <- runXml pre h ip ep ynr (convert id)
	return $ HttpPullSv (fromTChan inc) (toTChan otc)

data HttpPullSvTest h = HttpPullSvTest
	(Pipe () XmlNode (HandleMonad h) ())
	(Pipe XmlNode () (HandleMonad h) ())

data HttpPullSvTestArgs h = HttpPullSvTestArgs (HttpPullSvArgs h) [XmlNode]

instance XmlPusher HttpPullSvTest where
	type NumOfHandle HttpPullSvTest = One
	type PusherArgs HttpPullSvTest = HttpPullSvTestArgs
	generate h (HttpPullSvTestArgs a pre) = do
		HttpPullSv r w <- makeHttpPull pre h a
		return $ HttpPullSvTest r w
	readFrom (HttpPullSvTest r _) = r
	writeTo (HttpPullSvTest _ w) = w
