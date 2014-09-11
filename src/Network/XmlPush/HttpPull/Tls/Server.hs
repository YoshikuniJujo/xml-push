{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts,
	PackageImports #-}

module Network.XmlPush.HttpPull.Tls.Server (
	HttpPullTlsSv, TlsArgs(..) ) where

import Prelude hiding (filter)

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.HandleLike
import Data.Pipe
import Data.Pipe.TChan
import Text.XML.Pipe
import Network.PeyoTLS.Server
import "crypto-random" Crypto.Random

import Network.XmlPush
import Network.XmlPush.HttpPull.Server.Common
import Network.XmlPush.Tls.Server

data HttpPullTlsSv h = HttpPullTlsSv
	(Pipe () XmlNode (HandleMonad h) ())
	(Pipe XmlNode () (HandleMonad h) ())

instance XmlPusher HttpPullTlsSv where
	type NumOfHandle HttpPullTlsSv = One
	type PusherArg HttpPullTlsSv = (XmlNode -> Bool, XmlNode, TlsArgs)
	generate = makeHttpPull
	readFrom (HttpPullTlsSv r _) = r
	writeTo (HttpPullTlsSv _ w) = w

makeHttpPull :: (ValidateHandle h, MonadBaseControl IO (HandleMonad h)) =>
	One h -> (XmlNode -> Bool, XmlNode, TlsArgs) ->
	HandleMonad h (HttpPullTlsSv h)
makeHttpPull (One h) (ip, ep, TlsArgs cs mca kcs) = do
	g <- liftBase (cprgCreate <$> createEntropyPool :: IO SystemRNG)
	(inc, otc) <- (`run` g) $ do
		t <- open h cs kcs mca
		runXml t ip ep
	return $ HttpPullTlsSv (fromTChan inc) (toTChan otc)
