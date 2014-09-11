{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts,
	PackageImports #-}

module Network.XmlPush.HttpPull.Tls.Server (HttpPullTlsSv) where

import Prelude hiding (filter)

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.HandleLike
import Data.Pipe
import Data.Pipe.TChan
import Text.XML.Pipe
import Network.PeyoTLS.Server
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import Network.XmlPush
import Network.XmlPush.HttpPull.Server.Common

data HttpPullTlsSv h = HttpPullTlsSv
	(Pipe () XmlNode (HandleMonad h) ())
	(Pipe XmlNode () (HandleMonad h) ())

instance XmlPusher HttpPullTlsSv where
	type NumOfHandle HttpPullTlsSv = One
	type PusherArg HttpPullTlsSv = (XmlNode -> Bool, XmlNode)
	generate = makeHttpPull
	readFrom (HttpPullTlsSv r _) = r
	writeTo (HttpPullTlsSv _ w) = w

makeHttpPull :: (ValidateHandle h, MonadBaseControl IO (HandleMonad h)) =>
	One h -> (XmlNode -> Bool, XmlNode) -> HandleMonad h (HttpPullTlsSv h)
makeHttpPull (One h) (ip, ep) = do
	k <- liftBase $ readKey "certs/localhost.sample_key"
	c <- liftBase $ readCertificateChain ["certs/localhost.sample_crt"]
	g <- liftBase (cprgCreate <$> createEntropyPool :: IO SystemRNG)
	(inc, otc) <- (`run` g) $ do
		t <- open h ["TLS_RSA_WITH_AES_128_CBC_SHA"] [(k, c)] Nothing
		runXml t ip ep
	return $ HttpPullTlsSv (fromTChan inc) (toTChan otc)
