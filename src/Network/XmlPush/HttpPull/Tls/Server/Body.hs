{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts,
	PackageImports #-}

module Network.XmlPush.HttpPull.Tls.Server.Body (
	HttpPullTlsSv, HttpPullTlsSvArgs(..), HttpPullSvArgs(..), TlsArgs(..),
	HttpPullTlsSvTest, HttpPullTlsSvTestArgs(..),
	makeHttpPull,
	) where

import Prelude hiding (filter)

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.HandleLike
import Data.Pipe
import Data.Pipe.TChan
import Data.X509 hiding (getCertificate)
import Text.XML.Pipe
import Network.PeyoTLS.Server
import "crypto-random" Crypto.Random

import Network.XmlPush
import Network.XmlPush.HttpPull.Server.Common
import Network.XmlPush.Tls.Server

data HttpPullTlsSv h = HttpPullTlsSv
	(Pipe () XmlNode (HandleMonad h) ())
	(Pipe XmlNode () (HandleMonad h) ())

data HttpPullTlsSvArgs h = HttpPullTlsSvArgs (HttpPullSvArgs h) TlsArgs

instance XmlPusher HttpPullTlsSv where
	type NumOfHandle HttpPullTlsSv = One
	type PusherArgs HttpPullTlsSv = HttpPullTlsSvArgs
	generate = makeHttpPull []
	readFrom (HttpPullTlsSv r _) = r
	writeTo (HttpPullTlsSv _ w) = w

data HttpPullTlsSvTest h = HttpPullTlsSvTest
	(Pipe () XmlNode (HandleMonad h) ())
	(Pipe XmlNode () (HandleMonad h) ())

data HttpPullTlsSvTestArgs h = HttpPullTlsSvTestArgs (HttpPullTlsSvArgs h) [XmlNode]

instance XmlPusher HttpPullTlsSvTest where
	type NumOfHandle HttpPullTlsSvTest = One
	type PusherArgs HttpPullTlsSvTest = HttpPullTlsSvTestArgs
	generate h (HttpPullTlsSvTestArgs a pre) = do
		HttpPullTlsSv r w <- makeHttpPull pre h a
		return $ HttpPullTlsSvTest r w
	readFrom (HttpPullTlsSvTest r _) = r
	writeTo (HttpPullTlsSvTest _ w) = w

makeHttpPull :: (ValidateHandle h, MonadBaseControl IO (HandleMonad h)) =>
	[XmlNode] ->
	One h -> HttpPullTlsSvArgs h -> HandleMonad h (HttpPullTlsSv h)
makeHttpPull pre (One h) (HttpPullTlsSvArgs
	(HttpPullSvArgs ip ep ynr) (TlsArgs gn cc cs mca kcs)) = do
	g <- liftBase (cprgCreate <$> createEntropyPool :: IO SystemRNG)
	(inc, otc) <- (`run` g) $ do
		t <- open h cs kcs mca
		runXml pre t ip ep ynr $ checkNameP t gn cc
	return $ HttpPullTlsSv (fromTChan inc) (toTChan otc)

checkNameP :: HandleLike h => TlsHandle h g -> (XmlNode -> Maybe String) ->
	(XmlNode -> Maybe (SignedCertificate -> Bool)) ->
	Pipe XmlNode XmlNode (TlsM h g) ()
checkNameP t gn cc = (await >>=) . maybe (return ()) $ \n -> do
	ok <- maybe (return True) (lift . checkName t) $ gn n
	unless ok $ error "checkNameP: bad client name"
	let ck = maybe (const True) id $ cc n
	c <- lift $ getCertificate t
	unless (ck c) $ error "checkNameP: bad certificate"
	yield n
	checkNameP t gn cc
