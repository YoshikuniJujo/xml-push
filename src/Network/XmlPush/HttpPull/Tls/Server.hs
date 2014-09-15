{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts,
	PackageImports #-}

module Network.XmlPush.HttpPull.Tls.Server (
	HttpPullTlsSv, HttpPullTlsSvArgs(..), HttpPullSvArgs(..), TlsArgs(..)
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
	generate = makeHttpPull
	readFrom (HttpPullTlsSv r _) = r
	writeTo (HttpPullTlsSv _ w) = w

makeHttpPull :: (ValidateHandle h, MonadBaseControl IO (HandleMonad h)) =>
	One h -> HttpPullTlsSvArgs h -> HandleMonad h (HttpPullTlsSv h)
makeHttpPull (One h) (HttpPullTlsSvArgs
	(HttpPullSvArgs ip ep ynr) (TlsArgs gn cs mca kcs)) = do
	g <- liftBase (cprgCreate <$> createEntropyPool :: IO SystemRNG)
	(inc, otc) <- (`run` g) $ do
		t <- open h cs kcs mca
		runXml t ip ep ynr $ checkNameP t gn
	return $ HttpPullTlsSv (fromTChan inc) (toTChan otc)

checkNameP :: HandleLike h => TlsHandle h g -> (XmlNode -> Maybe String) ->
	Pipe XmlNode XmlNode (TlsM h g) ()
checkNameP t gn = (await >>=) . maybe (return ()) $ \n -> do
	ok <- maybe (return True) (lift . checkName t) $ gn n
	unless ok $ error "checkName: bad client name"
	yield n
	checkNameP t gn
