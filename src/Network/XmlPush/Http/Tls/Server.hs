{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, PackageImports #-}

module Network.XmlPush.Http.Tls.Server (
	HttpTlsSv,
	HttpTlsSvArgs(..), Mechanism(..),
	HttpPullTlsSvArgs(..), HttpPullSvArgs(HttpPullSvArgs),
	HttpPushTlsArgs(..), HttpPushArgs(HttpPushArgs),
	tlsArgsCl, tlsArgsSv,
	) where

import Control.Applicative
import "monads-tf" Control.Monad.Error
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Maybe
import Data.HandleLike
import Data.Pipe
import Data.Pipe.List
import Text.XML.Pipe
import Network.XmlPush
import Network.XmlPush.HttpPull.Tls.Server.Body
import Network.XmlPush.HttpPush.Tls.Body
import Network.TigHTTP.Server
import Network.Sasl
import Network.PeyoTLS.Server
import "crypto-random" Crypto.Random

newtype HttpTlsSv h = HttpTlsSv (Either (HttpPullTlsSv h) (HttpPushTls h))

data Mechanism = Pull | Push deriving Show

data HttpTlsSvArgs h = HttpTlsSvArgs
	(XmlNode -> Mechanism) (HttpPullTlsSvArgs h) (HttpPushTlsArgs h)

instance XmlPusher HttpTlsSv where
	type NumOfHandle HttpTlsSv = Two
	type PusherArgs HttpTlsSv = HttpTlsSvArgs
	generate (Two ch (Just sh)) (HttpTlsSvArgs s pla psa) =
		makeHttpTlsSv ch sh s pla psa
	generate _ _ = error "bad"
	readFrom (HttpTlsSv e) = either readFrom readFrom e
	writeTo (HttpTlsSv e) = either writeTo writeTo e

makeHttpTlsSv :: (
	ValidateHandle h, MonadBaseControl IO (HandleMonad h),
	MonadError (HandleMonad h), SaslError (ErrorType (HandleMonad h))
	) => Maybe h -> h -> (XmlNode -> Mechanism) ->
	HttpPullTlsSvArgs h -> HttpPushTlsArgs h -> HandleMonad h (HttpTlsSv h)
makeHttpTlsSv ch sh s
	(HttpPullTlsSvArgs pla' (TlsArgs gn cc cs mca kcs))
	psa = do
	g <- liftBase (cprgCreate <$> createEntropyPool :: IO SystemRNG)
	(`run` g) $ do
		t <- open sh cs kcs mca
		rq <- getRequest t
		Just [rn] <- runPipe $ requestBody rq
			=$= xmlEvent
			=$= convert fromJust
			=$= xmlNode []
			=$= toList
		HttpTlsSv `liftM` case s rn of
			Pull -> do
				HttpPullTlsSv r w <- makeHttpPull [rn] t pla' gn cc
				return . Left $ HttpPullTlsSv r w
			Push -> do
				hlDebug t "critical" "PUSH\n"
				ps <- makeHttpPush [rn] ch t psa
				return $ Right ps
