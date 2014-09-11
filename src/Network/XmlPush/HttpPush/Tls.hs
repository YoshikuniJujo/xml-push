{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables,
	TypeFamilies, FlexibleContexts,
	PackageImports #-}

module Network.XmlPush.HttpPush.Tls (
	HttpPushTls, HttpPushArgs(..), tlsArgsCl, tlsArgsSv) where

import Prelude hiding (filter)

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Data.Maybe
import Data.HandleLike
import Data.Pipe
import Data.Pipe.Flow
import Data.Pipe.TChan
import Data.X509
import Data.X509.CertificateStore
import Text.XML.Pipe
import Network.TigHTTP.Server
import Network.PeyoTLS.ReadFile
import Network.PeyoTLS.Client (ValidateHandle)
import "crypto-random" Crypto.Random

import qualified Data.ByteString.Lazy as LBS
import qualified Network.PeyoTLS.Client as Cl
import qualified Network.PeyoTLS.Server as Sv

import Network.XmlPush
import Network.XmlPush.HttpPush.Common
import Network.XmlPush.Tls.Client as TC
import Network.XmlPush.Tls.Server as TS

tlsArgsCl :: CertificateStore -> [(CertSecretKey, CertificateChain)] -> TC.TlsArgs
tlsArgsCl = TC.TlsArgs

tlsArgsSv :: Maybe CertificateStore -> [(CertSecretKey, CertificateChain)] ->
	TS.TlsArgs
tlsArgsSv = TS.TlsArgs

data HttpPushTls h = HttpPushTls {
	needReply :: TVar Bool,
	clientReadChan :: TChan (XmlNode, Bool),
	clientWriteChan :: TChan (Maybe XmlNode),
	serverReadChan :: TChan (XmlNode, Bool),
	serverWriteChan :: TChan (Maybe XmlNode) }

instance XmlPusher HttpPushTls where
	type NumOfHandle HttpPushTls = Two
	type PusherArg HttpPushTls = (HttpPushArgs, TC.TlsArgs, TS.TlsArgs)
	generate (Two ch sh) = makeHttpPushTls ch sh
	readFrom hp = fromTChans [clientReadChan hp, serverReadChan hp] =$=
		setNeedReply (needReply hp)
	writeTo hp = (convert (((), ) . Just) =$=) . toTChansM $ do
		nr <- liftBase . atomically . readTVar $ needReply hp
		liftBase . atomically $ writeTVar (needReply hp) False
		return [
			(const nr, serverWriteChan hp),
			(const True, clientWriteChan hp) ]

makeHttpPushTls :: (ValidateHandle h, MonadBaseControl IO (HandleMonad h)) =>
	h -> h -> (HttpPushArgs, TC.TlsArgs, TS.TlsArgs) ->
	HandleMonad h (HttpPushTls h)
makeHttpPushTls ch sh
	(HttpPushArgs hn pn pt gp wr, TC.TlsArgs ca kcs, TS.TlsArgs mca' kcs') = do
	v <- liftBase . atomically $ newTVar False
	(ci, co) <- clientC ch hn pn pt gp ca kcs
	(si, so) <- talk wr sh mca' kcs'
	return $ HttpPushTls v ci co si so

clientC :: (ValidateHandle h, MonadBaseControl IO (HandleMonad h)) =>
	h -> String -> Int -> FilePath -> (XmlNode -> FilePath) ->
	CertificateStore -> [(CertSecretKey, CertificateChain)] ->
	HandleMonad h (TChan (XmlNode, Bool), TChan (Maybe XmlNode))
clientC h hn pn pt gp ca kcs = do
	inc <- liftBase $ atomically newTChan
	otc <- liftBase $ atomically newTChan
--	ca <- liftBase $ readCertificateStore ["certs/cacert.sample_pem"]
	(g :: SystemRNG) <- liftBase $ cprgCreate <$> createEntropyPool
	void . liftBaseDiscard forkIO . (`Cl.run` g) $ do
		t <- Cl.open' h "localhost" ["TLS_RSA_WITH_AES_128_CBC_SHA"] kcs ca
		runPipe_ $ fromTChan otc
			=$= filter isJust
			=$= convert fromJust
			=$= clientLoop t hn pn pt gp
			=$= convert (, False)
			=$= toTChan inc
	return (inc, otc)

talk :: (ValidateHandle h, MonadBaseControl IO (HandleMonad h)) =>
	(XmlNode -> Bool) -> h ->
	Maybe CertificateStore -> [(CertSecretKey, CertificateChain)] ->
	HandleMonad h (TChan (XmlNode, Bool), TChan (Maybe XmlNode))
talk wr h mca kcs = do
	inc <- liftBase $ atomically newTChan
	otc <- liftBase $ atomically newTChan
	g <- liftBase (cprgCreate <$> createEntropyPool :: IO SystemRNG)
	void . liftBaseDiscard forkIO . (`Sv.run` g) $ do
		t <- Sv.open h ["TLS_RSA_WITH_AES_128_CBC_SHA"] kcs mca
		runPipe_ . forever $ do
			req <- lift $ getRequest t
			requestBody req
				=$= xmlEvent
				=$= convert fromJust
				=$= xmlNode []
				=$= checkReply wr otc
				=$= toTChan inc
			fromTChan otc =$= await >>= maybe (return ()) (\mn ->
				lift . putResponse t . responseP $ case mn of
					Just n -> LBS.fromChunks [xmlString [n]]
					_ -> "")
	return (inc, otc)
