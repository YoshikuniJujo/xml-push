{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables,
	TypeFamilies, FlexibleContexts,
	PackageImports #-}

module Network.XmlPush.HttpPush.Tls.Body (
	HttpPushTls, HttpPushTlsArgs(..), HttpPushArgs(..),
	TlsArgsCl, tlsArgsCl, TlsArgsSv, tlsArgsSv,
	makeHttpPush, makeHttpPushTls,
	HttpPushTlsTest(..), HttpPushTlsTestArgs(..),
	) where

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
import Data.X509 hiding (getCertificate)
import Data.X509.CertificateStore
import Text.XML.Pipe
import Network.TigHTTP.Server
import Network.PeyoTLS.ReadFile
import Network.PeyoTLS.Server (getCertificate)
import Network.PeyoTLS.Client (ValidateHandle)
import "crypto-random" Crypto.Random

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Network.PeyoTLS.Client as Cl
import qualified Network.PeyoTLS.Server as Sv

import Network.XmlPush
import Network.XmlPush.HttpPush.Common
import Network.XmlPush.Tls.Client as TC
import Network.XmlPush.Tls.Server as TS

type TlsArgsCl = TC.TlsArgs

tlsArgsCl :: String -> Bool ->
	(XmlNode -> Maybe (SignedCertificate -> Bool)) ->
	[Cl.CipherSuite] -> CertificateStore ->
	[(CertSecretKey, CertificateChain)] -> TlsArgsCl
tlsArgsCl = TC.TlsArgs

type TlsArgsSv = TS.TlsArgs

tlsArgsSv :: (XmlNode -> Maybe String) ->
	(XmlNode -> Maybe (SignedCertificate -> Bool)) -> [Cl.CipherSuite] ->
	Maybe CertificateStore -> [(CertSecretKey, CertificateChain)] -> TlsArgsSv
tlsArgsSv = TS.TlsArgs

data HttpPushTls h = HttpPushTls {
	needReply :: TVar Bool,
	clientReadChan :: TChan (XmlNode, Bool),
	clientWriteChan :: TChan (Maybe XmlNode),
	serverReadChan :: TChan (XmlNode, Bool),
	serverWriteChan :: TChan (Maybe XmlNode) }

data HttpPushTlsArgs h = HttpPushTlsArgs (HttpPushArgs h) TC.TlsArgs TS.TlsArgs

instance XmlPusher HttpPushTls where
	type NumOfHandle HttpPushTls = Two
	type PusherArgs HttpPushTls = HttpPushTlsArgs
	generate (Two ch sh) = makeHttpPushTls [] ch sh
	readFrom hp = fromTChans [clientReadChan hp, serverReadChan hp] =$=
		setNeedReply (needReply hp)
	writeTo hp = (convert (((), ) . Just) =$=) . toTChansM $ do
		nr <- liftBase . atomically . readTVar $ needReply hp
		liftBase . atomically $ writeTVar (needReply hp) False
		return [
			(const nr, serverWriteChan hp),
			(const True, clientWriteChan hp) ]

data HttpPushTlsTest h = HttpPushTlsTest (HttpPushTls h)
data HttpPushTlsTestArgs h = HttpPushTlsTestArgs (HttpPushTlsArgs h) [XmlNode]

instance XmlPusher HttpPushTlsTest where
	type NumOfHandle HttpPushTlsTest = Two
	type PusherArgs HttpPushTlsTest = HttpPushTlsTestArgs
	generate (Two ch sh) (HttpPushTlsTestArgs a p) =
		HttpPushTlsTest <$> makeHttpPushTls p ch sh a
	readFrom (HttpPushTlsTest hp) = readFrom hp
	writeTo (HttpPushTlsTest hp) = writeTo hp

makeHttpPushTls :: (ValidateHandle h, MonadBaseControl IO (HandleMonad h)) =>
	[XmlNode] ->
	Maybe h -> Maybe h ->
	HttpPushTlsArgs h -> HandleMonad h (HttpPushTls h)
makeHttpPushTls pre mch msh (HttpPushTlsArgs (HttpPushArgs gc gs hi gp wr)
	(TC.TlsArgs dn cdn cc' cs ca kcs) (TS.TlsArgs gn cc cs' mca' kcs')) = do
	vch <- liftBase . atomically $ newTVar mch
	vsh <- liftBase . atomically $ newTVar msh
	case hi of
		Just (hn, _, _) -> when (dn /= hn) $
			error "makeHttpPushTls: conflicted domain name"
		_ -> return ()
	v <- liftBase . atomically $ newTVar False
	vhi <- liftBase . atomically $ newTVar hi
	(ci, co) <- clientC vch vhi cdn cc' gp cs ca kcs
	(si, so) <- talk pre wr vsh gn cc cs' mca' kcs' vch vhi gc gs
	return $ HttpPushTls v ci co si so

makeHttpPush :: (ValidateHandle h, MonadBaseControl IO (HandleMonad h), CPRG g) =>
	[XmlNode] -> Maybe h -> Sv.TlsHandle h g ->
	HttpPushTlsArgs h -> Sv.TlsM h g (HttpPushTls h)
makeHttpPush pre mch t (HttpPushTlsArgs (HttpPushArgs gc gs hi gp wr)
	(TC.TlsArgs dn cdn cc' cs ca kcs) (TS.TlsArgs gn cc cs' mca' kcs')) = do
	vch <- lift . lift . liftBase . atomically $ newTVar mch
	vsh <- lift . lift . liftBase . atomically $ newTVar undefined
	case hi of
		Just (hn, _, _) -> when (dn /= hn) $
			error "makeHttpPushTls: conflicted domain name"
		_ -> return ()
	v <- lift . lift . liftBase . atomically $ newTVar False
	vhi <- lift . lift . liftBase . atomically $ newTVar hi
	(ci, co) <- lift . lift $ clientC vch vhi cdn cc' gp cs ca kcs
	(si, so) <- do
		inc <- lift . lift . liftBase $ atomically newTChan
		otc <- lift . lift . liftBase $ atomically newTChan
		void . liftBaseDiscard forkIO $
			talkT t inc otc pre wr gn cc vch vhi gc
		return (inc, otc)
	return $ HttpPushTls v ci co si so

clientC :: (ValidateHandle h, MonadBaseControl IO (HandleMonad h)) =>
	TVar (Maybe h) -> TVar (Maybe (String, Int, FilePath)) -> Bool ->
	(XmlNode -> Maybe (SignedCertificate -> Bool)) ->
	(XmlNode -> FilePath) ->
	[Cl.CipherSuite] -> CertificateStore ->
	[(CertSecretKey, CertificateChain)] ->
	HandleMonad h (TChan (XmlNode, Bool), TChan (Maybe XmlNode))
clientC vh vhi cdn cc gp cs ca kcs = do
	inc <- liftBase $ atomically newTChan
	otc <- liftBase $ atomically newTChan
	(g :: SystemRNG) <- liftBase $ cprgCreate <$> createEntropyPool
	void . liftBaseDiscard forkIO $ do
		h <- liftBase . atomically $ do
			mh <- readTVar vh
			case mh of
				Just h -> return h
				_ -> retry
		(hn, pn, pt) <- liftBase . atomically $ do
			mhi <- readTVar vhi
			case mhi of
				Just hi -> return hi
				_ -> retry
		(`Cl.run` g) $ do
			t <- (if cdn then Cl.open' h hn else Cl.open h) cs kcs ca
			runPipe_ $ fromTChan otc
				=$= filter isJust
				=$= convert fromJust
				=$= clientLoop t hn pn pt gp (checkCertCl t cc)
				=$= convert (, False)
				=$= toTChan inc
	return (inc, otc)

talk :: (ValidateHandle h, MonadBaseControl IO (HandleMonad h)) =>
	[XmlNode] ->
	(XmlNode -> Bool) -> (TVar (Maybe h)) -> (XmlNode -> Maybe String) ->
	(XmlNode -> Maybe (SignedCertificate -> Bool)) -> [Sv.CipherSuite] ->
	Maybe CertificateStore -> [(CertSecretKey, CertificateChain)] ->
	TVar (Maybe h) -> TVar (Maybe (String, Int, FilePath)) ->
	(XmlNode -> Maybe (HandleMonad h h, String, Int, FilePath)) ->
	Maybe (HandleMonad h h) ->
	HandleMonad h (TChan (XmlNode, Bool), TChan (Maybe XmlNode))
talk pre wr vh gn cc cs mca kcs vch vhi gc mgs = do
	g <- liftBase (cprgCreate <$> createEntropyPool :: IO SystemRNG)
	inc <- liftBase $ atomically newTChan
	otc <- liftBase $ atomically newTChan
	void . liftBaseDiscard forkIO $ do
		flip (maybe (return ())) mgs $ \gs -> do
			h <- gs
			liftBase . atomically $ writeTVar vh (Just h)
		h <- liftBase . atomically $ do
			mh <- readTVar vh
			case mh of
				Just h -> return h
				_ -> retry
		(`Sv.run` g) $ do
			t <- Sv.open h cs kcs mca
			talkT t inc otc pre wr gn cc vch vhi gc
	return (inc, otc)

hlDebugP :: HandleLike h => h -> (a -> BS.ByteString) -> Pipe a a (HandleMonad h) ()
hlDebugP h shw = (await >>=) . maybe (return ()) $ \x -> do
	lift . hlDebug h "medium" $ shw x
	yield x
	hlDebugP h shw

talkT :: (ValidateHandle h, MonadBase IO (HandleMonad h), CPRG g) =>
	Sv.TlsHandle h g -> TChan (XmlNode, Bool) -> TChan (Maybe XmlNode) ->
	[XmlNode] -> (XmlNode -> Bool) -> (XmlNode -> Maybe String) ->
	(XmlNode -> Maybe (SignedCertificate -> Bool)) -> TVar (Maybe h) ->
	TVar (Maybe (String, Int, FilePath)) ->
	(XmlNode -> Maybe (HandleMonad h h, String, Int, FilePath)) ->
	Sv.TlsM h g ()
talkT t inc otc pre wr gn cc vch vhi gc = do
			runPipe_ . writeToChan t inc otc pre $
				setClient vch vhi gc =$= checkReply wr otc
			runPipe_ . forever $ do
				req <- lift $ getRequest t
				requestBody req
					=$= xmlEvent
					=$= convert fromJust
					=$= xmlNode []
					=$= setClient vch vhi gc
					=$= checkCert t cc
					=$= checkName t gn
					=$= hlDebugP t ((`BS.append` "\n")
						. ("xml-push: talkT: " `BS.append`)
						. xmlString . (: []))
					=$= checkReply wr otc
					=$= toTChan inc
				fromTChan otc =$= await >>= maybe (return ()) (\mn ->
					lift . putResponse t . responseP $ case mn of
						Just n -> LBS.fromChunks [xmlString [n]]
						_ -> "")

writeToChan :: (HandleLike h, MonadBase IO (HandleMonad h)) =>
	h -> TChan a -> TChan (Maybe XmlNode) -> [XmlNode] ->
	Pipe XmlNode a (HandleMonad h) () ->
	Pipe () () (HandleMonad h) ()
writeToChan _ _ _ [] _ = return ()
writeToChan h inc otc pre pp = do
	mapM yield pre =$= pp =$= toTChan inc
	fromTChan otc =$= await >>= maybe (return ()) (\mn ->
		lift . putResponse h . responseP $ case mn of
			Just n -> LBS.fromChunks [xmlString [n]]
			_ -> "")

checkCert :: HandleLike h => Sv.TlsHandle h g ->
	(XmlNode -> Maybe (SignedCertificate -> Bool)) ->
	Pipe XmlNode XmlNode (Sv.TlsM h g) ()
checkCert t cc = (await >>=) . maybe (return ()) $ \n -> do
	let ck = maybe (const True) id $ cc n
	c <- lift $ getCertificate t
	unless (ck c) $ error "checkCert: bad certificate"
	yield n
	checkCert t cc

checkCertCl :: (ValidateHandle h, CPRG g) => Cl.TlsHandle h g ->
	(XmlNode -> Maybe (SignedCertificate -> Bool)) ->
	Pipe XmlNode XmlNode (Cl.TlsM h g) ()
checkCertCl t cc = (await >>=) . maybe (return ()) $ \n -> do
	lift $ hlDebug t "medium" "begin checkCertCl"
	let ck = maybe (const True) id $ cc n
	c <- lift $ Cl.getCertificate t
	unless (ck c) $ error "checkCert: bad certificate"
	yield n
	checkCertCl t cc

checkName :: HandleLike h => Sv.TlsHandle h g -> (XmlNode -> Maybe String) ->
	Pipe XmlNode XmlNode (Sv.TlsM h g) ()
checkName t gn = (await >>=) . maybe (return ()) $ \n -> do
	ok <- maybe (return True) (lift . svCheckName t) $ gn n
	unless ok $ error "checkName: bad client name"
	yield n
	checkName t gn

svCheckName :: HandleLike h => Sv.TlsHandle h g -> String -> Sv.TlsM h g Bool
svCheckName t n = do
	ns <- Sv.getNames t
	return $ n `elem` ns

setClient :: (MonadBase IO (HandleMonad h)) =>
	TVar (Maybe h) -> TVar (Maybe (String, Int, FilePath)) ->
	(XmlNode -> Maybe (HandleMonad h h, String, Int, FilePath)) ->
	Pipe XmlNode XmlNode (Sv.TlsM h g) ()
setClient vch vhi gc = (await >>=) . maybe (return ()) $ \n -> do
	yield n
	case gc n of
		Just (gh, hn, pn, pt) -> do
			h <- lift . lift $ lift gh
			lift . liftBase . atomically . writeTVar vch $ Just h
			lift . liftBase . atomically . writeTVar vhi
				$ Just (hn, pn, pt)
		_ -> return ()
	setClient vch vhi gc
