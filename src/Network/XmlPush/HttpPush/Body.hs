{-# LANGUAGE OverloadedStrings, TupleSections, TypeFamilies, FlexibleContexts,
	PackageImports #-}

module Network.XmlPush.HttpPush.Body (
	HttpPush, HttpPushArgs(..), makeHttpPush,
	HttpPushTest(..), HttpPushTestArgs(..),
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
import Data.Pipe.IO
import Data.Pipe.TChan
import Text.XML.Pipe
import Network.TigHTTP.Server

import qualified Data.ByteString.Lazy as LBS

import Network.XmlPush
import Network.XmlPush.HttpPush.Common

data HttpPush h = HttpPush {
	needReply :: TVar Bool,
	clientReadChan :: TChan (XmlNode, Bool),
	clientWriteChan :: TChan (Maybe XmlNode),
	serverReadChan :: TChan (XmlNode, Bool),
	serverWriteChan :: TChan (Maybe XmlNode) }

instance XmlPusher HttpPush where
	type NumOfHandle HttpPush = Two
	type PusherArgs HttpPush = HttpPushArgs
	generate (Two ch sh) = makeHttpPush [] ch sh
--	readFrom hp = fromTChans [clientReadChan hp, serverReadChan hp] =$=
	readFrom hp = fromTChans [serverReadChan hp, clientReadChan hp] =$=
		setNeedReply (needReply hp)
	writeTo hp = (convert ((() ,) . Just) =$=) . toTChansM $ do
		nr <- liftBase . atomically . readTVar $ needReply hp
		liftBase . atomically $ writeTVar (needReply hp) False
		return [
			(const nr, serverWriteChan hp),
			(const True, clientWriteChan hp) ]

data HttpPushTest h = HttpPushTest (HttpPush h)
data HttpPushTestArgs h = HttpPushTestArgs (HttpPushArgs h) [XmlNode]

instance XmlPusher HttpPushTest where
	type NumOfHandle HttpPushTest = Two
	type PusherArgs HttpPushTest = HttpPushTestArgs
	generate (Two ch sh) (HttpPushTestArgs a p) =
		HttpPushTest <$> makeHttpPush p ch sh a
--	readFrom hp = fromTChans [clientReadChan hp, serverReadChan hp] =$=
	readFrom (HttpPushTest hp) = fromTChans [serverReadChan hp, clientReadChan hp] =$=
		setNeedReply (needReply hp)
	writeTo (HttpPushTest hp) = (convert ((() ,) . Just) =$=) . toTChansM $ do
		nr <- liftBase . atomically . readTVar $ needReply hp
		liftBase . atomically $ writeTVar (needReply hp) False
		return [
			(const nr, serverWriteChan hp),
			(const True, clientWriteChan hp) ]

makeHttpPush :: (HandleLike h, MonadBaseControl IO (HandleMonad h)) =>
	[XmlNode] ->
	(Maybe h) -> (Maybe h) ->
	HttpPushArgs h -> HandleMonad h (HttpPush h)
makeHttpPush pre mch msh (HttpPushArgs gc gs hi gp wr) = do
	vch <- liftBase . atomically $ newTVar mch
	vsh <- liftBase . atomically $ newTVar msh
	v <- liftBase . atomically $ newTVar False
	vhi <- liftBase . atomically $ newTVar hi
	(ci, co) <- clientC vch vhi gp
	liftBase $ putStrLn "after clientC"
	(si, so) <- talk pre wr vsh vch vhi gc gs
	return $ HttpPush v ci co si so

clientC :: (HandleLike h, MonadBaseControl IO (HandleMonad h)) =>
	TVar (Maybe h) -> TVar (Maybe (String, Int, FilePath)) ->
	(XmlNode -> FilePath) ->
	HandleMonad h (TChan (XmlNode, Bool), TChan (Maybe XmlNode))
clientC vh vhi gp = do
	inc <- liftBase $ atomically newTChan
	otc <- liftBase $ atomically newTChan
	void . liftBaseDiscard forkIO $ do
		liftBase $ putStrLn "clientC: forked"
		h <- liftBase . atomically $ do
			mh <- readTVar vh
			case mh of
				Just h -> return h
				_ -> retry
		liftBase $ putStrLn "clientC: got client handle"
		(hn, pn, pt) <- liftBase . atomically $ do
			mhi <- readTVar vhi
			case mhi of
				Just hi -> return hi
				_ -> retry
		liftBase $ putStrLn "clientC: got client params"
		runPipe_ $ fromTChan otc
			=$= filter isJust
			=$= convert fromJust
			=$= clientLoop h hn pn pt gp (convert id)
			=$= convert (, False)
			=$= toTChan inc
	return (inc, otc)

talk :: (HandleLike h, MonadBaseControl IO (HandleMonad h)) =>
	[XmlNode] ->
	(XmlNode -> Bool) -> (TVar (Maybe h)) -> (TVar (Maybe h)) ->
	(TVar (Maybe (String, Int, FilePath))) ->
	(XmlNode -> Maybe (HandleMonad h h, String, Int, FilePath)) ->
	Maybe (HandleMonad h h) ->
	HandleMonad h (TChan (XmlNode, Bool), TChan (Maybe XmlNode))
talk pre wr vh vch vhi gc mgs = do
	liftBase . putStrLn $ "talk begin"
	inc <- liftBase $ atomically newTChan
	otc <- liftBase $ atomically newTChan
	void . liftBaseDiscard forkIO $ do
		flip (maybe (return ())) mgs $ \gs -> do
			h <- gs
			liftBase . atomically $ writeTVar vh (Just h)
		liftBase . putStrLn $ "talk: before get handle"
		h <- liftBase . atomically $ do
			mh <- readTVar vh
			case mh of
				Just h -> return h
				_ -> retry
		liftBase . putStrLn $ "talk: after get handle"
		runPipe_ . writeToChan h inc otc pre $
			setClient vch vhi gc =$= checkReply wr otc
		liftBase . putStrLn $ "talk: before runPipe_"
		runPipe_ . forever $ do
			lift . liftBase . putStrLn $ "before getRequest"
			req <- lift $ getRequest h
			lift . liftBase . putStrLn $ "getRequest done"
			requestBody req
				=$= xmlEvent
				=$= convert fromJust
				=$= xmlNode []
				=$= setClient vch vhi gc
				=$= checkReply wr otc
				=$= debug
				=$= toTChan inc
			fromTChan otc =$= await >>= maybe (return ()) (\mn ->
				lift . putResponse h . responseP $ case mn of
					Just n -> LBS.fromChunks [xmlString [n]]
					_ -> "")
	return (inc, otc)

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

setClient :: (MonadBase IO (HandleMonad h)) =>
	TVar (Maybe h) -> TVar (Maybe (String, Int, FilePath)) ->
	(XmlNode -> Maybe (HandleMonad h h, String, Int, FilePath)) ->
	Pipe XmlNode XmlNode (HandleMonad h) ()
setClient vch vhi gc = (await >>=) . maybe (return ()) $ \n -> do
	yield n
	case gc n of
		Just (gh, hn, pn, pt) -> do
			lift . liftBase $ putStrLn "setClient"
			h <- lift gh
			lift . liftBase . atomically . writeTVar vch $ Just h
			lift . liftBase . atomically . writeTVar vhi
				$ Just (hn, pn, pt)
		_ -> return ()
	setClient vch vhi gc
