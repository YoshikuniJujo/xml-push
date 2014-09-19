{-# LANGUAGE OverloadedStrings, TupleSections, TypeFamilies, FlexibleContexts,
	PackageImports #-}

module Network.XmlPush.HttpPush (HttpPush, HttpPushArgs(..)) where

import Prelude hiding (filter)

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
	generate (Two ch sh) = makeHttpPush ch sh
--	readFrom hp = fromTChans [clientReadChan hp, serverReadChan hp] =$=
	readFrom hp = fromTChans [serverReadChan hp, clientReadChan hp] =$=
		setNeedReply (needReply hp)
	writeTo hp = (convert ((() ,) . Just) =$=) . toTChansM $ do
		nr <- liftBase . atomically . readTVar $ needReply hp
		liftBase . atomically $ writeTVar (needReply hp) False
		return [
			(const nr, serverWriteChan hp),
			(const True, clientWriteChan hp) ]

makeHttpPush :: (HandleLike h, MonadBaseControl IO (HandleMonad h)) =>
	TVar (Maybe h) -> TVar (Maybe h) ->
	HttpPushArgs h -> HandleMonad h (HttpPush h)
makeHttpPush vch vsh (HttpPushArgs gc gs (hn, pn, pt) gp wr) = do
	v <- liftBase . atomically $ newTVar False
	(ci, co) <- clientC vch (hn, pn, pt) gp
	(si, so) <- talk wr vsh vch gc gs
	return $ HttpPush v ci co si so

clientC :: (HandleLike h, MonadBaseControl IO (HandleMonad h)) =>
	TVar (Maybe h) -> (String, Int, FilePath) -> (XmlNode -> FilePath) ->
	HandleMonad h (TChan (XmlNode, Bool), TChan (Maybe XmlNode))
clientC vh (hn, pn, pt) gp = do
	inc <- liftBase $ atomically newTChan
	otc <- liftBase $ atomically newTChan
	void . liftBaseDiscard forkIO $ do
		h <- liftBase . atomically $ do
			mh <- readTVar vh
			case mh of
				Just h -> return h
				_ -> retry
		runPipe_ $ fromTChan otc
			=$= filter isJust
			=$= convert fromJust
			=$= clientLoop h hn pn pt gp
			=$= convert (, False)
			=$= toTChan inc
	return (inc, otc)

talk :: (HandleLike h, MonadBaseControl IO (HandleMonad h)) =>
	(XmlNode -> Bool) -> (TVar (Maybe h)) -> (TVar (Maybe h)) ->
	(XmlNode -> Maybe (HandleMonad h h)) -> Maybe (HandleMonad h h) ->
	HandleMonad h (TChan (XmlNode, Bool), TChan (Maybe XmlNode))
talk wr vh vch gc mgs = do
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
		runPipe_ . forever $ do
			req <- lift $ getRequest h
			requestBody req
				=$= xmlEvent
				=$= convert fromJust
				=$= xmlNode []
				=$= setClient vch gc
				=$= checkReply wr otc
				=$= toTChan inc
			fromTChan otc =$= await >>= maybe (return ()) (\mn ->
				lift . putResponse h . responseP $ case mn of
					Just n -> LBS.fromChunks [xmlString [n]]
					_ -> "")
	return (inc, otc)

setClient :: (MonadBase IO (HandleMonad h)) =>
	TVar (Maybe h) -> (XmlNode -> Maybe (HandleMonad h h)) ->
	Pipe XmlNode XmlNode (HandleMonad h) ()
setClient vch gc = (await >>=) . maybe (return ()) $ \n -> do
	yield n
	case gc n of
		Just gh -> do
			h <- lift gh
			lift . liftBase . atomically . writeTVar vch $ Just h
		_ -> return ()
	setClient vch gc
