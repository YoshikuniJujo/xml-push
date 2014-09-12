{-# LANGUAGE FlexibleContexts, PackageImports #-}

module Network.XmlPush.HttpPull.Server.Common (HttpPullSvArgs(..), runXml) where

import "monads-tf" Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Data.Maybe
import Data.HandleLike
import Data.Pipe
import Data.Pipe.List
import Data.Pipe.TChan
import Text.XML.Pipe
import Network.TigHTTP.Server
import Network.TigHTTP.Types

import qualified Data.ByteString.Lazy as LBS

data HttpPullSvArgs = HttpPullSvArgs {
	isPoll :: XmlNode -> Bool,
	noPending :: XmlNode
	}

runXml :: (HandleLike h, MonadBaseControl IO (HandleMonad h)) =>
	h -> (XmlNode -> Bool) -> XmlNode ->
	Pipe XmlNode XmlNode (HandleMonad h) () ->
	HandleMonad h (TChan XmlNode, TChan XmlNode)
runXml h ip ep cn = do
	inc <- liftBase $ atomically newTChan
	otc <- liftBase $ atomically newTChan
	_ <- liftBaseDiscard forkIO . runPipe_ $ talk h ip ep inc otc cn
	return (inc, otc)

talk :: (HandleLike h, MonadBase IO (HandleMonad h)) =>
	h -> (XmlNode -> Bool) -> XmlNode ->
	TChan XmlNode -> TChan XmlNode -> Pipe XmlNode XmlNode (HandleMonad h) () ->
	Pipe () () (HandleMonad h) ()
talk h ip ep inc otc cn = do
	r <- lift $ getRequest h
	lift . liftBase . print $ requestPath r
	rns <- requestBody r
		=$= xmlEvent
		=$= convert fromJust
		=$= xmlNode []
		=$= cn
		=$= toList
	if case rns of [n] -> ip n; _ -> False
	then (flushOr otc ep =$=) . (await >>=) . maybe (return ()) $ \n ->
		lift . putResponse h . responseP $ LBS.fromChunks [xmlString [n]]
	else do	mapM_ yield rns =$= toTChan inc
		(fromTChan otc =$=) . (await >>=) . maybe (return ()) $ \n ->
			lift . putResponse h . responseP
				$ LBS.fromChunks [xmlString [n]]
	talk h ip ep inc otc cn

responseP :: (HandleLike h, MonadBase IO (HandleMonad h)) =>
	LBS.ByteString -> Response Pipe h
responseP = response

flushOr :: MonadBase IO m => TChan XmlNode -> XmlNode -> Pipe () XmlNode m ()
flushOr c ep = do
	e <- lift . liftBase . atomically $ isEmptyTChan c
	lift . liftBase $ print e
	if e then yield ep else do
		po <- lift . liftBase . atomically $ readTChan c
		yield po
