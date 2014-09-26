{-# LANGUAGE FlexibleContexts, PackageImports #-}

module Network.XmlPush.HttpPush.Common (
	HttpPushArgs(..),
	setNeedReply,
	clientLoop,
	checkReply,
	responseP,
	) where

import Control.Monad
import "monads-tf" Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Concurrent.STM
import Data.Maybe
import Data.HandleLike
import Data.Pipe
import Data.Pipe.IO
import Text.XML.Pipe
import Network.TigHTTP.Client
import Network.TigHTTP.Server
import Network.TigHTTP.Types

import qualified Data.ByteString.Lazy as LBS

data HttpPushArgs h = HttpPushArgs {
	getClient :: XmlNode -> Maybe (HandleMonad h h, String, Int, FilePath),
--	getClient :: XmlNode -> Maybe (HandleMonad h h),
	getServer :: Maybe (HandleMonad h h),
	hostName :: Maybe (String, Int, FilePath),
	getPath :: XmlNode -> FilePath,
	youNeedResponse :: XmlNode -> Bool
	}

setNeedReply :: MonadBase IO m => TVar Bool -> Pipe (a, Bool) a m ()
setNeedReply nr = await >>= maybe (return ()) (\(x, b) ->
	lift (liftBase . atomically $ writeTVar nr b) >> yield x >> setNeedReply nr)

clientLoop :: (HandleLike h, MonadBaseControl IO (HandleMonad h)) =>
	h -> String -> Int -> FilePath -> (XmlNode -> FilePath) ->
	Pipe XmlNode XmlNode (HandleMonad h) () ->
	Pipe XmlNode XmlNode (HandleMonad h) ()
clientLoop h hn pn pt gp p = (await >>=) . maybe (return ()) $ \n -> do
	lift . liftBase . putStrLn $ "before request" ++ show n
	r <- lift . request h $ post hn pn (pt ++ "/" ++ gp n)
		(Nothing, LBS.fromChunks [xmlString [n]])
	lift . liftBase . putStrLn $ "after request" ++ show n
	return ()
		=$= responseBody r
		=$= xmlEvent
		=$= convert fromJust
		=$= void (xmlNode [])
		=$= p
	clientLoop h hn pn pt gp p

checkReply :: MonadBase IO m => (XmlNode -> Bool) -> TChan (Maybe XmlNode) ->
	Pipe XmlNode (XmlNode, Bool) m ()
checkReply wr o = (await >>=) . maybe (return ()) $ \n ->
	if wr n
	then yield (n, True) >> checkReply wr o
	else do	lift (liftBase . atomically $ writeTChan o Nothing)
		yield (n, False)
		checkReply wr o

responseP :: HandleLike h => LBS.ByteString -> Response Pipe h
responseP = response
