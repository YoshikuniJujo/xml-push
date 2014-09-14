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
import Text.XML.Pipe
import Network.TigHTTP.Client
import Network.TigHTTP.Server
import Network.TigHTTP.Types

import qualified Data.ByteString.Lazy as LBS

data HttpPushArgs h = HttpPushArgs {
	hostName :: String,
	portNumber :: Int,
	basePath :: FilePath,
	getPath :: XmlNode -> FilePath,
	youNeedResponse :: XmlNode -> Bool
	}

setNeedReply :: MonadBase IO m => TVar Bool -> Pipe (a, Bool) a m ()
setNeedReply nr = await >>= maybe (return ()) (\(x, b) ->
	lift (liftBase . atomically $ writeTVar nr b) >> yield x >> setNeedReply nr)

clientLoop :: (HandleLike h, MonadBaseControl IO (HandleMonad h)) =>
	h -> String -> Int -> FilePath -> (XmlNode -> FilePath) ->
	Pipe XmlNode XmlNode (HandleMonad h) ()
clientLoop h hn pn pt gp = (await >>=) . maybe (return ()) $ \n -> do
	r <- lift . request h $ post hn pn (pt ++ "/" ++ gp n)
		(Nothing, LBS.fromChunks [xmlString [n]])
	return ()
		=$= responseBody r
		=$= xmlEvent
		=$= convert fromJust
		=$= void (xmlNode [])
	clientLoop h hn pn pt gp

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
