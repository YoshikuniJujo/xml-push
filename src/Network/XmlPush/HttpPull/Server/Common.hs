{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

data HttpPullSvArgs h = HttpPullSvArgs {
	isPoll :: XmlNode -> Bool,
	noPending :: XmlNode,
	youNeedResponse :: XmlNode -> Bool
	}

runXml :: (HandleLike h, MonadBaseControl IO (HandleMonad h)) =>
	[XmlNode] ->
	h -> (XmlNode -> Bool) -> XmlNode -> (XmlNode -> Bool) ->
	Pipe XmlNode XmlNode (HandleMonad h) () ->
	HandleMonad h (TChan XmlNode, TChan XmlNode)
runXml pre h ip ep ynr cn = do
	inc <- liftBase $ atomically newTChan
	otc <- liftBase $ atomically newTChan
	_ <- liftBaseDiscard forkIO . runPipe_ $ do
		writeToChan h inc otc pre cn
		talk h ip ep ynr inc otc cn
	return (inc, otc)

writeToChan :: (HandleLike h, MonadBase IO (HandleMonad h)) => h ->
	TChan XmlNode -> TChan XmlNode ->
	[XmlNode] -> Pipe XmlNode XmlNode (HandleMonad h) () ->
	Pipe () () (HandleMonad h) ()
writeToChan h inc otc pre cn = do
	mapM yield pre =$= cn =$= toTChan inc
	(fromTChan otc =$=) . (await >>=) . maybe (return ()) $ \n ->
		lift . putResponse h . responseP $ LBS.fromChunks [xmlString [n]]

talk :: (HandleLike h, MonadBase IO (HandleMonad h)) =>
	h -> (XmlNode -> Bool) -> XmlNode -> (XmlNode -> Bool) ->
	TChan XmlNode -> TChan XmlNode -> Pipe XmlNode XmlNode (HandleMonad h) () ->
	Pipe () () (HandleMonad h) ()
talk h ip ep ynr inc otc cn = do
	r <- lift $ getRequest h
	rns <- requestBody r
		=$= xmlEvent
		=$= convert fromJust
		=$= xmlNode []
		=$= cn
		=$= toList
	lift . hlDebug h "medium" $
		"\nxml-push: in: " `BS.append` xmlString rns `BS.append` "\n"
	case rns of
		[rn]	| ip rn -> (flushOr otc ep =$=) . (await >>=)
				. maybe (return ()) $ \n -> lift $ do
					let rt = xmlString [n]
					hlDebug h "medium" $ BS.concat [
						"xml-push: out:",  rt, "\n" ]
					putResponse h . responseP
						$ LBS.fromChunks [rt]
			| not $ ynr rn -> do
				mapM_ yield rns =$= toTChan inc
				lift $ do
					hlDebug h "medium"
						"xml-push: out: (empty)\n"
					putResponse h $ responseP ""
		_ -> do	mapM_ yield rns =$= toTChan inc
			(fromTChan otc =$=) . (await >>=) . maybe (return ()) $
				\n -> lift $ do
					let rt = xmlString [n]
					hlDebug h "medium" $ BS.concat [
						"xml-push: out:",  rt, "\n" ]
					putResponse h . responseP
						$ LBS.fromChunks [rt]
	talk h ip ep ynr inc otc cn

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
