{-# LANGUAGE TypeFamilies, FlexibleContexts, PackageImports #-}

module Network.XmlPush.Http.Server (
	HttpSv,
	HttpSvArgs(..), Mechanism(..),
	HttpPullSvArgs(HttpPullSvArgs), HttpPushArgs(HttpPushArgs),
	) where

import Control.Monad
import "monads-tf" Control.Monad.Error
-- import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Maybe
import Data.HandleLike
import Data.Pipe
import Data.Pipe.List
import Text.XML.Pipe
import Network.XmlPush
import Network.XmlPush.HttpPull.Server.Body
import Network.XmlPush.HttpPush.Body
import Network.TigHTTP.Server
import Network.Sasl
import Network.PeyoTLS.Server

newtype HttpSv h = HttpSv (Either (HttpPullSv h) (HttpPush h))

data Mechanism = Pull | Push deriving Show

data HttpSvArgs h =
	HttpSvArgs (XmlNode -> Mechanism) (HttpPullSvArgs h) (HttpPushArgs h)

instance XmlPusher HttpSv where
	type NumOfHandle HttpSv = Two
	type PusherArgs HttpSv = HttpSvArgs
	generate (Two ch (Just sh)) (HttpSvArgs s pla psa) =
		makeHttpSv ch sh s pla psa
	generate _ _ = error "bad"
	readFrom (HttpSv e) = either readFrom readFrom e
	writeTo (HttpSv e) = either writeTo writeTo e

makeHttpSv :: (
	ValidateHandle h, MonadBaseControl IO (HandleMonad h),
	MonadError (HandleMonad h), SaslError (ErrorType (HandleMonad h))
	) => Maybe h -> h -> (XmlNode -> Mechanism) ->
	HttpPullSvArgs h -> HttpPushArgs h -> HandleMonad h (HttpSv h)
makeHttpSv ch sh s pla psa = do
	rq <- getRequest sh
--	liftBase . print $ requestPath r
	Just [rn] <- runPipe $ requestBody rq
		=$= xmlEvent
		=$= convert fromJust
		=$= xmlNode []
		=$= toList
--	liftBase . putStrLn $ "here"
	HttpSv `liftM` case s rn of
		Pull -> do
			HttpPullSvTest r w <- generate (One sh) $
				HttpPullSvTestArgs pla [rn]
--				HttpPullSvTestArgs pla []
			return . Left $ HttpPullSv r w
		Push -> do
			HttpPushTest ps <- generate (Two ch (Just sh)) $
				HttpPushTestArgs psa [rn]
			return $ Right ps
