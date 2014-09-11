{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, ScopedTypeVariables,
	PackageImports #-}

module Network.XmlPush.HttpPull.Tls.Client (
	HttpPullTlsCl, HttpPullClArgs(..), TlsArgs(..)) where

import Prelude hiding (filter)

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Concurrent.STM
import Data.HandleLike
import Data.Pipe
import Data.Pipe.TChan
import Text.XML.Pipe
import Network.PeyoTLS.TChan.Client
import "crypto-random" Crypto.Random

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Network.XmlPush
import Network.XmlPush.HttpPull.Client.Common
import Network.XmlPush.Tls

data HttpPullTlsCl h = HttpPullTlsCl
	(Pipe () XmlNode (HandleMonad h) ())
	(Pipe XmlNode () (HandleMonad h) ())

instance XmlPusher HttpPullTlsCl where
	type NumOfHandle HttpPullTlsCl = One
	type PusherArg HttpPullTlsCl = (HttpPullClArgs, TlsArgs)
	generate = makeHttpPull
	readFrom (HttpPullTlsCl r _) = r
	writeTo (HttpPullTlsCl _ w) = w

data TChanHandle = TChanHandle (TChan BS.ByteString) (TChan BS.ByteString)

instance HandleLike TChanHandle where
	type HandleMonad TChanHandle = IO
	hlPut (TChanHandle _ o) = atomically . writeTChan o
	hlGet (TChanHandle i _) = atomically . getBS i
	hlGetLine (TChanHandle i _) = atomically $ bsGetLine i
	hlGetContent (TChanHandle i _) = atomically $ readTChan i
	hlDebug _ "critical" = BSC.putStrLn
	hlDebug _ _ = const $ return ()
	hlClose = const $ return ()

bsGetLine :: TChan BS.ByteString -> STM BS.ByteString
bsGetLine c = do
	bs <- readTChan c
	case BSC.span (/= '\n') bs of
		(_, "") -> (bs `BS.append`) <$> bsGetLine c
		(l, r) -> do
			unGetTChan c $ BS.tail r
			return $ chomp l

chomp :: BS.ByteString -> BS.ByteString
chomp bs = case (BSC.null bs, BSC.init bs, BSC.last bs) of
	(True, _, _) -> bs
	(_, ln, '\r') -> ln
	_ -> bs

getBS :: TChan BS.ByteString -> Int -> STM BS.ByteString
getBS _ 0 = return ""
getBS i n = do
	bs <- readTChan i
	if BS.length bs > n
	then do	let (rtn, rst) = BS.splitAt n bs
		unGetTChan i rst
		return rtn
	else (bs `BS.append`) <$> getBS i (n - BS.length bs)

makeHttpPull :: (ValidateHandle h, MonadBaseControl IO (HandleMonad h)) =>
	One h -> (HttpPullClArgs, TlsArgs) -> HandleMonad h (HttpPullTlsCl h)
makeHttpPull (One h)
	(HttpPullClArgs hn pn fp pl ip gd gp, TlsArgs ca kcs) = do
	dr <- liftBase . atomically $ newTVar Nothing
	(inc, otc) <- do
		(g :: SystemRNG) <- liftBase $ cprgCreate <$> createEntropyPool
		(ic, oc) <- open' h "localhost"
			["TLS_RSA_WITH_AES_128_CBC_SHA"] kcs ca g
		liftBase $ talkC (TChanHandle ic oc) hn pn fp gp pl ip dr gd
	return $ HttpPullTlsCl (fromTChan inc) (toTChan otc)
