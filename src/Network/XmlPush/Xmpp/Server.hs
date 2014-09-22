{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts,
	PackageImports #-}

module Network.XmlPush.Xmpp.Server (
	XmppServer, Null(..),
	) where

import Prelude hiding (filter)

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Maybe
import Data.HandleLike
import Data.Pipe
import Data.Pipe.Flow
import Network.XMPiPe.Core.C2S.Server
import Network.Sasl

import qualified Data.ByteString as BS

import Network.XmlPush
import Network.XmlPush.Xmpp.Common

data XmppServer h = XmppServer
	(Pipe () Mpi (HandleMonad h) ())
	(Pipe Mpi () (HandleMonad h) ())

data Null h = Null

instance XmlPusher XmppServer where
	type NumOfHandle XmppServer = One
	type PusherArgs XmppServer = Null
	generate = makeXmppServer
	readFrom (XmppServer r _) = r
		=$= convert fromMessage
		=$= filter isJust
		=$= convert fromJust
	writeTo (XmppServer _ w) = convert (Message (tagsType "chat") . (: []))
		=$= w

makeXmppServer :: (
	HandleLike h,
	MonadError (HandleMonad h), SaslError (ErrorType (HandleMonad h))
	) =>
	One h -> Null h -> HandleMonad h (XmppServer h)
makeXmppServer (One h) _ = do
	_ <- (`runStateT` initXSt) . runPipe $ do
		fromHandleLike (THandle h)
			=$= sasl "localhost" retrieves
			=$= toHandleLike (THandle h)
		fromHandleLike (THandle h)
			=$= bind "localhost" []
			=@= toHandleLike (THandle h)
	return undefined

initXSt :: XSt
initXSt = XSt {
	user = Jid "" "localhost" Nothing, rands = repeat "00DEADBEEF00",
	sSt = [	("realm", "localhost"), ("qop", "auth"), ("charset", "utf-8"),
		("algorithm", "md5-sess") ] }

retrieves :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m) ) => [Retrieve m]
retrieves = [RTPlain retrievePln]

retrievePln :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m)) =>
	BS.ByteString -> BS.ByteString -> BS.ByteString -> m ()
retrievePln "" "yoshikuni" "password" = return ()
retrievePln "" "yoshio" "password" = return ()
retrievePln _ _ _ = throwError $ fromSaslError NotAuthorized "auth failure"

type Pairs a = [(a, a)]
data XSt = XSt { user :: Jid, rands :: [BS.ByteString], sSt :: Pairs BS.ByteString }

instance XmppState XSt where
	getXmppState xs = (user xs, rands xs)
	putXmppState (usr, rl) xs = xs { user = usr, rands = rl }

instance SaslState XSt where
	getSaslState XSt { user = Jid n _ _, rands = nnc : _, sSt = ss } =
		("username", n) : ("nonce", nnc) : ("snonce", nnc) : ss
	getSaslState _ = error "XSt.getSaslState: null random list"
	putSaslState ss xs@XSt { user = Jid _ d r, rands = _ : rs } =
		xs { user = Jid n d r, rands = rs, sSt = ss }
		where Just n = lookup "username" ss
	putSaslState _ _ = error "XSt.getSaslState: null random list"
