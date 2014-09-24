{-# LANGUAGE
	PatternGuards,
	OverloadedStrings, TypeFamilies, FlexibleContexts,
	PackageImports #-}

module Network.XmlPush.Xmpp.Server (
	XmppServer, XmppServerArgs(..),
	) where

import Prelude hiding (filter)

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Monad.Base
import Control.Concurrent.STM
import Data.Maybe
import Data.HandleLike
import Data.Pipe
import Data.Pipe.Flow
import Data.Pipe.IO
import Text.XML.Pipe
import Network.XMPiPe.Core.C2S.Server
import Network.Sasl

import Network.XmlPush
import Network.XmlPush.Xmpp.Common
import Network.XmlPush.Xmpp.Server.Common

data XmppServer h = XmppServer
	(Pipe () XmlNode (HandleMonad h) ())
	(Pipe XmlNode () (HandleMonad h) ())

instance XmlPusher XmppServer where
	type NumOfHandle XmppServer = One
	type PusherArgs XmppServer = XmppServerArgs
	generate = makeXmppServer
	readFrom (XmppServer r _) = r
	writeTo (XmppServer _ w) = w

makeXmppServer :: (
	HandleLike h,
	MonadError (HandleMonad h), SaslError (ErrorType (HandleMonad h)),
	MonadBase IO (HandleMonad h) ) =>
	One h -> XmppServerArgs h -> HandleMonad h (XmppServer h)
makeXmppServer (One h) (XmppServerArgs dn ps inr ynr) = do
	rids <- liftBase $ atomically newTChan
	(Just ns, st) <- (`runStateT` initXSt dn) . runPipe $ do
		fromHandleLike (THandle h)
			=$= sasl dn (retrieves dn ps)
			=$= toHandleLike (THandle h)
		fromHandleLike (THandle h)
			=$= bind dn []
			=@= toHandleLike (THandle h)
	liftBase . print $ user st
	let	r = fromHandleLike h
			=$= input ns
			=$= debug
			=$= setIds h ynr rids
			=$= convert fromMessage
			=$= filter isJust
			=$= convert fromJust
		w = makeMpi (user st) inr rids
			=$= debug
			=$= output
			=$= toHandleLike h
	return $ XmppServer r w
