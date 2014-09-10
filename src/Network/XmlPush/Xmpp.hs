{-# LANGUAGE OverloadedStrings,
	TypeFamilies, FlexibleContexts, ScopedTypeVariables,
	PackageImports #-}

module Network.XmlPush.Xmpp (Xmpp, XmppArgs(..)) where

import Prelude hiding (filter)

import Control.Applicative
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Writer
import "monads-tf" Control.Monad.Error
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
import Network.XMPiPe.Core.C2S.Client
import "crypto-random" Crypto.Random

import qualified Data.ByteString as BS

import Network.XmlPush
import Network.XmlPush.Xmpp.Common

data Xmpp h = Xmpp
	(XmlNode -> Bool)
	(TChan (Maybe BS.ByteString))
	(Pipe () Mpi (HandleMonad h) ())
	(TChan (Either BS.ByteString XmlNode))

instance XmlPusher Xmpp where
	type NumOfHandle Xmpp = One
	type PusherArg Xmpp = XmppArgs
	generate = makeXmpp
	readFrom (Xmpp wr nr r wc) = r
		=$= pushId wr nr wc
		=$= convert fromMessage
		=$= filter isJust
		=$= convert fromJust
	writeTo (Xmpp _ _ _ w) = convert maybeToEither =$= toTChan w

makeXmpp :: (
	HandleLike h, MonadBaseControl IO (HandleMonad h),
	MonadError (HandleMonad h), Error (ErrorType (HandleMonad h))
	) => One h -> XmppArgs -> HandleMonad h (Xmpp h)
makeXmpp (One h) (XmppArgs ms wr inr me ps you) = do
	nr <- liftBase $ atomically newTChan
	wc <- liftBase $ atomically newTChan
	(g :: SystemRNG) <- liftBase $ cprgCreate <$> createEntropyPool
	let	(cn, _g') = cprgGenerate 32 g
		(Jid un d (Just rsc)) = me
		ss = St [
			("username", un), ("authcid", un), ("password", ps),
			("cnonce", cn) ]
	void . (`evalStateT` ss) . runPipe $ fromHandleLike (THandle h)
		=$= sasl d ms
		=$= toHandleLike (THandle h)
	(Just ns, _fts) <- runWriterT . runPipe $ fromHandleLike (THandle h)
		=$= bind d rsc
		=@= toHandleLike (THandle h)
	runPipe_ $ yield (Presence tagsNull []) =$= output =$= toHandleLike h
	(>> return ()) . liftBaseDiscard forkIO . runPipe_ $ fromTChan wc
		=$= addRandom =$= makeResponse inr you nr =$= output =$= toHandleLike h
	let	r = fromHandleLike h =$= input ns
	return $ Xmpp wr nr r wc
