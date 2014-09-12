{-# LANGUAGE OverloadedStrings,
	TypeFamilies, FlexibleContexts, ScopedTypeVariables,
	PackageImports #-}

module Network.XmlPush.Xmpp.Tls (XmppTls, XmppArgs(..), TlsArgs(..)) where

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
import Network.PeyoTLS.TChan.Client
import "crypto-random" Crypto.Random

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Network.XmlPush
import Network.XmlPush.Xmpp.Common
import Network.XmlPush.Tls.Client

data XmppTls h = XmppTls
	(XmlNode -> Bool)
	(TChan (Maybe BS.ByteString))
	(Pipe () Mpi (HandleMonad h) ())
	(TChan (Either BS.ByteString XmlNode))

instance XmlPusher XmppTls where
	type NumOfHandle XmppTls = One
	type PusherArgs XmppTls = (XmppArgs, TlsArgs)
	generate = makeXmppTls
	readFrom (XmppTls wr nr r wc) = r
		=$= pushId wr nr wc
		=$= convert fromMessage
		=$= filter isJust
		=$= convert fromJust
	writeTo (XmppTls _ _nr _ w) = convert maybeToEither =$= toTChan w

makeXmppTls :: (
	ValidateHandle h, MonadBaseControl IO (HandleMonad h),
	MonadError (HandleMonad h), Error (ErrorType (HandleMonad h))
	) => One h -> (XmppArgs, TlsArgs) -> HandleMonad h (XmppTls h)
makeXmppTls (One h) (XmppArgs ms me ps you inr wr, TlsArgs dn cs ca kcs) = do
	nr <- liftBase $ atomically newTChan
	wc <- liftBase $ atomically newTChan
	(g :: SystemRNG) <- liftBase $ cprgCreate <$> createEntropyPool
	let	(Jid un d (Just rsc)) = me
		(cn, g') = cprgGenerate 32 g
		ss = St [
			("username", un), ("authcid", un), ("password", ps),
			("cnonce", cn) ]
	runPipe_ $ fromHandleLike h =$= starttls (BSC.pack dn) =$= toHandleLike h
	(inc, otc) <- open' h dn cs kcs ca g'
	(`evalStateT` ss) . runPipe_ $ fromTChan inc =$= sasl d ms =$= toTChan otc
	(Just ns, _fts) <- runWriterT . runPipe $ fromTChan inc
		=$= bind d rsc
		=@= toTChan otc
	runPipe_ $ yield (Presence tagsNull []) =$= output =$= toTChan otc
	(>> return ()) . liftBaseDiscard forkIO . runPipe_ $ fromTChan wc
		=$= addRandom =$= makeResponse inr you nr =$= output =$= toTChan otc
	let	r = fromTChan inc =$= input ns
	return $ XmppTls wr nr r wc
