{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables,
	PackageImports #-}

module Network.XmlPush.Xmpp.Tls.Server (
	XmppTlsServer,
	XmppTlsServerArgs(..), XmppServerArgs(..), TlsArgs(..),
	) where

import Prelude hiding (filter)

import Control.Applicative
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Concurrent.STM
import Data.Maybe
import Data.HandleLike
import Data.Pipe
import Data.Pipe.Flow
import Data.Pipe.IO
import Data.Pipe.TChan
import Data.UUID
import System.Random
import Text.XML.Pipe
import Network.XMPiPe.Core.C2S.Server
import Network.XmlPush
import Network.Sasl
import Network.PeyoTLS.TChan.Server
import "crypto-random" Crypto.Random

-- import qualified Data.ByteString.Char8 as BSC

import Network.XmlPush.Xmpp.Common
import Network.XmlPush.Xmpp.Server.Common
import Network.XmlPush.Tls.Server

data XmppTlsServer h = XmppTlsServer
	(Pipe () XmlNode (HandleMonad h) ())
	(Pipe XmlNode () (HandleMonad h) ())

data XmppTlsServerArgs h = XmppTlsServerArgs (XmppServerArgs h) TlsArgs

instance XmlPusher XmppTlsServer where
	type NumOfHandle XmppTlsServer = One
	type PusherArgs XmppTlsServer = XmppTlsServerArgs
	generate = makeXmppTlsServer
	readFrom (XmppTlsServer r _) = r
	writeTo (XmppTlsServer _ w) = w

makeXmppTlsServer :: (
	ValidateHandle h,
	MonadError (HandleMonad h), SaslError (ErrorType (HandleMonad h)),
	MonadBaseControl IO (HandleMonad h) ) =>
	One h -> XmppTlsServerArgs h -> HandleMonad h (XmppTlsServer h)
makeXmppTlsServer (One h) (XmppTlsServerArgs
	(XmppServerArgs dn ps inr ynr)
	(TlsArgs gn cc cs mca kcs)) = do
	rids <- liftBase $ atomically newTChan
	(g :: SystemRNG) <- liftBase $ cprgCreate <$> createEntropyPool
	us <- liftBase $ map toASCIIBytes . randoms <$> getStdGen
	_ <- (`execStateT` us) . runPipe_ $ fromHandleLike (THandle h)
		=$= starttls dn
		=$= toHandleLike (THandle h)
	(Just (cn, _), (inp, otp)) <- open h cs kcs mca g
	(Just ns, st) <- (`runStateT` initXSt dn) . runPipe $ do
		fromTChan inp =$= sasl dn (retrieves dn ps) =$= toTChan otp
		fromTChan inp =$= bind dn [] =@= toTChan otp
	liftBase . print $ user st
	let	r = fromTChan inp
			=$= input ns
			=$= debug
			=$= setIds h ynr rids
			=$= convert fromMessage
			=$= filter isJust
			=$= convert fromJust
		w = makeMpi (user st) inr rids
			=$= debug
			=$= output
			=$= toTChan otp
	return $ XmppTlsServer r w
