{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts,
	PackageImports #-}

module Network.XmlPush.Xmpp.Server (
	XmppServer, Null(..),
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
import Network.XMPiPe.Core.C2S.Client (toJid)
import Network.Sasl

import qualified Data.ByteString as BS
import qualified Network.Sasl.DigestMd5.Server as DM5
import qualified Network.Sasl.ScramSha1.Server as SS1

import Network.XmlPush
import Network.XmlPush.Xmpp.Common

data XmppServer h = XmppServer
	(Pipe () XmlNode (HandleMonad h) ())
	(Pipe XmlNode () (HandleMonad h) ())

data Null h = Null

instance XmlPusher XmppServer where
	type NumOfHandle XmppServer = One
	type PusherArgs XmppServer = Null
	generate = makeXmppServer
	readFrom (XmppServer r _) = r
	writeTo (XmppServer _ w) = w

makeXmppServer :: (
	HandleLike h,
	MonadError (HandleMonad h), SaslError (ErrorType (HandleMonad h)),
	MonadBase IO (HandleMonad h) ) =>
	One h -> Null h -> HandleMonad h (XmppServer h)
makeXmppServer (One h) _ = do
	rids <- liftBase $ atomically newTChan
	(Just ns, _st) <- (`runStateT` initXSt) . runPipe $ do
		fromHandleLike (THandle h)
			=$= sasl "localhost" retrieves
			=$= toHandleLike (THandle h)
		fromHandleLike (THandle h)
			=$= bind "localhost" []
			=@= toHandleLike (THandle h)
	let	r = fromHandleLike h
			=$= input ns
			=$= debug
			=$= setIds h rids
			=$= convert fromMessage
			=$= filter isJust
			=$= convert fromJust
		w = makeMpi rids
			=$= debug
			=$= output
			=$= toHandleLike h
	return $ XmppServer r w

makeMpi :: MonadBase IO m => TChan BS.ByteString -> Pipe XmlNode Mpi m ()
makeMpi rids = (await >>=) . maybe (return ()) $ \n -> do
	e <- lift . liftBase . atomically $ isEmptyTChan rids
	if e
	then yield $ Message (tagsType "chat") [n]
	else do	i <- lift . liftBase .atomically $ readTChan rids
		lift . liftBase . putStrLn $ "makeMpi: " ++ show i
		yield $ Iq (tagsType "return") {
			tagId = Just i
			} [n]
	makeMpi rids

setIds :: (HandleLike h, MonadBase IO (HandleMonad h)) =>
	h -> TChan BS.ByteString -> Pipe Mpi Mpi (HandleMonad h) ()
setIds h rids = (await >>=) . maybe (return ()) $ \mpi -> do
	yield mpi
	if sampleYouNeedResponse mpi
	then when (isGetSet mpi) . lift . liftBase . atomically
		$ writeTChan rids (fromJust $ getId mpi)
	else lift $ returnEmpty h "hoge"
	lift . liftBase . putStrLn $ "\nsetIds: " ++ show (getId mpi)
	setIds h rids

isGetSet :: Mpi -> Bool
isGetSet (Iq Tags { tagType = Just "set" } _) = True
isGetSet (Iq Tags { tagType = Just "get" } _) = True
isGetSet _ = False

getId :: Mpi -> Maybe BS.ByteString
getId (Iq t _) = tagId t
getId (Message t _) = tagId t
getId _ = Nothing

sampleYouNeedResponse :: Mpi -> Bool
sampleYouNeedResponse (Iq _ [XmlNode (_, "no_response") _ _ _]) = False
sampleYouNeedResponse _ = True

returnEmpty :: (HandleLike h, MonadBase IO (HandleMonad h)) => h -> BS.ByteString -> HandleMonad h ()
returnEmpty h i = runPipe_ $ yield e =$= output =$= debug =$= toHandleLike h
	where
	you = toJid "hoge@hogehost"
	e = Iq (tagsType "result") { tagId = Just i, tagTo = Just you } []

initXSt :: XSt
initXSt = XSt {
	user = Jid "" "localhost" Nothing, rands = repeat "00DEADBEEF00",
	sSt = [	("realm", "localhost"), ("qop", "auth"), ("charset", "utf-8"),
		("algorithm", "md5-sess") ] }

retrieves :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m) ) => [Retrieve m]
retrieves = [RTPlain retrievePln, RTDigestMd5 retrieveDM5, RTScramSha1 retrieveSS1]

retrievePln :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m)) =>
	BS.ByteString -> BS.ByteString -> BS.ByteString -> m ()
retrievePln "" "yoshikuni" "password" = return ()
retrievePln "" "yoshio" "password" = return ()
retrievePln _ _ _ = throwError $ fromSaslError NotAuthorized "auth failure"

retrieveDM5 :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m) ) => BS.ByteString -> m BS.ByteString
retrieveDM5 "yoshikuni" = return $ DM5.mkStored "yoshikuni" "localhost" "password"
retrieveDM5 "yoshio" = return $ DM5.mkStored "yoshio" "localhost" "password"
retrieveDM5 _ = throwError $ fromSaslError NotAuthorized "auth failure"

retrieveSS1 :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m) ) => BS.ByteString ->
	m (BS.ByteString, BS.ByteString, BS.ByteString, Int)
retrieveSS1 "yoshikuni" = return (slt, stk, svk, i)
	where slt = "pepper"; i = 4492; (stk, svk) = SS1.salt "password" slt i
retrieveSS1 "yoshio" = return (slt, stk, svk, i)
	where slt = "sugar"; i = 4492; (stk, svk) = SS1.salt "password" slt i
retrieveSS1 _ = throwError $ fromSaslError NotAuthorized "auth failure"

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
