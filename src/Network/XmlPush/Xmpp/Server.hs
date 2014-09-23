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
import Data.UUID
import System.Random
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

data XmppServerArgs h = XmppServerArgs {
	domainName :: BS.ByteString,
	iNeedResponse :: XmlNode -> Bool,
	youNeedResponse :: XmlNode -> Bool
	}

instance XmlPusher XmppServer where
	type NumOfHandle XmppServer = One
	type PusherArgs XmppServer = XmppServerArgs
	generate = makeXmppServer
	readFrom (XmppServer r _) = r
	writeTo (XmppServer _ w) = w

samplePasswords :: [(BS.ByteString, BS.ByteString)]
samplePasswords = [
	("yoshikuni", "password"),
	("yoshio", "password")
	]

makeXmppServer :: (
	HandleLike h,
	MonadError (HandleMonad h), SaslError (ErrorType (HandleMonad h)),
	MonadBase IO (HandleMonad h) ) =>
	One h -> XmppServerArgs h -> HandleMonad h (XmppServer h)
makeXmppServer (One h) (XmppServerArgs dn inr ynr) = do
	rids <- liftBase $ atomically newTChan
	(Just ns, st) <- (`runStateT` initXSt dn) . runPipe $ do
		fromHandleLike (THandle h)
			=$= sasl dn (retrieves dn samplePasswords)
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

makeMpi :: MonadBase IO m => Jid ->
	(XmlNode -> Bool) -> TChan BS.ByteString -> Pipe XmlNode Mpi m ()
makeMpi usr inr rids = (await >>=) . maybe (return ()) $ \n -> do
	e <- lift . liftBase . atomically $ isEmptyTChan rids
	if e
	then if inr n
		then do	uuid <- lift $ liftBase randomIO
			yield $ Iq (tagsType "get") {
				tagId = Just $ toASCIIBytes uuid,
				tagTo = Just usr
				} [n]
		else do uuid <- lift $ liftBase randomIO
			yield $ Message (tagsType "chat") {
			tagId = Just $ toASCIIBytes uuid,
			tagTo = Just usr
			} [n]
	else do	i <- lift . liftBase .atomically $ readTChan rids
		lift . liftBase . putStrLn $ "makeMpi: " ++ show i
		yield $ Iq (tagsType "return") {
			tagId = Just i,
			tagTo = Just usr
			} [n]
	makeMpi usr inr rids

setIds :: (HandleLike h, MonadBase IO (HandleMonad h)) => h ->
	(XmlNode -> Bool) -> TChan BS.ByteString -> Pipe Mpi Mpi (HandleMonad h) ()
setIds h ynr rids = (await >>=) . maybe (return ()) $ \mpi -> do
	yield mpi
	if boolXmlNode ynr mpi
	then when (isGetSet mpi) . lift . liftBase . atomically
		$ writeTChan rids (fromJust $ getId mpi)
	else lift $ returnEmpty h "hoge"
	lift . liftBase . putStrLn $ "\nsetIds: " ++ show (getId mpi)
	setIds h ynr rids

isGetSet :: Mpi -> Bool
isGetSet (Iq Tags { tagType = Just "set" } _) = True
isGetSet (Iq Tags { tagType = Just "get" } _) = True
isGetSet _ = False

getId :: Mpi -> Maybe BS.ByteString
getId (Iq t _) = tagId t
getId (Message t _) = tagId t
getId _ = Nothing

boolXmlNode :: (XmlNode -> Bool) -> Mpi -> Bool
boolXmlNode f (Iq _ [n]) = f n
boolXmlNode _ _ = False

returnEmpty :: (HandleLike h, MonadBase IO (HandleMonad h)) => h -> BS.ByteString -> HandleMonad h ()
returnEmpty h i = runPipe_ $ yield e =$= output =$= debug =$= toHandleLike h
	where
	you = toJid "hoge@hogehost"
	e = Iq (tagsType "result") { tagId = Just i, tagTo = Just you } []

initXSt :: BS.ByteString -> XSt
initXSt dn = XSt {
	user = Jid "" dn Nothing, rands = repeat "00DEADBEEF00",
	sSt = [	("realm", dn), ("qop", "auth"), ("charset", "utf-8"),
		("algorithm", "md5-sess") ] }

retrieves :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m) ) =>
	BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> [Retrieve m]
retrieves dn ps = [
	RTPlain $ retrievePln ps,
	RTDigestMd5 $ retrieveDM5 dn ps,
	RTScramSha1 $ retrieveSS1 ps ]

retrievePln :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m)) =>
	[(BS.ByteString, BS.ByteString)] ->
	BS.ByteString -> BS.ByteString -> BS.ByteString -> m ()
retrievePln ps "" usr pwd0
	| Just pwd <- lookup usr ps, pwd == pwd0 = return ()
retrievePln _ _ _ _ = throwError $ fromSaslError NotAuthorized "auth failure"

retrieveDM5 :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m) ) =>
	BS.ByteString -> [(BS.ByteString, BS.ByteString)] ->
	BS.ByteString -> m BS.ByteString
retrieveDM5 dn ps usr
	| Just pwd <- lookup usr ps = return $ DM5.mkStored usr dn pwd
retrieveDM5 _ _ _ = throwError $ fromSaslError NotAuthorized "auth failure"

retrieveSS1 :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m) ) =>
	[(BS.ByteString, BS.ByteString)] -> BS.ByteString ->
	m (BS.ByteString, BS.ByteString, BS.ByteString, Int)
retrieveSS1 ps usr | Just pwd <- lookup usr ps = let
	slt = "pepper"; i = 4492; (stk, svk) = SS1.salt pwd slt i in
	return (slt, stk, svk, i)
	{-
retrieveSS1 "yoshikuni" = return (slt, stk, svk, i)
	where slt = "pepper"; i = 4492; (stk, svk) = SS1.salt "password" slt i
retrieveSS1 "yoshio" = return (slt, stk, svk, i)
	where slt = "sugar"; i = 4492; (stk, svk) = SS1.salt "password" slt i
	-}
retrieveSS1 _ _ = throwError $ fromSaslError NotAuthorized "auth failure"

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
