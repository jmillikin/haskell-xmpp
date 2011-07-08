{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- Copyright (C) 2009-2011 John Millikin <jmillikin@gmail.com>
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Network.Protocol.XMPP.Client.Authentication
	( Result (..)
	, authenticate
	) where

import qualified Control.Exception as Exc
import           Control.Monad (when, (>=>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Error as E
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import           Data.Typeable (Typeable)
import qualified Network.Protocol.SASL.GNU as SASL

import qualified Network.Protocol.XMPP.Monad as M
import qualified Network.Protocol.XMPP.XML as X
import           Network.Protocol.XMPP.JID (JID, formatJID, jidResource)

data Result = Success | Failure
	deriving (Show, Eq)

data AuthException = XmppError M.Error | SaslError TL.Text
	deriving (Typeable, Show)

instance Exc.Exception AuthException

authenticate :: [B.ByteString] -- ^ Mechanisms
             -> JID -- ^ User JID
             -> JID -- ^ Server JID
             -> TL.Text -- ^ Username
             -> TL.Text -- ^ Password
             -> M.XMPP ()
authenticate xmppMechanisms userJID serverJID username password = xmpp where
	mechanisms = map SASL.Mechanism xmppMechanisms
	authz = formatJID $ userJID { jidResource = Nothing }
	hostname = formatJID serverJID
	utf8 = TE.encodeUtf8 . T.concat . TL.toChunks
	
	xmpp = do
		ctx <- M.getSession
		res <- liftIO $ Exc.try $ SASL.runSASL $ do
			suggested <- SASL.clientSuggestMechanism mechanisms
			case suggested of
				Nothing -> saslError "No supported authentication mechanism"
				Just mechanism -> authSasl ctx mechanism
		case res of
			Right Success -> return ()
			Right Failure -> E.throwError M.AuthenticationFailure
			Left (XmppError err) -> E.throwError err
			Left (SaslError err) -> E.throwError $ M.AuthenticationError err
	
	authSasl ctx mechanism = do
		let (SASL.Mechanism mechBytes) = mechanism
		sessionResult <- SASL.runClient mechanism $ do
			SASL.setProperty SASL.PropertyAuthzID $ utf8 authz
			SASL.setProperty SASL.PropertyAuthID $ utf8 username
			SASL.setProperty SASL.PropertyPassword $ utf8 password
			SASL.setProperty SASL.PropertyService $ B.pack "xmpp"
			SASL.setProperty SASL.PropertyHostname $ utf8 hostname
			
			(b64text, rc) <- SASL.step64 $ B.pack ""
			putElement ctx $ X.nselement "urn:ietf:params:xml:ns:xmpp-sasl" "auth"
				[("mechanism", TL.pack $ B.unpack mechBytes)]
				[X.NodeContent $ X.ContentText $ T.pack $ B.unpack b64text]
			
			case rc of
				SASL.Complete -> saslFinish ctx
				SASL.NeedsMore -> saslLoop ctx
			
		case sessionResult of
			Right x -> return x
			Left err -> saslError $ TL.pack $ show err

saslLoop :: M.Session -> SASL.Session Result
saslLoop ctx = do
	elemt <- getElement ctx
	let name = X.Name "challenge" (Just "urn:ietf:params:xml:ns:xmpp-sasl") Nothing
	let getChallengeText =
		X.isNamed name
		>=> X.elementNodes
		>=> X.isContent
		>=> return . X.contentText
	let challengeText = getChallengeText elemt
	when (null challengeText) $ saslError "Received empty challenge"
	
	(b64text, rc) <- SASL.step64 . B.pack . concatMap TL.unpack $ challengeText
	putElement ctx $ X.nselement "urn:ietf:params:xml:ns:xmpp-sasl" "response"
		[] [X.NodeContent $ X.ContentText $ T.pack $ B.unpack b64text]
	case rc of
		SASL.Complete -> saslFinish ctx
		SASL.NeedsMore -> saslLoop ctx

saslFinish :: M.Session -> SASL.Session Result
saslFinish ctx = do
	elemt <- getElement ctx
	let name = X.Name "success" (Just "urn:ietf:params:xml:ns:xmpp-sasl") Nothing
	let success = X.isNamed name elemt
	return $ if null success then Failure else Success

putElement :: M.Session -> X.Element -> SASL.Session ()
putElement ctx elemt = liftIO $ do
	res <- M.runXMPP ctx $ M.putElement elemt
	case res of
		Left err -> Exc.throwIO $ XmppError err
		Right x -> return x

getElement :: M.Session -> SASL.Session X.Element
getElement ctx = liftIO $ do
	res <- M.runXMPP ctx M.getElement
	case res of
		Left err -> Exc.throwIO $ XmppError err
		Right x -> return x

saslError :: MonadIO m => TL.Text -> m a
saslError = liftIO . Exc.throwIO . SaslError
