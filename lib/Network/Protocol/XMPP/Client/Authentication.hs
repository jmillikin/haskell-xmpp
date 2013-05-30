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
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Error as E
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8
import qualified Data.Text
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Typeable (Typeable)
import qualified Network.Protocol.SASL.GNU as SASL

import qualified Network.Protocol.XMPP.Monad as M
import qualified Network.Protocol.XMPP.XML as X
import           Network.Protocol.XMPP.JID (JID, formatJID, jidResource)

data Result = Success | Failure X.Element
	deriving (Show, Eq)

data AuthException = XmppError M.Error | SaslError Text
	deriving (Typeable, Show)

instance Exc.Exception AuthException

authenticate :: [ByteString] -- ^ Mechanisms
             -> JID -- ^ User JID
             -> JID -- ^ Server JID
             -> Text -- ^ Username
             -> Text -- ^ Password
             -> M.XMPP ()
authenticate xmppMechanisms userJID serverJID username password = xmpp where
	mechanisms = map SASL.Mechanism xmppMechanisms
	authz = formatJID (userJID { jidResource = Nothing })
	hostname = formatJID serverJID
	
	xmpp = do
		ctx <- M.getSession
		res <- liftIO . Exc.try . SASL.runSASL $ do
			suggested <- SASL.clientSuggestMechanism mechanisms
			case suggested of
				Nothing -> saslError "No supported authentication mechanism"
				Just mechanism -> authSasl ctx mechanism
		case res of
			Right Success -> return ()
			Right (Failure e) -> E.throwError (M.AuthenticationFailure e)
			Left (XmppError err) -> E.throwError err
			Left (SaslError err) -> E.throwError (M.AuthenticationError err)
	
	authSasl ctx mechanism = do
		let (SASL.Mechanism mechBytes) = mechanism
		sessionResult <- SASL.runClient mechanism $ do
			SASL.setProperty SASL.PropertyAuthzID (encodeUtf8 authz)
			SASL.setProperty SASL.PropertyAuthID (encodeUtf8 username)
			SASL.setProperty SASL.PropertyPassword (encodeUtf8 password)
			SASL.setProperty SASL.PropertyService "xmpp"
			SASL.setProperty SASL.PropertyHostname (encodeUtf8 hostname)
			
			(b64text, rc) <- SASL.step64 ""
			putElement ctx $ X.element "{urn:ietf:params:xml:ns:xmpp-sasl}auth"
				[("mechanism", Data.Text.pack (Data.ByteString.Char8.unpack mechBytes))]
				[X.NodeContent (X.ContentText (Data.Text.pack (Data.ByteString.Char8.unpack b64text)))]
			
			case rc of
				SASL.Complete -> saslFinish ctx
				SASL.NeedsMore -> saslLoop ctx
			
		case sessionResult of
			Right x -> return x
			Left err -> saslError (show err)

saslLoop :: M.Session -> SASL.Session Result
saslLoop ctx = do
	e <- getElement ctx
	let challengeTexts = X.elementNodes e >>= X.isContent >>= return . X.contentText
	let challenge = concatMap Data.Text.unpack challengeTexts
	case X.elementName e of
		-- The server needs more data before it can authenticate this client.
		n | n == "{urn:ietf:params:xml:ns:xmpp-sasl}challenge" -> do
			when (null challenge) (saslError "Received empty challenge")
			(b64text, rc) <- SASL.step64 (Data.ByteString.Char8.pack challenge)
			putElement ctx (X.element
				"{urn:ietf:params:xml:ns:xmpp-sasl}response"
				[]
				[X.NodeContent (X.ContentText (Data.Text.pack (Data.ByteString.Char8.unpack b64text)))])
			case rc of
				SASL.Complete -> saslFinish ctx
				SASL.NeedsMore -> saslLoop ctx
		
		-- The server has authenticated this client, but the client-side
		-- SASL protocol wants more data from the server.
		n | n == "{urn:ietf:params:xml:ns:xmpp-sasl}success" -> do
			when (null challenge) (saslError "Received empty challenge")
			(_, rc) <- SASL.step64 (Data.ByteString.Char8.pack challenge)
			case rc of
				SASL.Complete -> return Success
				SASL.NeedsMore -> saslError "Server didn't provide enough SASL data."
		
		-- The server has rejected this client's credentials.
		n | n == "{urn:ietf:params:xml:ns:xmpp-sasl}failure" -> return (Failure e)
		
		_ -> saslError ("Server sent unexpected element during authentication.")

saslFinish :: M.Session -> SASL.Session Result
saslFinish ctx = do
	elemt <- getElement ctx
	return $ if X.elementName elemt == "{urn:ietf:params:xml:ns:xmpp-sasl}success"
		then Success
		else Failure elemt

putElement :: M.Session -> X.Element -> SASL.Session ()
putElement ctx elemt = liftIO $ do
	res <- M.runXMPP ctx (M.putElement elemt)
	case res of
		Left err -> Exc.throwIO (XmppError err)
		Right x -> return x

getElement :: M.Session -> SASL.Session X.Element
getElement ctx = liftIO $ do
	res <- M.runXMPP ctx M.getElement
	case res of
		Left err -> Exc.throwIO (XmppError err)
		Right x -> return x

saslError :: MonadIO m => String -> m a
saslError = liftIO . Exc.throwIO . SaslError . Data.Text.pack
