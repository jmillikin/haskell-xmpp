-- Copyright (C) 2009-2010 John Millikin <jmillikin@gmail.com>
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Network.Protocol.XMPP.Client.Authentication
	( Result (..)
	, authenticate
	) where
import qualified Control.Exception as Exc
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Error as E
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable (Typeable)

import Text.XML.HXT.Arrow ((>>>))
import qualified Text.XML.HXT.Arrow as A
import qualified Text.XML.HXT.DOM.XmlNode as XN
import Text.XML.HXT.DOM.Interface (XmlTree)
import qualified Network.Protocol.SASL.GNU as SASL

import qualified Network.Protocol.XMPP.Monad as M
import Network.Protocol.XMPP.JID (JID, formatJID)
import Network.Protocol.XMPP.XML (element, qname)

data Result = Success | Failure
	deriving (Show, Eq)

data AuthException = XmppError M.Error | SaslError T.Text
	deriving (Typeable, Show)

instance Exc.Exception AuthException

authenticate :: [B.ByteString] -- ^ Mechanisms
             -> JID -- ^ User JID
             -> JID -- ^ Server JID
             -> T.Text -- ^ Username
             -> T.Text -- ^ Password
             -> M.XMPP ()
authenticate xmppMechanisms userJID serverJID username password = xmpp where
	mechanisms = map SASL.Mechanism xmppMechanisms
	authz = formatJID userJID
	hostname = formatJID serverJID
	utf8 = TE.encodeUtf8
	
	xmpp = do
		ctx <- M.getContext
		res <- liftIO $ Exc.try $ SASL.runSASL $ do
			suggested <- SASL.clientSuggestMechanism mechanisms
			case suggested of
				Nothing -> saslError "No supported authentication mechanism"
				Just mechanism -> authSasl ctx mechanism
		case res of
			Right Success -> return ()
			Right Failure -> E.throwError $ M.AuthenticationFailure
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
			putTree ctx $ element ("", "auth")
				[ ("", "xmlns", "urn:ietf:params:xml:ns:xmpp-sasl")
				, ("", "mechanism", B.unpack mechBytes)]
				[XN.mkText $ B.unpack b64text]
			
			case rc of
				SASL.Complete -> saslFinish ctx
				SASL.NeedsMore -> saslLoop ctx
			
		case sessionResult of
			Right x -> return x
			Left err -> saslError $ T.pack $ show err

saslLoop :: M.Context -> SASL.Session Result
saslLoop ctx = do
	challengeText <- liftIO $ A.runX (
		A.arrIO (\_ -> getTree ctx)
		>>> A.getChildren
		>>> A.hasQName (qname "urn:ietf:params:xml:ns:xmpp-sasl" "challenge")
		>>> A.getChildren >>> A.getText)
	when (null challengeText) $ saslError "Received empty challenge"
	
	(b64text, rc) <- SASL.step64 $ B.pack $ concat challengeText
	putTree ctx $ element ("", "response")
		[("", "xmlns", "urn:ietf:params:xml:ns:xmpp-sasl")]
		[XN.mkText $ B.unpack b64text]
	case rc of
		SASL.Complete -> saslFinish ctx
		SASL.NeedsMore -> saslLoop ctx

saslFinish :: M.Context -> SASL.Session Result
saslFinish ctx = liftIO $ do
	successElem <- A.runX (
		A.arrIO (\_ -> getTree ctx)
		>>> A.getChildren
		>>> A.hasQName (qname "urn:ietf:params:xml:ns:xmpp-sasl" "success"))
	
	return $ if null successElem then Failure else Success

putTree :: M.Context -> XmlTree -> SASL.Session ()
putTree ctx tree = liftIO $ do
	res <- M.runXMPP ctx $ M.putTree tree
	case res of
		Left err -> Exc.throwIO $ XmppError err
		Right x -> return x

getTree :: M.Context -> IO XmlTree
getTree ctx = do
	res <- M.runXMPP ctx $ M.getTree
	case res of
		Left err -> Exc.throwIO $ XmppError err
		Right x -> return x

saslError :: MonadIO m => T.Text -> m a
saslError = liftIO . Exc.throwIO . SaslError
