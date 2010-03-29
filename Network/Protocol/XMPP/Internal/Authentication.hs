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

module Network.Protocol.XMPP.Internal.Authentication
	( Result(..)
	, authenticate
	) where
import qualified Data.Text as T

import Text.XML.HXT.Arrow ((>>>))
import qualified Text.XML.HXT.Arrow as A
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Network.Protocol.SASL.GSASL as G

import Network.Protocol.XMPP.JID (JID, formatJID)
import Network.Protocol.XMPP.Internal.XML (element, qname)
import qualified Network.Protocol.XMPP.Internal.Stream as S

data Result = Success | Failure
	deriving (Show, Eq)

authenticate :: S.Stream stream => stream
             -> [T.Text] -- ^ Mechanisms
             -> JID -- ^ User JID
             -> JID -- ^ Server JID
             -> T.Text -- ^ Username
             -> T.Text -- ^ Password
             -> IO Result
authenticate stream mechanisms userJID serverJID username password = do
	let authz = formatJID userJID
	let hostname = formatJID serverJID
	
	G.withContext $ \ctxt -> do
	
	suggested <- G.clientSuggestMechanism ctxt (map T.unpack mechanisms)
	mechanism <- case suggested of
		Just m -> return m
		Nothing -> error "No supported SASL mechanisms advertised"
	
	G.withSession (G.clientStart ctxt mechanism) $ \s -> do
	
	G.propertySet s G.GSASL_AUTHZID $ T.unpack authz
	G.propertySet s G.GSASL_AUTHID $ T.unpack username
	G.propertySet s G.GSASL_PASSWORD $ T.unpack password
	G.propertySet s G.GSASL_SERVICE "xmpp"
	G.propertySet s G.GSASL_HOSTNAME $ T.unpack hostname
	
	(b64text, rc) <- G.step64 s ""
	S.putTree stream $ element ("", "auth")
		[ ("", "xmlns", "urn:ietf:params:xml:ns:xmpp-sasl")
		 ,("", "mechanism", mechanism)]
		[XN.mkText b64text]
	
	case rc of
		G.GSASL_OK -> saslFinish stream
		G.GSASL_NEEDS_MORE -> saslLoop stream s
		_ -> error "Unknown GNU SASL response"

saslLoop :: S.Stream s => s -> G.Session -> IO Result
saslLoop stream session = do
	challengeText <- A.runX (
		A.arrIO (\_ -> S.getTree stream)
		>>> A.getChildren
		>>> A.hasQName (qname "urn:ietf:params:xml:ns:xmpp-sasl" "challenge")
		>>> A.getChildren >>> A.getText)
	
	if null challengeText then return Failure
		else do
			(b64text, rc) <- G.step64 session (concat challengeText)
			S.putTree stream $ element ("", "response")
				[("", "xmlns", "urn:ietf:params:xml:ns:xmpp-sasl")]
				[XN.mkText b64text]
			case rc of
				G.GSASL_OK -> saslFinish stream
				G.GSASL_NEEDS_MORE -> saslLoop stream session
				_ -> error "Unknown GNU SASL response"

saslFinish :: S.Stream s => s -> IO Result
saslFinish stream = do
	successElem <- A.runX (
		A.arrIO (\_ -> S.getTree stream)
		>>> A.getChildren
		>>> A.hasQName (qname "urn:ietf:params:xml:ns:xmpp-sasl" "success"))
	
	return $ if null successElem then Failure else Success