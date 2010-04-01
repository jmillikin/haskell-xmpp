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
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Text.XML.HXT.Arrow ((>>>))
import qualified Text.XML.HXT.Arrow as A
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Network.Protocol.SASL.GNU as SASL

import Network.Protocol.XMPP.JID (JID, formatJID)
import Network.Protocol.XMPP.Internal.XML (element, qname)
import qualified Network.Protocol.XMPP.Internal.Stream as S

data Result = Success | Failure
	deriving (Show, Eq)

authenticate :: S.Stream stream => stream
             -> [B.ByteString] -- ^ Mechanisms
             -> JID -- ^ User JID
             -> JID -- ^ Server JID
             -> T.Text -- ^ Username
             -> T.Text -- ^ Password
             -> IO Result
authenticate stream mechanisms userJID serverJID username password = do
	let authz = formatJID userJID
	let hostname = formatJID serverJID
	let utf8 = TE.encodeUtf8
	
	SASL.runSASL $ do
		suggested <- SASL.clientSuggestMechanism $ map SASL.Mechanism mechanisms
		mechanism <- case suggested of
			Just m -> return m
			Nothing -> error "No supported SASL mechanisms advertised"
		let (SASL.Mechanism mechBytes) = mechanism
		SASL.runClient mechanism $ do
			SASL.setProperty SASL.PropertyAuthzID $ utf8 authz
			SASL.setProperty SASL.PropertyAuthID $ utf8 username
			SASL.setProperty SASL.PropertyPassword $ utf8 password
			SASL.setProperty SASL.PropertyService $ B.pack "xmpp"
			SASL.setProperty SASL.PropertyHostname $ utf8 hostname
			
			(b64text, rc) <- SASL.step64 $ B.pack ""
			liftIO $ S.putTree stream $ element ("", "auth")
				[ ("", "xmlns", "urn:ietf:params:xml:ns:xmpp-sasl")
				, ("", "mechanism", B.unpack mechBytes)]
				[XN.mkText $ B.unpack b64text]
			
			case rc of
				SASL.Complete -> liftIO $ saslFinish stream
				SASL.NeedsMore -> saslLoop stream

saslLoop :: S.Stream s => s -> SASL.Session Result
saslLoop stream = do
	challengeText <- liftIO $ A.runX (
		A.arrIO (\_ -> S.getTree stream)
		>>> A.getChildren
		>>> A.hasQName (qname "urn:ietf:params:xml:ns:xmpp-sasl" "challenge")
		>>> A.getChildren >>> A.getText)
	
	if null challengeText
		then return Failure
		else do
			(b64text, rc) <- SASL.step64 $ B.pack $ concat challengeText
			liftIO $ S.putTree stream $ element ("", "response")
				[("", "xmlns", "urn:ietf:params:xml:ns:xmpp-sasl")]
				[XN.mkText $ B.unpack b64text]
			case rc of
				SASL.Complete -> liftIO $ saslFinish stream
				SASL.NeedsMore -> saslLoop stream

saslFinish :: S.Stream s => s -> IO Result
saslFinish stream = do
	successElem <- A.runX (
		A.arrIO (\_ -> S.getTree stream)
		>>> A.getChildren
		>>> A.hasQName (qname "urn:ietf:params:xml:ns:xmpp-sasl" "success"))
	
	return $ if null successElem then Failure else Success
