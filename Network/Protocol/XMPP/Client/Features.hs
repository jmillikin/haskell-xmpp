-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
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

module Network.Protocol.XMPP.Client.Features
	( Feature (..)
	, parseFeatures
	, parseFeature
	) where
import qualified Data.ByteString.Char8 as B
import Text.XML.HXT.Arrow ((>>>), (&&&))
import qualified Text.XML.HXT.Arrow as A
import qualified Text.XML.HXT.DOM.Interface as DOM
import qualified Text.XML.HXT.DOM.XmlNode as XN
import Network.Protocol.XMPP.XML (qname)

data Feature =
	  FeatureStartTLS Bool
	| FeatureSASL [B.ByteString]
	| FeatureRegister
	| FeatureBind
	| FeatureSession
	| FeatureUnknown DOM.XmlTree
	deriving (Show, Eq)

parseFeatures :: DOM.XmlTree -> [Feature]
parseFeatures = A.runLA $
	A.getChildren
	>>> A.hasQName qnameFeatures
	>>> A.getChildren
	>>> A.arrL (\t' -> [parseFeature t'])

parseFeature :: DOM.XmlTree -> Feature
parseFeature t = feature where
	mkPair = maybe ("", "") $ DOM.namespaceUri &&& DOM.localPart
	feature = case mkPair (XN.getName t) of
		("urn:ietf:params:xml:ns:xmpp-tls", "starttls") -> parseFeatureTLS t
		("urn:ietf:params:xml:ns:xmpp-sasl", "mechanisms") -> parseFeatureSASL t
		("http://jabber.org/features/iq-register", "register") -> FeatureRegister
		("urn:ietf:params:xml:ns:xmpp-bind", "bind") -> FeatureBind
		("urn:ietf:params:xml:ns:xmpp-session", "session") -> FeatureSession
		_ -> FeatureUnknown t

parseFeatureTLS :: DOM.XmlTree -> Feature
parseFeatureTLS t = FeatureStartTLS True -- TODO: detect whether or not required

parseFeatureSASL :: DOM.XmlTree -> Feature
parseFeatureSASL = FeatureSASL . A.runLA (
	A.getChildren
	>>> A.hasQName qnameMechanism
	>>> A.getChildren
	>>> A.getText
	>>> A.arr B.pack)

qnameMechanism :: DOM.QName
qnameMechanism = qname "urn:ietf:params:xml:ns:xmpp-sasl" "mechanism"

qnameFeatures :: DOM.QName
qnameFeatures = qname "http://etherx.jabber.org/streams" "features"
