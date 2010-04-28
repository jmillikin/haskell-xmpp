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

{-# LANGUAGE OverloadedStrings #-}
module Network.Protocol.XMPP.Client.Features
	( Feature (..)
	, parseFeatures
	, parseFeature
	) where
import Control.Arrow ((&&&))
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Lazy as TL
import qualified Network.Protocol.XMPP.XML as X

data Feature =
	  FeatureStartTLS Bool
	| FeatureSASL [B.ByteString]
	| FeatureRegister
	| FeatureBind
	| FeatureSession
	| FeatureUnknown X.Element
	deriving (Show, Eq)

parseFeatures :: X.Element -> [Feature]
parseFeatures elemt =
	X.hasName nameFeatures elemt
	>>= X.elementChildren
	>>= return . parseFeature

parseFeature :: X.Element -> Feature
parseFeature elemt = feature where
	unpackName = (maybe "" id . X.nameNamespace) &&& X.nameLocalName
	feature = case unpackName (X.elementName elemt) of
		("urn:ietf:params:xml:ns:xmpp-tls", "starttls") -> parseFeatureTLS elemt
		("urn:ietf:params:xml:ns:xmpp-sasl", "mechanisms") -> parseFeatureSASL elemt
		("http://jabber.org/features/iq-register", "register") -> FeatureRegister
		("urn:ietf:params:xml:ns:xmpp-bind", "bind") -> FeatureBind
		("urn:ietf:params:xml:ns:xmpp-session", "session") -> FeatureSession
		_ -> FeatureUnknown elemt

parseFeatureTLS :: X.Element -> Feature
parseFeatureTLS _ = FeatureStartTLS True -- TODO: detect whether or not required

parseFeatureSASL :: X.Element -> Feature
parseFeatureSASL e = FeatureSASL $
	X.elementChildren e
	>>= X.hasName nameMechanism
	>>= X.elementNodes
	>>= X.getText
	>>= return . B.pack . TL.unpack

nameMechanism :: X.Name
nameMechanism = X.Name "mechanism" (Just "urn:ietf:params:xml:ns:xmpp-sasl") Nothing

nameFeatures :: X.Name
nameFeatures = X.Name "features" (Just "http://etherx.jabber.org/streams") Nothing
