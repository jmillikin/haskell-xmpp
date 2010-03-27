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

module Network.Protocol.XMPP.Internal.Stanza where
import Network.Protocol.XMPP.JID (JID)
import qualified Data.Text as T
import Text.XML.HXT.DOM.Interface (XmlTree)

class Stanza a where
	stanzaTo       :: a -> Maybe JID
	stanzaFrom     :: a -> Maybe JID
	stanzaID       :: a -> Maybe T.Text
	stanzaLang     :: a -> Maybe T.Text
	stanzaPayloads :: a -> [XmlTree]
	stanzaTree     :: a -> XmlTree

data ReceivedStanza
	= ReceivedMessage Message
	| ReceivedPresence Presence
	| ReceivedIQ IQ

data Message = Message
	{ messageType     :: MessageType
	, messageTo       :: Maybe JID
	, messageFrom     :: Maybe JID
	, messageID       :: Maybe T.Text
	, messageLang     :: Maybe T.Text
	, messagePayloads :: [XmlTree]
	}

instance Stanza Message where
	stanzaTo = messageTo
	stanzaFrom = messageFrom
	stanzaID = messageID
	stanzaLang = messageLang
	stanzaPayloads = messagePayloads
	stanzaTree = undefined

data MessageType
	= MessageNormal
	| MessageCHat
	| MessageGroupChat
	| MessageHeadline
	| MessageError
	deriving (Show, Eq)

data Presence = Presence
	{ presenceType     :: PresenceType
	, presenceTo       :: Maybe JID
	, presenceFrom     :: Maybe JID
	, presenceID       :: Maybe T.Text
	, presenceLang     :: Maybe T.Text
	, presencePayloads :: [XmlTree]
	}

instance Stanza Presence where
	stanzaTo = presenceTo
	stanzaFrom = presenceFrom
	stanzaID = presenceID
	stanzaLang = presenceLang
	stanzaPayloads = presencePayloads
	stanzaTree = undefined

data PresenceType
	= PresenceUnavailable
	| PresenceSubscribe
	| PresenceSubscribed
	| PresenceUnsubscribe
	| PresenceUnsubscribed
	| PresenceProbe
	| PresenceError
	deriving (Show, Eq)

data IQ = IQ
	{ iqType    :: IQType
	, iqTo      :: Maybe JID
	, iqFrom    :: Maybe JID
	, iqID      :: Maybe T.Text
	, iqLang    :: Maybe T.Text
	, iqPayload :: XmlTree
	}

instance Stanza IQ where
	stanzaTo = iqTo
	stanzaFrom = iqFrom
	stanzaID = iqID
	stanzaLang = iqLang
	stanzaPayloads iq = [iqPayload iq]
	stanzaTree = undefined

data IQType
	= IQGet
	| IQSet
	| IQResult
	| IQError
	deriving (Show, Eq)
