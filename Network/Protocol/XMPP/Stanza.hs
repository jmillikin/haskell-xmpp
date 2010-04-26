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

module Network.Protocol.XMPP.Stanza
	( Stanza (..)
	
	, ReceivedStanza (..)
	, Message (..)
	, Presence (..)
	, IQ (..)
	, MessageType (..)
	, PresenceType (..)
	, IQType (..)
	
	, emptyMessage
	, emptyPresence
	, emptyIQ
	
	, treeToStanza
	) where

import qualified Data.Text as T
import Text.XML.HXT.DOM.Interface (XmlTree)
import Text.XML.HXT.Arrow ((>>>))
import qualified Text.XML.HXT.Arrow as A

import Network.Protocol.XMPP.XML (element)
import Network.Protocol.XMPP.JID (JID, parseJID, formatJID)

class Stanza a where
	stanzaTo       :: a -> Maybe JID
	stanzaFrom     :: a -> Maybe JID
	stanzaID       :: a -> Maybe T.Text
	stanzaLang     :: a -> Maybe T.Text
	stanzaPayloads :: a -> [XmlTree]
	stanzaToTree   :: a -> XmlTree

data ReceivedStanza
	= ReceivedMessage Message
	| ReceivedPresence Presence
	| ReceivedIQ IQ
	deriving (Show)

data Message = Message
	{ messageType     :: MessageType
	, messageTo       :: Maybe JID
	, messageFrom     :: Maybe JID
	, messageID       :: Maybe T.Text
	, messageLang     :: Maybe T.Text
	, messagePayloads :: [XmlTree]
	}
	deriving (Show)

instance Stanza Message where
	stanzaTo = messageTo
	stanzaFrom = messageFrom
	stanzaID = messageID
	stanzaLang = messageLang
	stanzaPayloads = messagePayloads
	stanzaToTree x = stanzaToTree' x "message" typeStr where
		typeStr = case messageType x of
			MessageNormal -> "normal"
			MessageChat -> "chat"
			MessageGroupChat -> "groupchat"
			MessageHeadline -> "headline"
			MessageError -> "error"

data MessageType
	= MessageNormal
	| MessageChat
	| MessageGroupChat
	| MessageHeadline
	| MessageError
	deriving (Show, Eq)

emptyMessage :: MessageType -> Message
emptyMessage t = Message
	{ messageType = t
	, messageTo = Nothing
	, messageFrom = Nothing
	, messageID = Nothing
	, messageLang = Nothing
	, messagePayloads = []
	}

data Presence = Presence
	{ presenceType     :: PresenceType
	, presenceTo       :: Maybe JID
	, presenceFrom     :: Maybe JID
	, presenceID       :: Maybe T.Text
	, presenceLang     :: Maybe T.Text
	, presencePayloads :: [XmlTree]
	}
	deriving (Show)

instance Stanza Presence where
	stanzaTo = presenceTo
	stanzaFrom = presenceFrom
	stanzaID = presenceID
	stanzaLang = presenceLang
	stanzaPayloads = presencePayloads
	stanzaToTree x = stanzaToTree' x "presence" typeStr where
		typeStr = case presenceType x of
			PresenceAvailable -> ""
			PresenceUnavailable -> "unavailable"
			PresenceSubscribe -> "subscribe"
			PresenceSubscribed -> "subscribed"
			PresenceUnsubscribe -> "unsubscribe"
			PresenceUnsubscribed -> "unsubscribed"
			PresenceProbe -> "probe"
			PresenceError -> "error"

data PresenceType
	= PresenceAvailable
	| PresenceUnavailable
	| PresenceSubscribe
	| PresenceSubscribed
	| PresenceUnsubscribe
	| PresenceUnsubscribed
	| PresenceProbe
	| PresenceError
	deriving (Show, Eq)

emptyPresence :: PresenceType -> Presence
emptyPresence t = Presence
	{ presenceType = t
	, presenceTo = Nothing
	, presenceFrom = Nothing
	, presenceID = Nothing
	, presenceLang = Nothing
	, presencePayloads = []
	}

data IQ = IQ
	{ iqType    :: IQType
	, iqTo      :: Maybe JID
	, iqFrom    :: Maybe JID
	, iqID      :: Maybe T.Text
	, iqLang    :: Maybe T.Text
	, iqPayload :: Maybe XmlTree
	}
	deriving (Show)

instance Stanza IQ where
	stanzaTo = iqTo
	stanzaFrom = iqFrom
	stanzaID = iqID
	stanzaLang = iqLang
	stanzaPayloads iq = case iqPayload iq of
		Just tree -> [tree]
		Nothing -> []
	stanzaToTree x = stanzaToTree' x "iq" typeStr where
		typeStr = case iqType x of
			IQGet -> "get"
			IQSet -> "set"
			IQResult -> "result"
			IQError -> "error"

data IQType
	= IQGet
	| IQSet
	| IQResult
	| IQError
	deriving (Show, Eq)

emptyIQ :: IQType -> IQ
emptyIQ t = IQ
	{ iqType = t
	, iqTo = Nothing
	, iqFrom = Nothing
	, iqID = Nothing
	, iqLang = Nothing
	, iqPayload = Nothing
	}

stanzaToTree' :: Stanza a => a -> String -> String -> XmlTree
stanzaToTree' stanza name typeStr = element ("", name) attrs payloads where
	payloads = stanzaPayloads stanza
	attrs = concat
		[ mattr "to" $ fmap formatJID . stanzaTo
		, mattr "from" $ fmap formatJID . stanzaFrom
		, mattr "id" stanzaID
		, mattr "xml:lang" stanzaLang
		, if null typeStr then [] else [("", "type", typeStr)]
		]
	mattr label f = case f stanza of
		Nothing -> []
		Just text -> [("", label, T.unpack text)]

treeToStanza :: T.Text -> XmlTree -> Maybe ReceivedStanza
treeToStanza ns root = do
	tree <- runMA (A.getChildren >>> A.isElem) root
	treeNS <- runMA A.getNamespaceUri tree
	if T.pack treeNS == ns then Just () else Nothing
	
	treeName <- runMA A.getLocalPart tree
	case treeName of
		"message" -> ReceivedMessage `fmap` parseMessage tree
		"presence" -> ReceivedPresence `fmap` parsePresence tree
		"iq" -> ReceivedIQ `fmap` parseIQ tree
		_ -> Nothing

parseMessage :: XmlTree -> Maybe Message
parseMessage t = do
	typeStr <- runMA (A.getAttrValue "type") t
	msgType <- case typeStr of
		"normal"    -> Just MessageNormal
		"chat"      -> Just MessageChat
		"groupchat" -> Just MessageGroupChat
		"headline"  -> Just MessageHeadline
		"error"     -> Just MessageError
		_           -> Nothing
	msgTo <- xmlJID "to" t
	msgFrom <- xmlJID "from" t
	let msgID = T.pack `fmap` runMA (A.getAttrValue "id") t
	let msgLang = T.pack `fmap` runMA (A.getAttrValue "lang") t
	let payloads = A.runLA (A.getChildren >>> A.isElem) t
	return $ Message msgType msgTo msgFrom msgID msgLang payloads

parsePresence :: XmlTree -> Maybe Presence
parsePresence t = do
	let typeStr = maybe "" id $ runMA (A.getAttrValue "type") t
	pType <- case typeStr of
		""             -> Just PresenceAvailable
		"unavailable"  -> Just PresenceUnavailable
		"subscribe"    -> Just PresenceSubscribe
		"subscribed"   -> Just PresenceSubscribed
		"unsubscribe"  -> Just PresenceUnsubscribe
		"unsubscribed" -> Just PresenceUnsubscribed
		"probe"        -> Just PresenceProbe
		"error"        -> Just PresenceError
		_              -> Nothing
		
	msgTo <- xmlJID "to" t
	msgFrom <- xmlJID "from" t
	let msgID = T.pack `fmap` runMA (A.getAttrValue "id") t
	let msgLang = T.pack `fmap` runMA (A.getAttrValue "lang") t
	let payloads = A.runLA (A.getChildren >>> A.isElem) t
	return $ Presence pType msgTo msgFrom msgID msgLang payloads

parseIQ :: XmlTree -> Maybe IQ
parseIQ t = do
	typeStr <- runMA (A.getAttrValue "type") t
	iqType <- case typeStr of
		"get"    -> Just IQGet
		"set"    -> Just IQSet
		"result" -> Just IQResult
		"error"  -> Just IQError
		_        -> Nothing
	
	msgTo <- xmlJID "to" t
	msgFrom <- xmlJID "from" t
	let msgID = T.pack `fmap` runMA (A.getAttrValue "id") t
	let msgLang = T.pack `fmap` runMA (A.getAttrValue "lang") t
	let payload = runMA (A.getChildren >>> A.isElem) t
	return $ IQ iqType msgTo msgFrom msgID msgLang payload

xmlJID :: String -> XmlTree -> Maybe (Maybe JID)
xmlJID attr t = case runMA (A.getAttrValue attr) t of
	Nothing -> Just Nothing
	Just raw -> case parseJID (T.pack raw) of
		Just jid -> Just (Just jid)
		Nothing -> Nothing

runMA :: A.LA a b -> a -> Maybe b
runMA arr x = case A.runLA arr x of
	[] -> Nothing
	(y:_) -> Just y
