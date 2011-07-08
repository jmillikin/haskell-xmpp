{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010-2011 John Millikin <jmillikin@gmail.com>
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
	
	, elementToStanza
	) where

import           Control.Monad (when)
import qualified Data.Text.Lazy as T
import qualified Network.Protocol.XMPP.XML as X
import           Network.Protocol.XMPP.JID (JID, parseJID, formatJID)

class Stanza a where
	stanzaTo        :: a -> Maybe JID
	stanzaFrom      :: a -> Maybe JID
	stanzaID        :: a -> Maybe T.Text
	stanzaLang      :: a -> Maybe T.Text
	stanzaPayloads  :: a -> [X.Element]
	stanzaToElement :: a -> X.Element

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
	, messagePayloads :: [X.Element]
	}
	deriving (Show)

instance Stanza Message where
	stanzaTo = messageTo
	stanzaFrom = messageFrom
	stanzaID = messageID
	stanzaLang = messageLang
	stanzaPayloads = messagePayloads
	stanzaToElement x = stanzaToElement' x "message" typeStr where
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
	, presencePayloads :: [X.Element]
	}
	deriving (Show)

instance Stanza Presence where
	stanzaTo = presenceTo
	stanzaFrom = presenceFrom
	stanzaID = presenceID
	stanzaLang = presenceLang
	stanzaPayloads = presencePayloads
	stanzaToElement x = stanzaToElement' x "presence" typeStr where
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
	, iqPayload :: Maybe X.Element
	}
	deriving (Show)

instance Stanza IQ where
	stanzaTo = iqTo
	stanzaFrom = iqFrom
	stanzaID = iqID
	stanzaLang = iqLang
	stanzaPayloads iq = case iqPayload iq of
		Just elemt -> [elemt]
		Nothing -> []
	stanzaToElement x = stanzaToElement' x "iq" typeStr where
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

stanzaToElement' :: Stanza a => a -> T.Text -> T.Text -> X.Element
stanzaToElement' stanza name typeStr = X.element name attrs payloads where
	payloads = map X.NodeElement $ stanzaPayloads stanza
	attrs = concat
		[ mattr "to" $ fmap formatJID . stanzaTo
		, mattr "from" $ fmap formatJID . stanzaFrom
		, mattr "id" stanzaID
		, mattr "xml:lang" stanzaLang
		, if T.null typeStr then [] else [("type", typeStr)]
		]
	mattr label f = case f stanza of
		Nothing -> []
		Just text -> [(label, text)]

elementToStanza :: T.Text -> X.Element -> Maybe ReceivedStanza
elementToStanza ns elemt = do
	let elemNS = X.nameNamespace . X.elementName $ elemt
	when (elemNS /= Just ns) Nothing
	
	let elemName = X.nameLocalName . X.elementName $ elemt
	case elemName of
		"message" -> ReceivedMessage `fmap` parseMessage elemt
		"presence" -> ReceivedPresence `fmap` parsePresence elemt
		"iq" -> ReceivedIQ `fmap` parseIQ elemt
		_ -> Nothing

parseMessage :: X.Element -> Maybe Message
parseMessage elemt = do
	typeStr <- X.getattr (X.name "type") elemt
	msgType <- case typeStr of
		"normal"    -> Just MessageNormal
		"chat"      -> Just MessageChat
		"groupchat" -> Just MessageGroupChat
		"headline"  -> Just MessageHeadline
		"error"     -> Just MessageError
		_           -> Nothing
	msgTo <- xmlJID "to" elemt
	msgFrom <- xmlJID "from" elemt
	let msgID = X.getattr (X.name "id") elemt
	let msgLang = X.getattr (X.name "lang") elemt
	let payloads = X.elementChildren elemt
	return $ Message msgType msgTo msgFrom msgID msgLang payloads

parsePresence :: X.Element -> Maybe Presence
parsePresence elemt = do
	let typeStr = maybe "" id $ X.getattr (X.name "type") elemt
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
		
	msgTo <- xmlJID "to" elemt
	msgFrom <- xmlJID "from" elemt
	let msgID = X.getattr (X.name "id") elemt
	let msgLang = X.getattr (X.name "lang") elemt
	let payloads = X.elementChildren elemt
	return $ Presence pType msgTo msgFrom msgID msgLang payloads

parseIQ :: X.Element -> Maybe IQ
parseIQ elemt = do
	typeStr <- X.getattr (X.name "type") elemt
	iqType <- case typeStr of
		"get"    -> Just IQGet
		"set"    -> Just IQSet
		"result" -> Just IQResult
		"error"  -> Just IQError
		_        -> Nothing
	
	msgTo <- xmlJID "to" elemt
	msgFrom <- xmlJID "from" elemt
	let msgID = X.getattr (X.name "id") elemt
	let msgLang = X.getattr (X.name "lang") elemt
	let payload = case X.elementChildren elemt of
		[] -> Nothing
		child:_ -> Just child
	return $ IQ iqType msgTo msgFrom msgID msgLang payload

xmlJID :: T.Text -> X.Element -> Maybe (Maybe JID)
xmlJID name elemt = case X.getattr (X.name name) elemt of
	Nothing -> Just Nothing
	Just raw -> case parseJID raw of
		Just jid -> Just (Just jid)
		Nothing -> Nothing
