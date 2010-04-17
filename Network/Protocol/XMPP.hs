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


module Network.Protocol.XMPP
	( 
	-- * JIDs
	  JID (..)
	, Node
	, Domain
	, Resource
	
	, strNode
	, strDomain
	, strResource
	
	, parseJID
	, formatJID
	
	-- * Stanzas
	, Stanza
		( stanzaTo
		, stanzaFrom
		, stanzaID
		, stanzaLang
		, stanzaPayloads
		)
	
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
	
	-- * The XMPP monad
	, Server (..)
	, XMPP
	, runClient
	, runComponent
	, putStanza
	, getStanza
	, bindJID
	) where
import Network.Protocol.XMPP.Client
import Network.Protocol.XMPP.Component
import Network.Protocol.XMPP.Connections
import Network.Protocol.XMPP.JID
import Network.Protocol.XMPP.Monad
import Network.Protocol.XMPP.Stanza
