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
module Network.Protocol.XMPP.Connections
	( Server (..)
	, xmlHeader
	, startOfStream
	, qnameStream
	) where
import Network (HostName, PortID)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Text.XML.HXT.DOM.Interface as DOM
import qualified Text.XML.LibXML.SAX as SAX

import Network.Protocol.XMPP.JID (JID, formatJID)
import Network.Protocol.XMPP.XML (qname, convertQName)

data Server = Server
	{ serverJID      :: JID
	, serverHostname :: HostName
	, serverPort     :: PortID
	}

-- Since only the opening tag should be written, normal XML
-- serialization cannot be used. Be careful to escape any embedded
-- attributes.
xmlHeader :: T.Text -> JID -> B.ByteString
xmlHeader ns jid = encodeUtf8 header where
	escape = T.pack . DOM.attrEscapeXml . T.unpack
	attr x = T.concat ["\"", escape x, "\""]
	header = T.concat
		[ "<?xml version='1.0'?>\n"
		, "<stream:stream xmlns=" , attr ns
		, " to=", attr (formatJID jid)
		, " version=\"1.0\""
		, " xmlns:stream=\"http://etherx.jabber.org/streams\">"
		]

startOfStream :: Integer -> SAX.Event -> Bool
startOfStream depth event = case (depth, event) of
	(1, (SAX.BeginElement elemName _)) ->
		qnameStream == convertQName elemName
	_ -> False

qnameStream :: DOM.QName
qnameStream = qname "http://etherx.jabber.org/streams" "stream"
