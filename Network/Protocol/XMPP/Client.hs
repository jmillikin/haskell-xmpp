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
module Network.Protocol.XMPP.Client
	( Client
	, Server (..)
	, connectClient
	, bindClient
	) where
import Network (HostName, PortID, connectTo)
import Text.XML.HXT.Arrow ((>>>))
import qualified Text.XML.HXT.Arrow as A
import qualified Text.XML.HXT.DOM.Interface as DOM
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified System.IO as IO
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.XML.LibXML.SAX as SAX

import qualified Network.Protocol.XMPP.Internal.Authentication as A
import qualified Network.Protocol.XMPP.Internal.Features as F
import qualified Network.Protocol.XMPP.Internal.Handle as H
import qualified Network.Protocol.XMPP.Internal.Stream as S
import qualified Network.Protocol.XMPP.Stream as S
import Network.Protocol.XMPP.Internal.XML ( getTree, putTree
                                          , element, qname
                                          , readEventsUntil, convertQName
                                          )
import qualified Network.Protocol.XMPP.JID as J
import Network.Protocol.XMPP.Stanza

data Server = Server
	{ serverJID      :: J.JID
	, serverHostname :: HostName
	, serverPort     :: PortID
	}

data Client = Client
	{ clientJID    :: J.JID
	, clientServer :: Server
	, clientStream :: ClientStream
	}

data ClientStream = ClientStream
	{ streamJID      :: J.JID
	, streamHandle   :: H.Handle
	, streamFeatures :: [F.Feature]
	, streamParser   :: SAX.Parser
	}

instance S.Stream Client where
	getTree = S.getTree . clientStream
	putTree = S.putTree . clientStream

instance S.Stream ClientStream where
	getTree s = getTree (streamHandle s) (streamParser s)
	putTree s = putTree (streamHandle s)

connectClient :: Server -> J.JID -> T.Text -> T.Text -> IO Client
connectClient server jid username password = do
	-- Open a TCP connection
	let Server sjid host port = server
	rawHandle <- connectTo host port
	IO.hSetBuffering rawHandle IO.NoBuffering
	let handle = H.PlainHandle rawHandle
	
	-- Open the initial stream and authenticate
	stream <- beginClientStream server handle
	authedStream <- authenticate stream jid sjid username password
	return $ Client jid server authedStream

authenticate :: ClientStream -> J.JID -> J.JID -> T.Text -> T.Text -> IO ClientStream
authenticate stream jid sjid username password = do
	let mechanisms = authenticationMechanisms stream
	result <- A.authenticate stream mechanisms jid sjid username password
	case result of
		-- TODO: throwIO some exception type?
		A.Failure -> error "Authentication failure"
		_ -> restartStream stream

authenticationMechanisms :: ClientStream -> [T.Text]
authenticationMechanisms = step . streamFeatures where
	step [] = []
	step (f:fs) = case f of
		(F.FeatureSASL ms) -> ms
		_ -> step fs

-- TODO: does it make sense to put this in 'connect'?
-- Can multiple resources be bound to one client?
bindClient :: Client -> IO J.JID
bindClient c = do
	-- Bind
	S.putStanza c $ bindStanza . J.jidResource . clientJID $ c
	bindResult <- S.getStanza c
	
	let jidArrow =
		A.deep (A.hasQName (qname "urn:ietf:params:xml:ns:xmpp-bind" "jid"))
		>>> A.getChildren
		>>> A.getText
	
	-- TODO: throwIO with exception
	let Just jid = do
		result <- bindResult
		iq <- case result of
			ReceivedIQ x -> Just x
			_ -> Nothing
		
		case A.runLA jidArrow (iqPayload iq) of
			[] -> Nothing
			(str:_) -> J.parseJID (T.pack str)
	
	-- Session
	S.putStanza c sessionStanza
	S.getStanza c
	
	S.putStanza c $ emptyPresence PresenceAvailable
	S.getStanza c
	
	return jid

bindStanza :: Maybe J.Resource -> IQ
bindStanza resource = emptyIQ IQSet payload where
	payload = element ("", "bind")
		[("", "xmlns", "urn:ietf:params:xml:ns:xmpp-bind")]
		requested
	requested = case fmap J.strResource resource of
		Nothing -> []
		Just x -> [element ("", "resource")
			[]
			[XN.mkText (T.unpack x)]]

sessionStanza :: IQ
sessionStanza = emptyIQ IQSet $ element ("", "session")
	[("", "xmlns", "urn:ietf:params:xml:ns:xmpp-session")]
	[]

beginClientStream :: Server -> H.Handle -> IO ClientStream
beginClientStream server handle = do
	let jid = serverJID server
	plain <- newStream jid handle
	if streamSupportsTLS plain
		then do
			S.putTree plain xmlStartTLS
			S.getTree plain -- TODO: verify
			H.startTLS handle >>= newStream jid
		else return plain

restartStream :: ClientStream -> IO ClientStream
restartStream s = newStream (streamJID s) (streamHandle s)

newStream :: J.JID -> H.Handle -> IO ClientStream
newStream jid h = do
	let startOfStream depth event = case (depth, event) of
		(1, (SAX.BeginElement elemName _)) ->
			qnameStream == convertQName elemName
		_ -> False
	
	parser <- SAX.mkParser
	H.hPutBytes h $ xmlHeader "jabber:client" jid
	readEventsUntil startOfStream h parser
	features <- F.parseFeatures `fmap` getTree h parser
	
	return $ ClientStream jid h features parser

streamSupportsTLS :: ClientStream -> Bool
streamSupportsTLS = any isStartTLS . streamFeatures where
	isStartTLS (F.FeatureStartTLS _) = True
	isStartTLS _                     = False

xmlStartTLS :: DOM.XmlTree
xmlStartTLS = element ("", "starttls")
	[("", "xmlns", "urn:ietf:params:xml:ns:xmpp-tls")]
	[]

-- Since only the opening tag should be written, normal XML
-- serialization cannot be used. Be careful to escape any embedded
-- attributes.
xmlHeader :: T.Text -> J.JID -> B.ByteString
xmlHeader ns jid = TE.encodeUtf8 header where
	escape = T.pack . DOM.attrEscapeXml . T.unpack -- TODO: optimize?
	attr x = T.concat ["\"", escape x, "\""]
	header = T.concat
		[ "<?xml version='1.0'?>\n"
		, "<stream:stream xmlns=" , attr ns
		, " to=", attr (J.formatJID jid)
		, " version=\"1.0\""
		, " xmlns:stream=\"http://etherx.jabber.org/streams\">"
		]

qnameStream :: DOM.QName
qnameStream = qname "http://etherx.jabber.org/streams" "stream"
