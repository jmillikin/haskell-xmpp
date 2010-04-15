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
	, clientJID
	, connectClient
	, bindClient
	) where
import Network (connectTo)
import Text.XML.HXT.Arrow ((>>>))
import qualified Text.XML.HXT.Arrow as A
import qualified Text.XML.HXT.DOM.Interface as DOM
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified System.IO as IO
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Text.XML.LibXML.SAX as SAX

import qualified Network.Protocol.XMPP.Authentication as A
import qualified Network.Protocol.XMPP.Connections as C
import qualified Network.Protocol.XMPP.Features as F
import qualified Network.Protocol.XMPP.Handle as H
import qualified Network.Protocol.XMPP.Stream as S
import Network.Protocol.XMPP.XML ( getTree, putTree
                                       , element, qname
                                       , readEventsUntil
                                       )
import qualified Network.Protocol.XMPP.JID as J
import Network.Protocol.XMPP.Stanza

data Client = Client
	{ clientJID    :: J.JID
	, clientStream :: ClientStream
	}

data ClientStream = ClientStream
	{ streamJID      :: J.JID
	, streamHandle   :: H.Handle
	, streamFeatures :: [F.Feature]
	, streamParser   :: SAX.Parser
	}

instance S.Stream Client where
	streamNamespace _ = "jabber:client"
	getTree = S.getTree . clientStream
	putTree = S.putTree . clientStream

instance S.Stream ClientStream where
	streamNamespace _ = "jabber:client"
	getTree s = getTree (streamHandle s) (streamParser s)
	putTree s = putTree (streamHandle s)

connectClient :: C.Server
              -> J.JID -- ^ Client JID
              -> T.Text -- ^ Username
              -> T.Text -- ^ Password
              -> IO Client
connectClient server jid username password = do
	-- Open a TCP connection
	let C.Server sjid host port = server
	rawHandle <- connectTo host port
	IO.hSetBuffering rawHandle IO.NoBuffering
	let handle = H.PlainHandle rawHandle
	
	-- Open the initial stream and authenticate
	stream <- beginStream sjid handle
	authedStream <- authenticate stream jid sjid username password
	return $ Client jid authedStream

authenticate :: ClientStream -> J.JID -> J.JID -> T.Text -> T.Text -> IO ClientStream
authenticate stream jid sjid username password = do
	let mechanisms = authenticationMechanisms stream
	result <- A.authenticate stream mechanisms jid sjid username password
	case result of
		-- TODO: throwIO some exception type?
		A.Failure -> error "Authentication failure"
		_ -> restartStream stream

authenticationMechanisms :: ClientStream -> [ByteString]
authenticationMechanisms = step . streamFeatures where
	step [] = []
	step (f:fs) = case f of
		(F.FeatureSASL ms) -> ms
		_ -> step fs

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

beginStream :: J.JID -> H.Handle -> IO ClientStream
beginStream jid handle = do
	plain <- newStream jid handle
	if streamSupportsTLS plain
		then do
			S.putTree plain xmlStartTLS
			S.getTree plain
			H.startTLS handle >>= newStream jid
		else return plain

restartStream :: ClientStream -> IO ClientStream
restartStream s = newStream (streamJID s) (streamHandle s)

newStream :: J.JID -> H.Handle -> IO ClientStream
newStream jid h = do
	parser <- SAX.mkParser
	H.hPutBytes h $ C.xmlHeader "jabber:client" jid
	readEventsUntil C.startOfStream h parser
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
