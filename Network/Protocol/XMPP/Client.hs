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
	( runClient
	, bindJID
	) where
import Control.Monad.Error (throwError)
import Control.Monad.Trans (liftIO)
import Network (connectTo)
import Text.XML.HXT.Arrow ((>>>))
import qualified Text.XML.HXT.Arrow as A
import qualified Text.XML.HXT.DOM.Interface as DOM
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified System.IO as IO
import Data.ByteString (ByteString)
import qualified Data.Text as T

import qualified Network.Protocol.XMPP.Client.Authentication as A
import qualified Network.Protocol.XMPP.Connections as C
import qualified Network.Protocol.XMPP.Client.Features as F
import qualified Network.Protocol.XMPP.Handle as H
import qualified Network.Protocol.XMPP.JID as J
import qualified Network.Protocol.XMPP.Monad as M
import Network.Protocol.XMPP.XML (element, qname, readEventsUntil)
import Network.Protocol.XMPP.Stanza

runClient :: C.Server
          -> J.JID -- ^ Client JID
          -> T.Text -- ^ Username
          -> T.Text -- ^ Password
          -> M.XMPP a
          -> IO (Either M.Error a)
runClient server jid username password xmpp = do
	-- Open a TCP connection
	let C.Server sjid host port = server
	rawHandle <- connectTo host port
	IO.hSetBuffering rawHandle IO.NoBuffering
	let handle = H.PlainHandle rawHandle
	
	-- Open the initial stream and authenticate
	M.runXMPP handle "jabber:client" $ do
		features <- newStream sjid
		let mechanisms = authenticationMechanisms features
		tryTLS features $ do
			A.authenticate mechanisms jid sjid username password
			M.restartXMPP Nothing xmpp

newStream :: J.JID -> M.XMPP [F.Feature]
newStream jid = do
	M.Context h _ sax <- M.getContext
	liftIO $ H.hPutBytes h $ C.xmlHeader "jabber:client" jid
	liftIO $ readEventsUntil C.startOfStream h sax
	F.parseFeatures `fmap` M.getTree

tryTLS :: [F.Feature] -> M.XMPP a -> M.XMPP a
tryTLS features m
	| not (streamSupportsTLS features) = m
	| otherwise = do
		M.putTree xmlStartTLS
		M.getTree
		h <- M.getHandle
		tls <- liftIO $ H.startTLS h
		M.restartXMPP (Just tls) m

authenticationMechanisms :: [F.Feature] -> [ByteString]
authenticationMechanisms = step where
	step [] = []
	step (f:fs) = case f of
		(F.FeatureSASL ms) -> ms
		_ -> step fs

bindJID :: J.JID -> M.XMPP J.JID
bindJID jid = do
	-- Bind
	M.putStanza . bindStanza . J.jidResource $ jid
	bindResult <- M.getStanza
	
	let jidArrow =
		A.deep (A.hasQName (qname "urn:ietf:params:xml:ns:xmpp-bind" "jid"))
		>>> A.getChildren
		>>> A.getText
	
	let maybeJID = do
		iq <- case bindResult of
			ReceivedIQ x -> Just x
			_ -> Nothing
		
		case A.runLA jidArrow (iqPayload iq) of
			[] -> Nothing
			(str:_) -> J.parseJID (T.pack str)
	
	returnedJID <- case maybeJID of
		Just x -> return x
		Nothing -> throwError $ M.InvalidBindResult bindResult
	
	-- Session
	M.putStanza sessionStanza
	M.getStanza
	
	M.putStanza $ emptyPresence PresenceAvailable
	M.getStanza
	
	return returnedJID

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

streamSupportsTLS :: [F.Feature] -> Bool
streamSupportsTLS = any isStartTLS where
	isStartTLS (F.FeatureStartTLS _) = True
	isStartTLS _                     = False

xmlStartTLS :: DOM.XmlTree
xmlStartTLS = element ("", "starttls")
	[("", "xmlns", "urn:ietf:params:xml:ns:xmpp-tls")]
	[]
