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

module Network.Protocol.XMPP.Client
	( runClient
	, bindJID
	) where

import           Control.Monad ((>=>))
import           Control.Monad.Error (throwError)
import           Control.Monad.Trans (liftIO)
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Network (connectTo)
import qualified System.IO as IO

import qualified Network.Protocol.XMPP.Client.Authentication as A
import qualified Network.Protocol.XMPP.Connections as C
import qualified Network.Protocol.XMPP.Client.Features as F
import qualified Network.Protocol.XMPP.Handle as H
import qualified Network.Protocol.XMPP.JID as J
import qualified Network.Protocol.XMPP.Monad as M
import qualified Network.Protocol.XMPP.XML as X
import           Network.Protocol.XMPP.ErrorT
import           Network.Protocol.XMPP.Stanza

runClient :: C.Server
          -> J.JID -- ^ Client JID
          -> Text -- ^ Username
          -> Text -- ^ Password
          -> M.XMPP a
          -> IO (Either M.Error a)
runClient server jid username password xmpp = do
	-- Open a TCP connection
	let C.Server sjid host port = server
	rawHandle <- connectTo host port
	IO.hSetBuffering rawHandle IO.NoBuffering
	let handle = H.PlainHandle rawHandle
	
	-- Open the initial stream and authenticate
	M.startXMPP handle "jabber:client" $ do
		features <- newStream sjid
		tryTLS sjid features $ \tlsFeatures -> do
			let mechanisms = authenticationMechanisms tlsFeatures
			A.authenticate mechanisms jid sjid username password
			M.restartXMPP Nothing (newStream sjid >> xmpp)

newStream :: J.JID -> M.XMPP [F.Feature]
newStream jid = do
	M.putBytes $ C.xmlHeader "jabber:client" jid
	void (M.readEvents C.startOfStream)
	F.parseFeatures `fmap` M.getElement

tryTLS :: J.JID -> [F.Feature] -> ([F.Feature] -> M.XMPP a) -> M.XMPP a
tryTLS sjid features m
	| not (streamSupportsTLS features) = m features
	| otherwise = do
		M.putElement xmlStartTLS
		void M.getElement
		h <- M.getHandle
		eitherTLS <- liftIO $ runErrorT $ H.startTLS h
		case eitherTLS of
			Left err -> throwError $ M.TransportError err
			Right tls -> M.restartXMPP (Just tls) $ newStream sjid >>= m

authenticationMechanisms :: [F.Feature] -> [ByteString]
authenticationMechanisms = step where
	step [] = []
	step (f:fs) = case f of
		(F.FeatureSASL ms) -> ms
		_ -> step fs

-- | Send a @\<bind\>@ message for the given 'J.JID', returning the server's reply. In
-- most cases the reply will be the same as the input. However, if the input has no
-- 'J.Resource', the returned 'J.JID' will contain a generated 'J.Resource'.
-- 
-- Clients must bind a 'J.JID' before sending any 'Stanza's.
bindJID :: J.JID -> M.XMPP J.JID
bindJID jid = do
	-- Bind
	M.putStanza . bindStanza . J.jidResource $ jid
	bindResult <- M.getStanza
	let getJID =
		X.elementChildren
		>=> X.isNamed "{urn:ietf:params:xml:ns:xmpp-bind}jid"
		>=> X.elementNodes
		>=> X.isContent
		>=> return . X.contentText
	
	let maybeJID = do
		iq <- case bindResult of
			ReceivedIQ x -> Just x
			_ -> Nothing
		payload <- iqPayload iq
		
		case getJID payload of
			[] -> Nothing
			(str:_) -> J.parseJID str
	
	returnedJID <- case maybeJID of
		Just x -> return x
		Nothing -> throwError $ M.InvalidBindResult bindResult
	
	-- Session
	M.putStanza sessionStanza
	void M.getStanza
	
	M.putStanza $ emptyPresence PresenceAvailable
	void M.getStanza
	
	return returnedJID

bindStanza :: Maybe J.Resource -> IQ
bindStanza resource = (emptyIQ IQSet) { iqPayload = Just payload } where
	payload = X.element "{urn:ietf:params:xml:ns:xmpp-bind}bind" [] requested
	requested = case fmap J.strResource resource of
		Nothing -> []
		Just x -> [X.NodeElement $ X.element "resource" []
			[X.NodeContent $ X.ContentText x]]

sessionStanza :: IQ
sessionStanza = (emptyIQ IQSet) { iqPayload = Just payload } where
	payload = X.element "{urn:ietf:params:xml:ns:xmpp-session}session" [] []

streamSupportsTLS :: [F.Feature] -> Bool
streamSupportsTLS = any isStartTLS where
	isStartTLS (F.FeatureStartTLS _) = True
	isStartTLS _                     = False

xmlStartTLS :: X.Element
xmlStartTLS = X.element "{urn:ietf:params:xml:ns:xmpp-tls}starttls" [] []

void :: Monad m => m a -> m ()
void m = m >> return ()
