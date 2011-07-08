{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 Stephan Maka <stephan@spaceboyz.net>
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

module Network.Protocol.XMPP.Component
	( runComponent
	) where

import           Control.Monad (when)
import           Control.Monad.Error (throwError)
import           Data.Bits (shiftR, (.&.))
import           Data.Char (intToDigit)
import qualified Data.ByteString
import           Data.ByteString (ByteString)
import qualified Data.Text
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Network (connectTo)
import           Network.Protocol.SASL.GNU (sha1)
import qualified System.IO as IO

import qualified Network.Protocol.XMPP.Connections as C
import qualified Network.Protocol.XMPP.Handle as H
import qualified Network.Protocol.XMPP.Monad as M
import qualified Network.Protocol.XMPP.XML as X
import           Network.Protocol.XMPP.JID (JID)

runComponent :: C.Server
             -> Text -- ^ Server secret
             -> M.XMPP a
             -> IO (Either M.Error a)
runComponent server password xmpp = do
	let C.Server jid host port = server
	rawHandle <- connectTo host port
	IO.hSetBuffering rawHandle IO.NoBuffering
	let handle = H.PlainHandle rawHandle
	M.startXMPP handle "jabber:component:accept" $ do
		streamID <- beginStream jid
		authenticate streamID password
		xmpp

beginStream :: JID -> M.XMPP Text
beginStream jid = do
	M.putBytes $ C.xmlHeader "jabber:component:accept" jid
	events <- M.readEvents C.startOfStream
	case parseStreamID $ last events of
		Nothing -> throwError M.NoComponentStreamID
		Just x -> return x

parseStreamID :: X.Event -> Maybe Text
parseStreamID (X.EventBeginElement name attrs) = X.attributeText
	"{jabber:component:accept}jid"
	(X.Element name attrs [])
parseStreamID _ = Nothing

authenticate :: Text -> Text -> M.XMPP ()
authenticate streamID password = do
	let bytes = buildSecret streamID password
	let digest = showDigest (sha1 bytes)
	M.putElement (X.element "handshake" [] [X.NodeContent (X.ContentText digest)])
	result <- M.getElement
	let nameHandshake = "{jabber:component:accept}handshake"
	when (null (X.isNamed nameHandshake result)) (throwError M.AuthenticationFailure)

buildSecret :: Text -> Text -> ByteString
buildSecret sid password = encodeUtf8 (X.escape (Data.Text.append sid password))

showDigest :: ByteString -> Text
showDigest = Data.Text.pack . concatMap wordToHex . Data.ByteString.unpack where
	wordToHex x = [hexDig (shiftR x 4), hexDig (x .&. 0xF)]
	hexDig = intToDigit . fromIntegral
