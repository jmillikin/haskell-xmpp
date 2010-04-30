-- Copyright (C) 2010 Stephan Maka <stephan@spaceboyz.net>
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
module Network.Protocol.XMPP.Component
	( runComponent
	) where
import Control.Monad (when)
import Control.Monad.Error (throwError)
import Data.Bits (shiftR, (.&.))
import Data.Char (intToDigit)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import Network (connectTo)
import Network.Protocol.SASL.GNU (sha1)
import qualified System.IO as IO
import qualified Text.XML.LibXML.SAX as SAX

import qualified Network.Protocol.XMPP.Connections as C
import qualified Network.Protocol.XMPP.Handle as H
import qualified Network.Protocol.XMPP.Monad as M
import qualified Network.Protocol.XMPP.XML as X
import Network.Protocol.XMPP.JID (JID)

runComponent :: C.Server
             -> T.Text -- ^ Server secret
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

beginStream :: JID -> M.XMPP T.Text
beginStream jid = do
	M.putBytes $ C.xmlHeader "jabber:component:accept" jid
	events <- M.readEvents C.startOfStream
	case parseStreamID $ last events of
		Nothing -> throwError M.NoComponentStreamID
		Just x -> return x

parseStreamID :: SAX.Event -> Maybe T.Text
parseStreamID (SAX.BeginElement _ attrs) = sid where
	sid = case idAttrs of
		(x:_) -> Just . X.attributeValue $ x
		_ -> Nothing
	idAttrs = filter (matchingName . X.attributeName) attrs
	matchingName = (== X.Name "jid" (Just "jabber:component:accept") Nothing)
parseStreamID _ = Nothing

authenticate :: T.Text -> T.Text -> M.XMPP ()
authenticate streamID password = do
	let bytes = buildSecret streamID password
	let digest = showDigest $ sha1 bytes
	M.putElement $ X.element "handshake" [] [X.NodeText digest]
	result <- M.getElement
	let nameHandshake = X.Name "handshake" (Just "jabber:component:accept") Nothing
	when (null (X.named nameHandshake result)) $
		throwError M.AuthenticationFailure

buildSecret :: T.Text -> T.Text -> B.ByteString
buildSecret sid password = B.concat . BL.toChunks $ bytes where
	bytes = TE.encodeUtf8 $ X.escape $ T.append sid password

showDigest :: B.ByteString -> T.Text
showDigest = T.pack . concatMap wordToHex . B.unpack where
	wordToHex x = [hexDig $ shiftR x 4, hexDig $ x .&. 0xF]
	hexDig = intToDigit . fromIntegral
