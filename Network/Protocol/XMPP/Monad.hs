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

{-# LANGUAGE TypeFamilies #-}
module Network.Protocol.XMPP.Monad
	( XMPP (..)
	, Error (..)
	, Context (..)
	, runXMPP
	, startXMPP
	, restartXMPP
	
	, getHandle
	, getContext
	
	, readEvents
	, getChar
	, getTree
	, getStanza
	
	, putBytes
	, putTree
	, putStanza
	) where
import Prelude hiding (getChar)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Control.Monad.Error as E
import qualified Control.Monad.Reader as R
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text.Lazy (Text)

import Text.XML.HXT.Arrow ((>>>))
import qualified Text.XML.HXT.Arrow as A
import qualified Text.XML.HXT.DOM.Interface as DOM
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Text.XML.LibXML.SAX as SAX

import Network.Protocol.XMPP.ErrorT
import qualified Network.Protocol.XMPP.Handle as H
import qualified Network.Protocol.XMPP.Stanza as S
import qualified Network.Protocol.XMPP.XML as X

data Error
	= InvalidStanza DOM.XmlTree
	| InvalidBindResult S.ReceivedStanza
	| AuthenticationFailure
	| AuthenticationError Text
	| TransportError Text
	| NoComponentStreamID
	| ComponentHandshakeFailed
	deriving (Show)

data Context = Context H.Handle Text SAX.Parser

newtype XMPP a = XMPP { unXMPP :: ErrorT Error (R.ReaderT Context IO) a }

instance Functor XMPP where
	fmap f = XMPP . fmap f . unXMPP

instance Monad XMPP where
	return = XMPP . return
	m >>= f = XMPP $ unXMPP m >>= unXMPP . f

instance MonadIO XMPP where
	liftIO = XMPP . liftIO

instance E.MonadError XMPP where
	type E.ErrorType XMPP = Error
	throwError = XMPP . E.throwError
	catchError m h = XMPP $ E.catchError (unXMPP m) (unXMPP . h)

runXMPP :: Context -> XMPP a -> IO (Either Error a)
runXMPP ctx xmpp = R.runReaderT (runErrorT (unXMPP xmpp)) ctx

startXMPP :: H.Handle -> Text -> XMPP a -> IO (Either Error a)
startXMPP h ns xmpp = do
	sax <- SAX.mkParser
	runXMPP (Context h ns sax) xmpp

restartXMPP :: Maybe H.Handle -> XMPP a -> XMPP a
restartXMPP newH xmpp = do
	Context oldH ns _ <- getContext
	sax <- liftIO $ SAX.mkParser
	let ctx = Context (maybe oldH id newH) ns sax
	XMPP $ R.local (const ctx) (unXMPP xmpp)

getContext :: XMPP Context
getContext = XMPP R.ask

getHandle :: XMPP H.Handle
getHandle = do
	Context h _ _ <- getContext
	return h

liftTLS :: ErrorT Text IO a -> XMPP a
liftTLS io = do
	res <- liftIO $ runErrorT io
	case res of
		Left err -> E.throwError $ TransportError err
		Right x -> return x


putBytes :: B.ByteString -> XMPP ()
putBytes bytes = do
	h <- getHandle
	liftTLS $ H.hPutBytes h bytes

getChar :: XMPP Char
getChar = do
	h <- getHandle
	liftTLS $ H.hGetChar h

putTree :: DOM.XmlTree -> XMPP ()
putTree t = do
	let root = XN.mkRoot [] [t]
	[text] <- liftIO $ A.runX (A.constA root >>> A.writeDocumentToString [
		(A.a_no_xml_pi, "1")
		])
	putBytes $ B.pack text

putStanza :: S.Stanza a => a -> XMPP ()
putStanza = putTree . S.stanzaToTree

readEvents :: (Integer -> SAX.Event -> Bool) -> XMPP [SAX.Event]
readEvents done = do
	Context h _ p <- getContext
	X.readEvents done (liftTLS $ H.hGetChar h) p

getTree :: XMPP DOM.XmlTree
getTree = X.eventsToTree `fmap` readEvents endOfTree where
	endOfTree 0 (SAX.EndElement _) = True
	endOfTree _ _ = False

getStanza :: XMPP S.ReceivedStanza
getStanza = do
	tree <- getTree
	Context _ ns _ <- getContext
	case S.treeToStanza ns tree of
		Just x -> return x
		Nothing -> E.throwError $ InvalidStanza tree
