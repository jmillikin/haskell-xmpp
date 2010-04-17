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
	, continueXMPP
	, restartXMPP
	
	, getHandle
	, getContext
	
	, putTree
	, getTree
	
	, putStanza
	, getStanza
	) where
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Control.Monad.Error as E
import qualified Control.Monad.Reader as R
import Data.Text (Text)
import Text.XML.HXT.DOM.Interface (XmlTree)
import qualified Text.XML.LibXML.SAX as SAX
import Network.Protocol.XMPP.ErrorT
import qualified Network.Protocol.XMPP.Handle as H
import qualified Network.Protocol.XMPP.Stanza as S
import qualified Network.Protocol.XMPP.XML as X

data Error
	= InvalidStanza XmlTree
	| InvalidBindResult S.ReceivedStanza
	| AuthenticationFailure
	| AuthenticationError Text
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

runXMPP :: H.Handle -> Text -> XMPP a -> IO (Either Error a)
runXMPP h ns xmpp = do
	sax <- SAX.mkParser
	continueXMPP (Context h ns sax) xmpp

continueXMPP :: Context -> XMPP a -> IO (Either Error a)
continueXMPP ctx xmpp = R.runReaderT (runErrorT (unXMPP xmpp)) ctx

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

putTree :: XmlTree -> XMPP ()
putTree t = do
	h <- getHandle
	liftIO $ X.putTree h t

getTree :: XMPP XmlTree
getTree = do
	Context h _ sax <- getContext
	liftIO $ X.getTree h sax

putStanza :: S.Stanza a => a -> XMPP ()
putStanza = putTree . S.stanzaToTree

getStanza :: XMPP S.ReceivedStanza
getStanza = do
	tree <- getTree
	Context _ ns _ <- getContext
	case S.treeToStanza ns tree of
		Just x -> return x
		Nothing -> E.throwError $ InvalidStanza tree
