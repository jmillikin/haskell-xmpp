{-# LANGUAGE TypeFamilies #-}
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

module Network.Protocol.XMPP.Monad
	( XMPP (..)
	, Error (..)
	, Session (..)
	, runXMPP
	, startXMPP
	, restartXMPP
	
	, getHandle
	, getSession
	
	, readEvents
	, getElement
	, getStanza
	
	, putBytes
	, putElement
	, putStanza
	) where

import qualified Control.Applicative as A
import qualified Control.Concurrent.MVar as M
import           Control.Monad (ap)
import           Control.Monad.Fix (MonadFix, mfix)
import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Control.Monad.Error as E
import qualified Control.Monad.Reader as R
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Text.Lazy (Text)
import           Data.Text.Lazy.Encoding (encodeUtf8)

import           Network.Protocol.XMPP.ErrorT
import qualified Network.Protocol.XMPP.Handle as H
import qualified Network.Protocol.XMPP.Stanza as S
import qualified Network.Protocol.XMPP.XML as X

data Error
	-- | The remote host refused the specified authentication credentials.
	= AuthenticationFailure
	
	-- | There was an error while authenticating with the remote host.
	| AuthenticationError Text
	
	-- | An unrecognized or malformed 'S.Stanza' was received from the remote
	-- host.
	| InvalidStanza X.Element
	
	-- | The remote host sent an invalid reply to a resource bind request.
	| InvalidBindResult S.ReceivedStanza
	
	-- | There was an error with the underlying transport.
	| TransportError Text
	
	-- | The remote host did not send a stream ID when accepting a component
	-- connection.
	| NoComponentStreamID
	deriving (Show)

data Session = Session
	{ sessionHandle :: H.Handle
	, sessionNamespace :: Text
	, sessionParser :: X.Parser
	, sessionReadLock :: M.MVar ()
	, sessionWriteLock :: M.MVar ()
	}

newtype XMPP a = XMPP { unXMPP :: ErrorT Error (R.ReaderT Session IO) a }

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

instance A.Applicative XMPP where
	pure = return
	(<*>) = ap

instance MonadFix XMPP where
	mfix f = XMPP $ mfix $ unXMPP . f

runXMPP :: Session -> XMPP a -> IO (Either Error a)
runXMPP s xmpp = R.runReaderT (runErrorT (unXMPP xmpp)) s

startXMPP :: H.Handle -> Text -> XMPP a -> IO (Either Error a)
startXMPP h ns xmpp = do
	sax <- X.newParser
	readLock <- M.newMVar ()
	writeLock <- M.newMVar ()
	runXMPP (Session h ns sax readLock writeLock) xmpp

restartXMPP :: Maybe H.Handle -> XMPP a -> XMPP a
restartXMPP newH xmpp = do
	Session oldH ns _ readLock writeLock <- getSession
	sax <- liftIO $ X.newParser
	let s = Session (maybe oldH id newH) ns sax readLock writeLock
	XMPP $ R.local (const s) (unXMPP xmpp)

withLock :: (Session -> M.MVar ()) -> XMPP a -> XMPP a
withLock getLock xmpp = do
	s <- getSession
	let mvar = getLock s
	res <- liftIO $ M.withMVar mvar $ \_ -> runXMPP s xmpp
	case res of
		Left err -> E.throwError err
		Right x -> return x

getSession :: XMPP Session
getSession = XMPP R.ask

getHandle :: XMPP H.Handle
getHandle = fmap sessionHandle getSession

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

putElement :: X.Element -> XMPP ()
putElement = putBytes . encodeUtf8 . X.serialiseElement

putStanza :: S.Stanza a => a -> XMPP ()
putStanza = withLock sessionWriteLock . putElement . S.stanzaToElement

readEvents :: (Integer -> X.SaxEvent -> Bool) -> XMPP [X.SaxEvent]
readEvents done = xmpp where
	xmpp = do
		Session h _ p _ _ <- getSession
		let nextEvents = do
			-- TODO: read in larger increments
			bytes <- liftTLS $ H.hGetBytes h 1
			let eof = B.length bytes == 0
			parsed <- liftIO $ X.parse p bytes eof
			case parsed of
				Left err -> E.throwError $ TransportError err
				Right events -> return events
		X.readEvents done nextEvents

getElement :: XMPP X.Element
getElement = xmpp where
	xmpp = do
		events <- readEvents endOfTree
		case X.eventsToElement events of
			Just x -> return x
			Nothing -> E.throwError $ TransportError "getElement: invalid event list"
	
	endOfTree 0 (X.EndElement _) = True
	endOfTree _ _ = False

getStanza :: XMPP S.ReceivedStanza
getStanza = withLock sessionReadLock $ do
	elemt <- getElement
	Session _ ns _ _ _ <- getSession
	case S.elementToStanza ns elemt of
		Just x -> return x
		Nothing -> E.throwError $ InvalidStanza elemt
