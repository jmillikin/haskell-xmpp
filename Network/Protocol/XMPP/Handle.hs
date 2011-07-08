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

module Network.Protocol.XMPP.Handle
	( Handle (..)
	, startTLS
	, hPutBytes
	, hGetBytes
	) where

import           Control.Monad (when)
import qualified Control.Monad.Error as E
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import qualified Data.Text
import           Data.Text (Text)
import qualified System.IO as IO
import qualified Network.Protocol.TLS.GNU as TLS
import           Network.Protocol.XMPP.ErrorT

data Handle =
	  PlainHandle IO.Handle
	| SecureHandle IO.Handle TLS.Session

liftTLS :: TLS.Session -> TLS.TLS a -> ErrorT Text IO a
liftTLS s = liftTLS' . TLS.runTLS s

liftTLS' :: IO (Either TLS.Error a) -> ErrorT Text IO a
liftTLS' io = do
	eitherX <- liftIO io
	case eitherX of
		Left err -> E.throwError $ Data.Text.pack $ show err
		Right x -> return x

startTLS :: Handle -> ErrorT Text IO Handle
startTLS (SecureHandle _ _) = E.throwError "Can't start TLS on a secure handle"
startTLS (PlainHandle h) = liftTLS' $ TLS.runClient (TLS.handleTransport h) $ do
	TLS.setPriority [TLS.X509]
	TLS.setCredentials =<< TLS.certificateCredentials
	TLS.handshake
	SecureHandle h `fmap` TLS.getSession

hPutBytes :: Handle -> ByteString -> ErrorT Text IO ()
hPutBytes (PlainHandle h)  = liftIO . Data.ByteString.hPut h
hPutBytes (SecureHandle _ s) = liftTLS s . TLS.putBytes . toLazy where
	toLazy bytes = Data.ByteString.Lazy.fromChunks [bytes]

hGetBytes :: Handle -> Integer -> ErrorT Text IO ByteString
hGetBytes (PlainHandle h) n = liftIO $ Data.ByteString.hGet h $ fromInteger n
hGetBytes (SecureHandle h s) n = liftTLS s $ do
	pending <- TLS.checkPending
	let wait = IO.hWaitForInput h (- 1) >> return ()
	when (pending == 0) (liftIO wait)
	lazy <- TLS.getBytes n
	return (Data.ByteString.concat (Data.ByteString.Lazy.toChunks lazy))
