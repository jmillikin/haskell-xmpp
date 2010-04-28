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
module Network.Protocol.XMPP.Handle
	( Handle (..)
	, startTLS
	, hPutBytes
	, hGetBytes
	) where

import Control.Monad (when)
import qualified Control.Monad.Error as E
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified System.IO as IO
import qualified Network.Protocol.TLS.GNU as TLS
import Network.Protocol.XMPP.ErrorT

data Handle =
	  PlainHandle IO.Handle
	| SecureHandle IO.Handle TLS.Session

liftTLS :: TLS.Session -> TLS.TLS a -> ErrorT T.Text IO a
liftTLS s = liftTLS' . TLS.runTLS s

liftTLS' :: IO (Either TLS.Error a) -> ErrorT T.Text IO a
liftTLS' io = do
	eitherX <- liftIO io
	case eitherX of
		Left err -> E.throwError $ T.pack $ show err
		Right x -> return x

startTLS :: Handle -> ErrorT T.Text IO Handle
startTLS (SecureHandle _ _) = E.throwError "Can't start TLS on a secure handle"
startTLS (PlainHandle h) = liftTLS' $ TLS.runClient (TLS.handleTransport h) $ do
	TLS.setPriority [TLS.X509]
	TLS.setCredentials =<< TLS.certificateCredentials
	TLS.handshake
	SecureHandle h `fmap` TLS.getSession

hPutBytes :: Handle -> B.ByteString -> ErrorT T.Text IO ()
hPutBytes (PlainHandle h)  = liftIO . B.hPut h
hPutBytes (SecureHandle _ s) = liftTLS s . TLS.putBytes

hGetBytes :: Handle -> Integer -> ErrorT T.Text IO B.ByteString
hGetBytes (PlainHandle h) n = liftIO $  B.hGet h $ fromInteger n
hGetBytes (SecureHandle h s) n = liftTLS s $ do
	pending <- TLS.checkPending
	when (pending == 0) $ do
		liftIO $ IO.hWaitForInput h (- 1)
		return ()
	
	TLS.getBytes n
