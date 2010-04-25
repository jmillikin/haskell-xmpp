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
	, hGetChar
	) where

import Control.Monad (when)
import qualified Control.Monad.Error as E
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text as T
import qualified System.IO as IO
import qualified Network.GnuTLS as GnuTLS
import Network.GnuTLS (AttrOp (..))
import Foreign (allocaBytes, plusPtr)
import Foreign.C (peekCAStringLen)
import Network.Protocol.XMPP.ErrorT

data Handle =
	  PlainHandle IO.Handle
	| SecureHandle IO.Handle (GnuTLS.Session GnuTLS.Client)

startTLS :: Handle -> ErrorT T.Text IO Handle
startTLS (SecureHandle _ _) = E.throwError "Can't start TLS on a secure handle"
startTLS (PlainHandle h) = liftIO $ do
	session <- GnuTLS.tlsClient
		[ GnuTLS.handle := h
		, GnuTLS.priorities := [GnuTLS.CrtX509]
		, GnuTLS.credentials := GnuTLS.certificateCredentials
		]
	GnuTLS.handshake session
	return $ SecureHandle h session

hPutBytes :: Handle -> B.ByteString -> ErrorT T.Text IO ()
hPutBytes (PlainHandle h)           bytes = liftIO $ B.hPut h bytes
hPutBytes (SecureHandle _ session) bytes = liftIO useLoop where
	useLoop = B.unsafeUseAsCStringLen bytes $ uncurry loop
	loop ptr len = do
		r <- GnuTLS.tlsSend session ptr len
		case len - r of
			x | x > 0     -> loop (plusPtr ptr r) x
			  | otherwise -> return ()

hGetChar :: Handle -> ErrorT T.Text IO Char
hGetChar (PlainHandle h) = liftIO $ IO.hGetChar h
hGetChar (SecureHandle h session) = liftIO $ allocaBytes 1 $ \ptr -> do
	pending <- GnuTLS.tlsCheckPending session
	when (pending == 0) $ do
		IO.hWaitForInput h (-1)
		return ()
	
	len <- GnuTLS.tlsRecv session ptr 1
	[char] <- peekCAStringLen (ptr, len)
	return char
