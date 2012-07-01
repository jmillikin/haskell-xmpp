-- Copyright (c) 2010 John Millikin
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use,
-- copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the
-- Software is furnished to do so, subject to the following
-- conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
-- OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
-- HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
-- WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
-- OTHER DEALINGS IN THE SOFTWARE.

{-# LANGUAGE OverloadedStrings #-}
module Main where

-- XMPP imports
import Network
import Network.Protocol.XMPP
import Data.XML.Types

-- other imports
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import System.Environment

runEcho :: String -> T.Text -> T.Text -> IO ()
runEcho hostname user password = do
	-- Verify that the user provided a valid JID, and that it contains a username
	-- (AKA a "node").
	jid <- case parseJID user of
		Just x -> return x
		Nothing -> error $ "Invalid JID: " ++ show user
	username <- case strNode `fmap` jidNode jid of
		Just x -> return x
		Nothing -> error $ "JID must include a username"
	
	-- 'Server' values record what host the connection will be opened to. Normally
	-- the hostname and JID will be the same; however, in some cases the hostname is
	-- something special (like "jabber.domain.com" or "localhost").
	-- 
	-- The port number is hardcoded to 5222 in this example, but in the wild there
	-- might be servers with a jabberd running on alternative ports.
	let server = Server
		{ serverHostname = hostname
		, serverJID = JID Nothing (jidDomain jid) Nothing
		, serverPort = PortNumber 5222
		}
	
	-- 'runClient' and 'runComponent' open a connection to the remote server and
	-- establish an XMPP session.
	-- 
	-- It is possible to run an XMPP session over multiple IO chunks using the
	-- 'getSession' computation. The returned session value can be used to run
	-- 'runXMPP'.
	-- 
	-- Unusual conditions like socket errors or async exceptions might cause this
	-- computation to raise an exception, but in normal operation all XMPP errors
	-- are returned via a 'Left' value.
	-- 
	-- 'XMPP' is an instance of 'MonadError', so you can use the standard
	-- 'throwError' and 'catchError' computations to handle errors within an XMPP
	-- session.
	res <- runClient server jid username password $ do
		
		-- Some servers will close the XMPP connection after some period
		-- of inactivity. For this example, we'll simply send a "ping" every
		-- 60 seconds
		getSession >>= liftIO . forkIO . sendPings 60
		
		-- When running a client session, most servers require the user to
		-- "bind" their JID before sending any stanzas.
		boundJID <- bindJID jid
		
		-- 'XMPP' is an instance of 'MonadIO', so any IO may be performed
		-- within.
		liftIO $ putStrLn $ "Server bound our session to: " ++ show boundJID
		
		-- This is a simple loop which will echo received messages back to the
		-- sender; additionally, it prints *all* received stanzas to the console.
		forever $ do
			stanza <- getStanza
			liftIO $ putStr "\n" >> print stanza >> putStrLn "\n"
			case stanza of
				ReceivedMessage msg -> if messageType msg == MessageError
					then return ()
					else putStanza $ echo msg
				ReceivedPresence msg -> if presenceType msg == PresenceSubscribe
					then putStanza (subscribe msg)
					else return ()
				_ -> return ()
	
	-- If 'runClient' terminated due to an XMPP error, propagate it as an exception.
	-- In non-example code, you might want to show this error to the user.
	case res of
		Left err -> error $ show err
		Right _ -> return ()

-- Copy a 'Message' into another message, setting the 'messageTo' field to the
-- original sender's address.
echo :: Message -> Message
echo msg = Message
	{ messageType = MessageNormal
	, messageTo = messageFrom msg
	
	-- Note: Conforming XMPP servers populate the "from" attribute on
	-- stanzas, to prevent clients from spoofing it. Therefore, the
	-- 'messageFrom' field's value is irrelevant when sending stanzas.
	, messageFrom = Nothing
	
	, messageID = Nothing
	, messageLang = Nothing
	, messagePayloads = messagePayloads msg
	}

subscribe :: Presence -> Presence
subscribe p = Presence
	{ presenceType = PresenceSubscribed
	, presenceTo = presenceFrom p
	, presenceFrom = Nothing
	, presenceID = Nothing
	, presenceLang = Nothing
	, presencePayloads = []
	}

-- Send a "ping" occasionally, to prevent server timeouts from
-- closing the connection.
sendPings :: Integer -> Session -> IO ()
sendPings seconds s = forever send where
	send = do
		-- Ignore errors
		runXMPP s $ putStanza ping
		threadDelay $ fromInteger $ 1000000 * seconds
	ping = (emptyIQ IQGet)
		{ iqPayload = Just (Element pingName [] [])
		}

pingName :: Name
pingName = Name "ping" (Just "urn:xmpp:ping") Nothing

main :: IO ()
main = do
	args <- getArgs
	case args of
		(server:user:pass:_) -> runEcho server (T.pack user) (T.pack pass)
		_ -> do
			name <- getProgName
			error $ "Use: " ++ name ++ " <server> <username> <password>"
