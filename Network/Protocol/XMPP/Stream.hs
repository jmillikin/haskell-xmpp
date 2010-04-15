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

module Network.Protocol.XMPP.Stream
	( Stream (..)
	, putStanza
	, getStanza
	) where
import qualified Data.Text as T
import Text.XML.HXT.DOM.Interface (XmlTree)
import qualified Network.Protocol.XMPP.Stanza as S

class Stream a where
	streamNamespace :: a -> T.Text
	putTree :: a -> XmlTree -> IO ()
	getTree :: a -> IO XmlTree

putStanza :: (Stream stream, S.Stanza stanza) => stream -> stanza -> IO ()
putStanza stream = putTree stream . S.stanzaToTree

getStanza :: Stream stream => stream -> IO (Maybe S.ReceivedStanza)
getStanza stream = S.treeToStanza (streamNamespace stream) `fmap` getTree stream
