-- Copyright (C) 2009-2010 John Millikin <jmillikin@gmail.com>
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

module Network.Protocol.XMPP.XML
	( readEvents
	, eventsToTree
	, convertQName
	, element
	, attr
	, qname
	) where
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- XML Parsing
import Text.XML.HXT.Arrow ((>>>))
import qualified Text.XML.HXT.Arrow as A
import qualified Text.XML.HXT.DOM.Interface as DOM
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Text.XML.LibXML.SAX as SAX

readEvents :: MonadIO m => (Integer -> SAX.Event -> Bool) -> m Char -> SAX.Parser -> m [SAX.Event]
readEvents done getChar parser = readEvents' 0 [] where
	nextEvents = do
		char <- getChar
		liftIO $ SAX.parse parser [char] False
	
	readEvents' depth acc = do
		events <- nextEvents
		let (done', depth', acc') = step events depth acc
		if done'
			then return acc'
			else readEvents' depth' acc'
	
	step []     depth acc = (False, depth, acc)
	step (e:es) depth acc = let
		depth' = depth + case e of
			(SAX.BeginElement _ _) -> 1
			(SAX.EndElement _) -> (- 1)
			_ -> 0
		acc' = e : acc
		in if done depth' e
			then (True, depth', reverse acc')
			else step es depth' acc'

-------------------------------------------------------------------------------
-- For converting incremental XML event lists to HXT trees
-------------------------------------------------------------------------------

-- This function assumes the input list is valid. No validation is performed.
eventsToTree :: [SAX.Event] -> DOM.XmlTree
eventsToTree = XN.mkRoot [] . eventsToTrees

eventsToTrees :: [SAX.Event] -> [DOM.XmlTree]
eventsToTrees = concatMap blockToTrees . splitBlocks

-- Split event list into a sequence of "blocks", which are the events including
-- and between a pair of tags. <start><start2/></start> and <start/> are both
-- single blocks.
splitBlocks :: [SAX.Event] -> [[SAX.Event]]
splitBlocks es = ret where (_, _, ret) = foldl splitBlocks' (0, [], []) es

splitBlocks' :: (Int, [SAX.Event], [[SAX.Event]])
                -> SAX.Event
                -> (Int, [SAX.Event], [[SAX.Event]])
splitBlocks' (depth, accum, allAccum) e =
	if depth' == 0 then
		(depth', [], allAccum ++ [accum'])
	else
		(depth', accum', allAccum)
	where
		accum' = accum ++ [e]
		depth' = depth + case e of
			(SAX.BeginElement _ _) -> 1
			(SAX.EndElement _) -> (- 1)
			_ -> 0

blockToTrees :: [SAX.Event] -> [DOM.XmlTree]
blockToTrees [] = []
blockToTrees (begin:rest) = let end = (last rest) in case (begin, end) of
	(SAX.BeginElement qname' attrs, SAX.EndElement _) ->
		[XN.mkElement (convertQName qname')
			(map convertAttr attrs)
			(eventsToTrees (init rest))]
	(SAX.Characters s, _) -> [XN.mkText $ utf8 s]
	(_, SAX.ParseError text) -> error text
	_ -> []

convertAttr :: SAX.Attribute -> DOM.XmlTree
convertAttr (SAX.Attribute qname' value) = XN.NTree
	(XN.mkAttrNode (convertQName qname'))
	[XN.mkText $ utf8 value]

convertQName :: SAX.QName -> DOM.QName
convertQName (SAX.QName ns _ local) = qname (utf8 ns) (utf8 local)

-------------------------------------------------------------------------------
-- Utility functions for building XML trees
-------------------------------------------------------------------------------

element :: (String, String) -> [(String, String, String)] -> [DOM.XmlTree] -> DOM.XmlTree
element (ns, localpart) attrs children = let
	qname' = qname ns localpart
	attrs' = [attr ans alp text | (ans, alp, text) <- attrs]
	in XN.mkElement qname' attrs' children

attr :: String -> String -> String -> DOM.XmlTree
attr ns localpart text = XN.mkAttr (qname ns localpart) [XN.mkText text]

qname :: String -> String -> DOM.QName
qname ns localpart = case ns of
	"" -> DOM.mkName localpart
	_ -> DOM.mkNsName localpart ns

utf8 :: String -> String
utf8 = T.unpack . TE.decodeUtf8 . C8.pack
