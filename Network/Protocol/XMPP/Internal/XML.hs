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

module Network.Protocol.XMPP.Internal.XML
	( getTree
	, putTree
	, readEventsUntil
	, convertQName
	, element
	, attr
	, qname
	) where
import qualified Network.Protocol.XMPP.Internal.Handle as H
import qualified Data.ByteString.Char8 as C8

-- XML Parsing
import Text.XML.HXT.Arrow ((>>>))
import qualified Text.XML.HXT.Arrow as A
import qualified Text.XML.HXT.DOM.Interface as DOM
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Text.XML.LibXML.SAX as SAX

getTree :: H.Handle -> SAX.Parser -> IO DOM.XmlTree
getTree h p = eventsToTree `fmap` readEventsUntil finished h p where
	finished 0 (SAX.EndElement _) = True
	finished _ _ = False

putTree :: H.Handle -> DOM.XmlTree -> IO ()
putTree h t = do
	let root = XN.mkRoot [] [t]
	[text] <- A.runX (A.constA root >>> A.writeDocumentToString [
		(A.a_no_xml_pi, "1")
		])
	H.hPutBytes h $ C8.pack text

-------------------------------------------------------------------------------

readEventsUntil :: (Int -> SAX.Event -> Bool) -> H.Handle -> SAX.Parser -> IO [SAX.Event]
readEventsUntil done h parser = readEventsUntil' done 0 [] $ do
	char <- H.hGetChar h
	SAX.parse parser [char] False

readEventsUntil' :: (Int -> SAX.Event -> Bool) -> Int -> [SAX.Event] -> IO [SAX.Event] -> IO [SAX.Event]
readEventsUntil' done depth accum getEvents = do
	events <- getEvents
	let (done', depth', accum') = readEventsStep done events depth accum
	if done'
		then return accum'
		else readEventsUntil' done depth' accum' getEvents

readEventsStep :: (Int -> SAX.Event -> Bool) -> [SAX.Event] -> Int -> [SAX.Event] -> (Bool, Int, [SAX.Event])
readEventsStep _ [] depth accum = (False, depth, accum)
readEventsStep done (e:es) depth accum = let
	depth' = depth + case e of
		(SAX.BeginElement _ _) -> 1
		(SAX.EndElement _) -> (- 1)
		_ -> 0
	accum' = accum ++ [e]
	in if done depth' e then (True, depth', accum')
	else readEventsStep done es depth' accum'

-------------------------------------------------------------------------------
-- For converting incremental XML event lists to HXT trees
-------------------------------------------------------------------------------

-- This function assumes the input list is valid. No validation is performed.
eventsToTree :: [SAX.Event] -> DOM.XmlTree
eventsToTree es = XN.mkRoot [] (eventsToTrees es)

eventsToTrees :: [SAX.Event] -> [DOM.XmlTree]
eventsToTrees es = concatMap blockToTrees (splitBlocks es)

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
	(SAX.Characters s, _) -> [XN.mkText s]
	(_, SAX.ParseError text) -> error text
	_ -> []

convertAttr :: SAX.Attribute -> DOM.XmlTree
convertAttr (SAX.Attribute qname' value) = XN.NTree
	(XN.mkAttrNode (convertQName qname'))
	[XN.mkText value]

convertQName :: SAX.QName -> DOM.QName
convertQName (SAX.QName ns _ local) = qname ns local

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
