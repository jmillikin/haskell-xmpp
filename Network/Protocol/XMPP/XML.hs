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
module Network.Protocol.XMPP.XML
	( module Data.XML.Types
	
	-- * Constructors
	, name
	, nsname
	, element
	, nselement
	
	-- * Misc
	, getattr
	, contentText
	, attributeText
	, escape
	, serialiseElement
	, readEvents
	
	-- * libxml-sax-0.4 API imitation
	, Parser
	, SaxEvent (..)
	, newParser
	, parse
	, eventsToElement
	
	) where
import Control.Monad (when)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import Data.XML.Types
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Text.XML.LibXML.SAX as SAX

getattr :: Name -> Element -> Maybe T.Text
getattr n e = case elementAttributes e >>= isNamed n of
	[] -> Nothing
	attr:_ -> Just $ attributeText attr

contentText :: Content -> T.Text
contentText (ContentText t) = t
contentText (ContentEntity e) = T.concat ["&", e, ";"]

attributeText :: Attribute -> T.Text
attributeText = T.concat . map contentText . attributeContent

name :: T.Text -> Name
name t = Name t Nothing Nothing

nsname :: T.Text -> T.Text -> Name
nsname ns n = Name n (Just ns) Nothing

escape :: T.Text -> T.Text
escape = T.concatMap escapeChar where
	escapeChar c = case c of
		'&' -> "&amp;"
		'<' -> "&lt;"
		'>' -> "&gt;"
		'"' -> "&quot;"
		'\'' -> "&apos;"
		_ -> T.singleton c

escapeContent :: Content -> T.Text
escapeContent (ContentText t) = escape t
escapeContent (ContentEntity e) = T.concat ["&", escape e, ";"]

element :: T.Text -> [(T.Text, T.Text)] -> [Node] -> Element
element elemName attrs children = Element (name elemName) attrs' children where
	attrs' = map (uncurry mkattr) attrs

nselement :: T.Text -> T.Text -> [(T.Text, T.Text)] -> [Node] -> Element
nselement ns ln attrs children = Element (nsname ns ln) attrs' children where
	attrs' = map (uncurry mkattr) attrs

mkattr :: T.Text -> T.Text -> Attribute
mkattr n val = Attribute (name n) [ContentText val]

-- A somewhat primitive serialisation function
--
-- TODO: better namespace / prefix handling
serialiseElement :: Element -> T.Text
serialiseElement e = text where
	text = T.concat ["<", eName, " ", attrs, ">", contents, "</", eName, ">"]
	eName = formatName $ elementName e
	formatName = escape . nameLocalName
	attrs = T.intercalate " " $ map attr $ elementAttributes e ++ nsattr
	attr (Attribute n c) = T.concat $ [formatName n, "=\""] ++ map escapeContent c ++ ["\""]
	nsattr = case nameNamespace $ elementName e of
		Nothing -> []
		Just ns -> [mkattr "xmlns" ns]
	contents = T.concat $ map serialiseNode $ elementNodes e
	
	serialiseNode (NodeElement e') = serialiseElement e'
	serialiseNode (NodeContent c) = escape (contentText c)
	serialiseNode (NodeComment _) = ""
	serialiseNode (NodeInstruction _) = ""

-- quick-and-dirty imitation of libxml-sax-0.4 API; later, this should
-- probably be rewritten to use ST and discard the list parsing
data Parser = Parser (SAX.Parser IO) (IORef (Either T.Text [SaxEvent]))

newParser :: IO Parser
newParser = do
	let toLazy t = T.fromChunks [t]
	
	ref <- newIORef (Right [])
	p <- SAX.newParserIO (\err -> writeIORef ref (Left $ toLazy err)) Nothing
	
	let addEvent e = do
		x <- readIORef ref
		case x of
			Left _ -> return ()
			Right es -> writeIORef ref (Right (e:es))
		return True
	
	SAX.setCallback p SAX.parsedBeginElement (\name' attrs -> addEvent $ BeginElement name' attrs)
	SAX.setCallback p SAX.parsedEndElement (\name' -> addEvent $ EndElement name')
	SAX.setCallback p SAX.parsedCharacters (\txt -> addEvent $ Characters $ toLazy txt)
	SAX.setCallback p SAX.parsedComment (\txt -> addEvent $ Comment $ toLazy txt)
	SAX.setCallback p SAX.parsedInstruction (\i -> addEvent $ ProcessingInstruction i)
	
	return $ Parser p ref

parse :: Parser -> B.ByteString -> Bool -> IO (Either T.Text [SaxEvent])
parse (Parser p ref) bytes finish = do
	writeIORef ref (Right [])
	SAX.parseLazyBytes p bytes
	when finish $ SAX.parseComplete p
	eitherEvents <- readIORef ref
	return $ case eitherEvents of
		Left err -> Left err
		Right events -> Right $ reverse events

data SaxEvent
	= BeginElement Name [Attribute]
	| EndElement Name
	| Characters T.Text
	| Comment T.Text
	| ProcessingInstruction Instruction

readEvents :: Monad m
           => (Integer -> SaxEvent -> Bool)
           -> m [SaxEvent]
           -> m [SaxEvent]
readEvents done nextEvents = readEvents' 0 [] where
	readEvents' depth acc = do
		events <- nextEvents
		let (done', depth', acc') = step events depth acc
		if done'
			then return acc'
			else readEvents' depth' acc'
	
	step [] depth acc = (False, depth, acc)
	step (e:es) depth acc = let
		depth' = depth + case e of
			(BeginElement _ _) -> 1
			(EndElement _) -> (- 1)
			_ -> 0
		acc' = e : acc
		in if done depth' e
			then (True, depth', reverse acc')
			else step es depth' acc'

-- | Convert a list of events to a single 'X.Element'. If the events do not
-- contain at least one valid element, 'Nothing' will be returned instead.
eventsToElement :: [SaxEvent] -> Maybe Element
eventsToElement es = case eventsToNodes es >>= isElement of
	(e:_) -> Just e
	_ -> Nothing

eventsToNodes :: [SaxEvent] -> [Node]
eventsToNodes = concatMap blockToNodes . splitBlocks

-- Split event list into a sequence of "blocks", which are the events including
-- and between a pair of tags. <start><start2/></start> and <start/> are both
-- single blocks.
splitBlocks :: [SaxEvent] -> [[SaxEvent]]
splitBlocks es = ret where
	(_, _, ret) = foldl splitBlocks' (0, [], []) es
	
	splitBlocks' (depth, accum, allAccum) e = split where
		split = if depth' == 0
			then (depth', [], allAccum ++ [accum'])
			else (depth', accum', allAccum)
		accum' = accum ++ [e]
		depth' :: Integer
		depth' = depth + case e of
			(BeginElement _ _) -> 1
			(EndElement _) -> (- 1)
			_ -> 0

blockToNodes :: [SaxEvent] -> [Node]
blockToNodes [] = []
blockToNodes (begin:rest) = nodes where
	end = last rest
	nodes = case (begin, end) of
		(BeginElement name' attrs, EndElement _) -> [node name' attrs]
		(Characters t, _) -> [NodeContent (ContentText t)]
		_ -> []
	
	node n as = NodeElement $ Element n as $ eventsToNodes $ init rest
