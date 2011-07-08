{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

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

module Network.Protocol.XMPP.JID
	( JID (..)
	, Node (..)
	, Domain (..)
	, Resource (..)
	
	, parseJID
	, parseJID_
	, formatJID
	) where

import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy (Text)
import qualified Data.Text.IDN.StringPrep as SP
import           Data.String (IsString, fromString)

newtype Node = Node { strNode :: Text }
newtype Domain = Domain { strDomain :: Text }
newtype Resource = Resource { strResource :: Text }

instance Show Node where
	showsPrec d (Node x) = showParen (d > 10) $
		showString "Node " . shows x

instance Show Domain where
	showsPrec d (Domain x) = showParen (d > 10) $
		showString "Domain " . shows x

instance Show Resource where
	showsPrec d (Resource x) = showParen (d > 10) $
		showString "Resource " . shows x

instance Eq Node where
	(==) = equaling strNode

instance Eq Domain where
	(==) = equaling strDomain

instance Eq Resource where
	(==) = equaling strResource

data JID = JID
	{ jidNode :: Maybe Node
	, jidDomain :: Domain
	, jidResource :: Maybe Resource
	}
	deriving (Eq)

instance Show JID where
	showsPrec d jid =  showParen (d > 10) $
		showString "JID " . shows (formatJID jid)

instance IsString JID where
	fromString = parseJID_ . fromString

parseJID :: Text -> Maybe JID
parseJID str = maybeJID where
	(node, postNode) = case textSpanBy (/= '@') str of
		(x, y) -> if TL.null y
			then ("", x)
			else (x, TL.drop 1 y)
	(domain, resource) = case textSpanBy (/= '/') postNode of
		(x, y) -> if TL.null y
			then (x, "")
			else (x, TL.drop 1 y)
	nullable x f = if TL.null x then Just Nothing else fmap Just $ f x
	maybeJID = do
		preppedNode <- nullable node $ stringprepM SP.xmppNode
		preppedDomain <- stringprepM SP.nameprep domain
		preppedResource <- nullable resource $ stringprepM SP.xmppResource
		return $ JID
			(fmap Node preppedNode)
			(Domain preppedDomain)
			(fmap Resource preppedResource)
	stringprepM p x = case SP.stringprep p SP.defaultFlags (TL.toStrict x) of
		Left _ -> Nothing
		Right y -> Just (TL.fromStrict y)

parseJID_ :: Text -> JID
parseJID_ text = case parseJID text of
	Just jid -> jid
	Nothing -> error "Malformed JID"

formatJID :: JID -> Text
formatJID (JID node (Domain domain) resource) = formatted where
	formatted = TL.concat [node', domain, resource']
	node' = maybe "" (\(Node x) -> TL.append x "@") node
	resource' = maybe "" (\(Resource x) -> TL.append "/" x) resource

-- Similar to 'comparing'
equaling :: Eq a => (b -> a) -> b -> b -> Bool
equaling f x y = f x == f y

-- multi-version 'text' compatibility
textSpanBy :: (Char -> Bool) -> Text -> (Text, Text)
#if MIN_VERSION_text(0,11,0)
textSpanBy = TL.span
#else
textSpanBy = TL.spanBy
#endif
