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
module Network.Protocol.XMPP.JID
	( JID (..)
	, Node (..)
	, Domain (..)
	, Resource (..)
	
	, parseJID
	, parseJID_
	, formatJID
	) where
import qualified Data.Text.Lazy as T
import qualified Data.Text.IDN.StringPrep as SP
import Data.String (IsString, fromString)

newtype Node = Node { strNode :: T.Text }
newtype Domain = Domain { strDomain :: T.Text }
newtype Resource = Resource { strResource :: T.Text }

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

parseJID :: T.Text -> Maybe JID
parseJID str = maybeJID where
	(node, postNode) = case T.spanBy (/= '@') str of
		(x, y) -> if T.null y
			then ("", x)
			else (x, T.drop 1 y)
	(domain, resource) = case T.spanBy (/= '/') postNode of
		(x, y) -> if T.null y
			then (x, "")
			else (x, T.drop 1 y)
	nullable x f = if T.null x then Just Nothing else fmap Just $ f x
	maybeJID = do
		preppedNode <- nullable node $ stringprepM SP.profileNodeprep
		preppedDomain <- stringprepM SP.profileNameprep domain
		preppedResource <- nullable resource $ stringprepM SP.profileResourceprep
		return $ JID
			(fmap Node preppedNode)
			(Domain preppedDomain)
			(fmap Resource preppedResource)
	stringprepM p x = case SP.stringprep p SP.defaultFlags x of
		Left _ -> Nothing
		Right y -> Just y

parseJID_ :: T.Text -> JID
parseJID_ text = case parseJID text of
	Just jid -> jid
	Nothing -> error "Malformed JID"

formatJID :: JID -> T.Text
formatJID (JID node (Domain domain) resource) = formatted where
	formatted = T.concat [node', domain, resource']
	node' = maybe "" (\(Node x) -> T.append x "@") node
	resource' = maybe "" (\(Resource x) -> T.append "/" x) resource

-- Similar to 'comparing'
equaling :: Eq a => (b -> a) -> b -> b -> Bool
equaling f x y = f x == f y
