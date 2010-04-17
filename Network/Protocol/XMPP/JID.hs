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
import qualified Data.Text as T
import qualified Text.StringPrep as SP
import Text.NamePrep (namePrepProfile)
import Data.Ranges (single)
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
	(==) = equaling (SP.runStringPrep nodePrep . strNode)

instance Eq Domain where
	(==) = equaling (SP.runStringPrep domainPrep . strDomain)

instance Eq Resource where
	(==) = equaling (SP.runStringPrep resourcePrep . strResource)

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
	mNode = if T.null node then Nothing else Just (Node node)
	mResource = if T.null resource then Nothing else Just (Resource resource)
	maybeJID = do
		SP.runStringPrep nodePrep node
		SP.runStringPrep domainPrep domain
		SP.runStringPrep resourcePrep resource
		Just $ JID mNode (Domain domain) mResource

parseJID_ :: T.Text -> JID
parseJID_ text = case parseJID text of
	Just jid -> jid
	Nothing -> error "Malformed JID"

formatJID :: JID -> T.Text
formatJID (JID node (Domain domain) resource) = formatted where
	formatted = T.concat [node', domain, resource']
	node' = maybe "" (\(Node x) -> T.append x "@") node
	resource' = maybe "" (\(Resource x) -> T.append "/" x) resource

nodePrep :: SP.StringPrepProfile
nodePrep = SP.Profile
	{ SP.maps = [SP.b1, SP.b2]
	, SP.shouldNormalize = True
	, SP.prohibited = [ SP.c11, SP.c12, SP.c21, SP.c22
	                  , SP.c3, SP.c4, SP.c5, SP.c6, SP.c7, SP.c8, SP.c9
	                  , map single "\"&'/:<>@"
	                  ]
	, SP.shouldCheckBidi = True
	}

domainPrep :: SP.StringPrepProfile
domainPrep = namePrepProfile False

resourcePrep :: SP.StringPrepProfile
resourcePrep = SP.Profile
	{ SP.maps = [SP.b1]
	, SP.shouldNormalize = True
	, SP.prohibited = [ SP.c12, SP.c21, SP.c22
	                  , SP.c3, SP.c4, SP.c5, SP.c6, SP.c7, SP.c8, SP.c9]
	, SP.shouldCheckBidi = True
	}

-- Similar to 'comparing'
equaling :: Eq a => (b -> a) -> b -> b -> Bool
equaling f x y = f x == f y
