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

{-# LANGUAGE TypeFamilies #-}
module Network.Protocol.XMPP.ErrorT
	( ErrorT (..)
	, mapErrorT
	) where

import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import qualified Control.Monad.Error as E
import qualified Control.Monad.Reader as R

-- A custom version of ErrorT, without the 'Error' class restriction.

newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }

instance Functor m => Functor (ErrorT e m) where
	fmap f = ErrorT . fmap (fmap f) . runErrorT

instance Monad m => Monad (ErrorT e m) where
	return = ErrorT . return . Right
	(>>=) m k = ErrorT $ do
		x <- runErrorT m
		case x of
			Left l -> return $ Left l
			Right r -> runErrorT $ k r

instance Monad m => E.MonadError (ErrorT e m) where
	type E.ErrorType (ErrorT e m) = e
	throwError = ErrorT . return . Left
	catchError m h = ErrorT $ do
		x <- runErrorT m
		case x of
			Left l -> runErrorT $ h l
			Right r -> return $ Right r

instance MonadTrans (ErrorT e) where
	lift = ErrorT . liftM Right

instance R.MonadReader m => R.MonadReader (ErrorT e m) where
	type R.EnvType (ErrorT e m) = R.EnvType m
	ask = lift R.ask
	local = mapErrorT . R.local

instance MonadIO m => MonadIO (ErrorT e m) where
	liftIO = lift . liftIO

mapErrorT :: (m (Either e a) -> n (Either e' b))
           -> ErrorT e m a
           -> ErrorT e' n b
mapErrorT f m = ErrorT $ f (runErrorT m)
