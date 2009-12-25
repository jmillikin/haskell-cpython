-- Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
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
-- 
{-# LANGUAGE ForeignFunctionInterface #-}
module CPython.Types.Tuple
	( Tuple
	, tupleType
	, toTuple
	, iterableToTuple
	, fromTuple
	, length
	, getItem
	, getSlice
	, setItem
	) where
import Prelude hiding (length)
import qualified Prelude as Prelude
import CPython.Internal hiding (new)

#include <hscpython-shim.h>

instance Concrete Tuple where
	concreteType _ = tupleType

{# fun pure hscpython_PyTuple_Type as tupleType
	{} -> `Type' peekStaticObject* #}

toTuple :: [SomeObject] -> IO Tuple
toTuple xs =
	mapWith withObject xs $ \ptrs ->
	withArrayLen ptrs $ \count array ->
	{# call hscpython_poke_tuple #} (fromIntegral count) array
	>>= stealObject

iterableToTuple :: Object iter => iter -> IO Tuple
iterableToTuple iter = do
	raw <- callObjectRaw tupleType =<< toTuple [toObject iter]
	return $ unsafeCast raw

fromTuple :: Tuple -> IO [SomeObject]
fromTuple py =
	withObject py $ \pyPtr ->
	({# call PyTuple_Size as ^ #} pyPtr >>=) $ \size ->
	let size' = fromIntegral size :: Int in
	withArray (replicate size' nullPtr) $ \ptrs ->
	{# call hscpython_peek_tuple #} pyPtr size ptrs >>
	peekArray size' ptrs >>= mapM peekObject

{# fun PyTuple_Size as length
	{ withObject* `Tuple'
	} -> `Integer' checkIntReturn* #}

{# fun PyTuple_GetItem as getItem
	{ withObject* `Tuple'
	, fromIntegral `Integer'
	} -> `SomeObject' peekObject* #}

{# fun PyTuple_GetSlice as getSlice
	{ withObject* `Tuple'
	, fromIntegral `Integer'
	, fromIntegral `Integer'
	} -> `Tuple' stealObject* #}

setItem :: Object o => Tuple -> Integer -> o -> IO ()
setItem self index x =
	withObject self $ \selfPtr ->
	withObject x $ \xPtr -> do
	incref xPtr
	{# call PyTuple_SetItem as ^ #} selfPtr (fromIntegral index) xPtr
	>>= checkStatusCode
