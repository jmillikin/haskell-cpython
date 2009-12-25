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
module CPython.Types.List
	( List
	, listType
	, toList
	, fromList
	, length
	, getItem
	, setItem
	, insert
	, append
	, getSlice
	, setSlice
	, sort
	, reverse
	, toTuple
	) where
import Prelude hiding (reverse, length)
import qualified Prelude as Prelude
import CPython.Internal hiding (new)

#include <hscpython-shim.h>

instance Concrete List where
	concreteType _ = listType

{# fun pure hscpython_PyList_Type as listType
	{} -> `Type' peekStaticObject* #}

toList :: [SomeObject] -> IO List
toList xs =
	mapWith withObject xs $ \ptrs ->
	withArrayLen ptrs $ \count array ->
	{# call hscpython_poke_list #} (fromIntegral count) array
	>>= stealObject

fromList :: List -> IO [SomeObject]
fromList py =
	withObject py $ \pyPtr ->
	({# call PyList_Size as ^ #} pyPtr >>=) $ \size ->
	let size' = fromIntegral size :: Int in
	withArray (replicate size' nullPtr) $ \ptrs ->
	{# call hscpython_peek_list #} pyPtr size ptrs >>
	peekArray size' ptrs >>= mapM peekObject

{# fun PyList_Size as length
	{ withObject* `List'
	} -> `Integer' checkIntReturn* #}

{# fun PyList_GetItem as getItem
	{ withObject* `List'
	, fromIntegral `Integer'
	} -> `SomeObject' peekObject* #}

setItem :: Object o => List -> Integer -> o -> IO ()
setItem self index x =
	withObject self $ \selfPtr ->
	withObject x $ \xPtr -> do
	incref xPtr
	{# call PyList_SetItem as ^ #} selfPtr (fromIntegral index) xPtr
	>>= checkStatusCode

{# fun PyList_Insert as insert
	`Object item' =>
	{ withObject* `List'
	, fromIntegral `Integer'
	, withObject* `item'
	} -> `()' checkStatusCode* #}

{# fun PyList_Append as append
	`Object item' =>
	{ withObject* `List'
	, withObject* `item'
	} -> `()' checkStatusCode* #}

{# fun PyList_GetSlice as getSlice
	{ withObject* `List'
	, fromIntegral `Integer'
	, fromIntegral `Integer'
	} -> `List' stealObject* #}

setSlice :: List -> Integer -> Integer -> Maybe List -> IO ()
setSlice self low high items = let
	low' = fromIntegral low
	high' = fromIntegral high in
	withObject self $ \selfPtr ->
	maybeWith withObject items $ \itemsPtr -> do
	{# call PyList_SetSlice as ^ #} selfPtr low' high' itemsPtr
	>>= checkStatusCode

{# fun PyList_Sort as sort
	{ withObject* `List'
	} -> `()' checkStatusCode* #}

{# fun PyList_Reverse as reverse
	{ withObject* `List'
	} -> `()' checkStatusCode* #}

{# fun PyList_AsTuple as toTuple
	{ withObject* `List'
	} -> `Tuple' stealObject* #}
