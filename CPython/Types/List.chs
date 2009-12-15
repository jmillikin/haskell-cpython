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
	, new
	, pack
	, size
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
import Prelude hiding (reverse)
import CPython.Internal hiding (new)

#include <Python.h>
#include <hscpython-shim.h>

{# fun pure hscpython_PyList_Type as listType
	{} -> `Type' peekStaticObject* #}

{# fun PyList_New as new
	{ fromIntegral `Integer'
	} -> `List' stealObject* #}

pack :: [SomeObject] -> IO List
pack xs = do
	list <- new . toInteger $ length xs
	setItems list 0 xs
	return list

setItems :: List -> Integer -> [SomeObject] -> IO ()
setItems _ _ [] = return ()
setItems l idx (x:xs) = setItem l idx x >> setItems l idx xs

{# fun PyList_Size as size
	{ withObject* `List'
	} -> `Integer' toInteger #}

{# fun PyList_GetItem as getItem
	{ withObject* `List'
	, fromIntegral `Integer'
	} -> `SomeObject' peekObject* #}

setItem :: Object o => List -> Integer -> o -> IO ()
setItem self index x =
	withObject self $ \selfPtr ->
	withObject x $ \xPtr -> do
	incref xPtr
	cRes <- {# call PyList_SetItem as ^ #} selfPtr (fromIntegral index) xPtr
	checkStatusCode cRes

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
	cRes <- {# call PyList_SetSlice as ^ #} selfPtr low' high' itemsPtr
	checkStatusCode cRes

{# fun PyList_Sort as sort
	{ withObject* `List'
	} -> `()' checkStatusCode* #}

{# fun PyList_Reverse as reverse
	{ withObject* `List'
	} -> `()' checkStatusCode* #}

{# fun PyList_AsTuple as toTuple
	{ withObject* `List'
	} -> `Tuple' stealObject* #}
