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
	, new
	, pack
	, size
	, getItem
	, getSlice
	, setItem
	) where
import CPython.Internal hiding (new)

#include <hscpython-shim.h>

instance Concrete Tuple where
	concreteType _ = tupleType

{# fun pure hscpython_PyTuple_Type as tupleType
	{} -> `Type' peekStaticObject* #}

{# fun PyTuple_New as new
	{ fromIntegral `Integer'
	} -> `Tuple' stealObject* #}

pack :: [SomeObject] -> IO Tuple
pack xs = do
	tuple <- new . toInteger $ length xs
	setItems tuple 0 xs
	return tuple

setItems :: Tuple -> Integer -> [SomeObject] -> IO ()
setItems _ _ [] = return ()
setItems t idx (x:xs) = setItem t idx x >> setItems t (idx + 1) xs

{# fun PyTuple_Size as size
	{ withObject* `Tuple'
	} -> `Integer' toInteger #}

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
	cRes <- {# call PyTuple_SetItem as ^ #} selfPtr (fromIntegral index) xPtr
	exceptionIf $ cRes /= 0
