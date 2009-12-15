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
module CPython.ByteArray
	( ByteArray
	, byteArrayType
	, toByteString
	, fromByteString
	, fromObject
	, append
	, size
	, resize
	) where
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import CPython.Internal

#include <Python.h>
#include <hscpython-shim.h>

newtype ByteArray = ByteArray (ForeignPtr ByteArray)
instance ObjectClass ByteArray where
	toObject (ByteArray x) = Object x
	fromForeignPtr = ByteArray

{# fun pure hscpython_PyByteArray_Type as byteArrayType
	{} -> `Type' peekStaticObject* #}

toByteString :: ByteArray -> IO B.ByteString
toByteString py =
	withObject py $ \pyPtr -> do
	size' <- {# call PyByteArray_Size as ^ #} pyPtr
	bytes <- {# call PyByteArray_AsString as ^ #} pyPtr
	B.packCStringLen (bytes, fromIntegral size')

fromByteString :: B.ByteString -> IO ByteArray
fromByteString bytes = let
	mkByteArray = {# call PyByteArray_FromStringAndSize as ^ #}
	in B.unsafeUseAsCStringLen bytes $ \(cstr, len) ->
	   stealObject =<< mkByteArray cstr (fromIntegral len)

{# fun PyByteArray_FromObject as fromObject
	`ObjectClass self ' =>
	{ withObject* `self'
	} -> `ByteArray' stealObject* #}

{# fun PyByteArray_Concat as append
	{ withObject* `ByteArray'
	, withObject* `ByteArray'
	} -> `ByteArray' stealObject* #}

{# fun PyByteArray_Size as size
	{ withObject* `ByteArray'
	} -> `Integer' toInteger #}

{# fun PyByteArray_Resize as resize
	{ withObject* `ByteArray'
	, fromIntegral `Integer'
	} -> `()' checkStatusCode* #}
