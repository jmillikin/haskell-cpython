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
{-# LANGUAGE ExistentialQuantification #-}
module CPython.Protocols.Sequence
	( Sequence (..)
	, SomeSequence
	, castToSequence
	, size
	, append
	, repeat
	, inPlaceAppend
	, inPlaceRepeat
	, getItem
	, setItem
	, deleteItem
	, getSlice
	, setSlice
	, deleteSlice
	, count
	, contains
	, index
	, toList
	, toTuple
	, fast
	) where
import Prelude hiding (repeat)
import CPython.Internal
import CPython.Types.ByteArray (ByteArray)
import CPython.Types.Bytes (Bytes)
import CPython.Types.Unicode (Unicode)

#include <Python.h>
#include <hscpython-shim.h>

data SomeSequence = forall a. (Sequence a) => SomeSequence (ForeignPtr a)

class Object a => Sequence a where
	toSequence :: a -> SomeSequence

instance Object SomeSequence where
	toObject (SomeSequence x) = SomeObject x
	fromForeignPtr = SomeSequence

instance Sequence SomeSequence where
	toSequence = id

instance Sequence ByteArray where
	toSequence = unsafeCastToSequence

instance Sequence Bytes where
	toSequence = unsafeCastToSequence

instance Sequence List where
	toSequence = unsafeCastToSequence

instance Sequence Tuple where
	toSequence = unsafeCastToSequence

instance Sequence Unicode where
	toSequence = unsafeCastToSequence

unsafeCastToSequence :: Object a => a -> SomeSequence
unsafeCastToSequence x = case toObject x of
	SomeObject ptr -> let
		ptr' = castForeignPtr ptr :: ForeignPtr SomeSequence
		in SomeSequence ptr'

castToSequence :: Object a => a -> IO (Maybe SomeSequence)
castToSequence obj =
	withObject obj $ \objPtr -> do
	isSequence <- fmap cToBool $ {# call PySequence_Check as ^ #} objPtr
	return $ if isSequence
		then Just $ unsafeCastToSequence obj
		else Nothing

{# fun PySequence_Size as size
	`Sequence self' =>
	{ withObject* `self'
	} -> `Integer' fromIntegral #}

{# fun PySequence_Concat as append
	`(Sequence a, Sequence b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeSequence' stealObject* #}

{# fun PySequence_Repeat as repeat
	`Sequence a' =>
	{ withObject* `a'
	, fromIntegral `Integer'
	} -> `a' stealObject* #}

{# fun PySequence_InPlaceConcat as inPlaceAppend
	`(Sequence a, Sequence b)' =>
	{ withObject* `a'
	, withObject* `b'
	} -> `SomeSequence' stealObject* #}

{# fun PySequence_InPlaceRepeat as inPlaceRepeat
	`Sequence a' =>
	{ withObject* `a'
	, fromIntegral `Integer'
	} -> `a' stealObject* #}

{# fun PySequence_GetItem as getItem
	`Sequence self' =>
	{ withObject* `self'
	, fromIntegral `Integer'
	} -> `SomeObject' stealObject* #}

{# fun PySequence_SetItem as setItem
	`(Sequence self, Object v)' =>
	{ withObject* `self'
	, fromIntegral `Integer'
	, withObject* `v'
	} -> `()' checkStatusCode* #}

{# fun PySequence_DelItem as deleteItem
	`Sequence self' =>
	{ withObject* `self'
	, fromIntegral `Integer'
	} -> `()' checkStatusCode* #}

{# fun PySequence_GetSlice as getSlice
	`Sequence self' =>
	{ withObject* `self'
	, fromIntegral `Integer'
	, fromIntegral `Integer'
	} -> `SomeObject' stealObject* #}

{# fun PySequence_SetSlice as setSlice
	`(Sequence self, Object v)' =>
	{ withObject* `self'
	, fromIntegral `Integer'
	, fromIntegral `Integer'
	, withObject* `v'
	} -> `()' checkStatusCode* #}

{# fun PySequence_DelSlice as deleteSlice
	`Sequence self' =>
	{ withObject* `self'
	, fromIntegral `Integer'
	, fromIntegral `Integer'
	} -> `()' checkStatusCode* #}

{# fun PySequence_Count as count
	`(Sequence self, Object v)' =>
	{ withObject* `self'
	, withObject* `v'
	} -> `Integer' toInteger #}

{# fun PySequence_Contains as contains
	`(Sequence self, Object v)' =>
	{ withObject* `self'
	, withObject* `v'
	} -> `Bool' checkBoolReturn* #}

index :: (Sequence self, Object v) => self -> v -> IO Integer
index self v =
	withObject self $ \selfPtr ->
	withObject v $ \vPtr -> do
	cRes <- {# call PySequence_Index as ^ #} selfPtr vPtr
	exceptionIf $ cRes == -1
	return $ toInteger cRes

{# fun PySequence_List as toList
	`Sequence self' =>
	{ withObject* `self'
	} -> `List' stealObject* #}

{# fun PySequence_Tuple as toTuple
	`Sequence self' =>
	{ withObject* `self'
	} -> `Tuple' stealObject* #}

{# fun PySequence_Fast as fast
	`Sequence self' =>
	{ withObject* `self'
	, withCString* `String'
	} -> `SomeSequence' stealObject* #}
