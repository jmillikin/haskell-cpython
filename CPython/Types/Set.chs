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
module CPython.Types.Set
	( Set
	, FrozenSet
	, setType
	, frozenSetType
	, newSet
	, newFrozenSet
	, size
	, contains
	, add
	, discard
	, pop
	, clear
	) where
import CPython.Internal

#include <Python.h>
#include <hscpython-shim.h>

class Object a => AnySet a

newtype Set = Set (ForeignPtr Set)
instance Object Set where
	toObject (Set x) = SomeObject x
	fromForeignPtr = Set

newtype FrozenSet = FrozenSet (ForeignPtr FrozenSet)
instance Object FrozenSet where
	toObject (FrozenSet x) = SomeObject x
	fromForeignPtr = FrozenSet

instance AnySet Set
instance AnySet FrozenSet

{# fun pure hscpython_PySet_Type as setType
	{} -> `Type' peekStaticObject* #}

{# fun pure hscpython_PyFrozenSet_Type as frozenSetType
	{} -> `Type' peekStaticObject* #}

newSet :: Object obj => Maybe obj -> IO Set
newSet obj =
	maybeWith withObject obj $ \objPtr ->
	{# call PySet_New as ^ #} objPtr
	>>= stealObject

newFrozenSet :: Object obj => Maybe obj -> IO FrozenSet
newFrozenSet obj =
	maybeWith withObject obj $ \objPtr ->
	{# call PyFrozenSet_New as ^ #} objPtr
	>>= stealObject

size :: AnySet set => set -> IO Integer
size set =
	withObject set $ \setPtr -> do
	cRes <- {# call PySet_Size as ^ #} setPtr
	exceptionIf $ cRes == -1
	return $ toInteger cRes

{# fun PySet_Contains as contains
	`(AnySet set, Object key)' =>
	{ withObject* `set'
	, withObject* `key'
	} -> `Bool' checkBoolReturn* #}

{# fun PySet_Add as add
	`(AnySet set, Object key)' =>
	{ withObject* `set'
	, withObject* `key'
	} -> `()' checkStatusCode* #}

{# fun PySet_Discard as discard
	`Object key' =>
	{ withObject* `Set'
	, withObject* `key'
	} -> `Bool' checkBoolReturn* #}

{# fun PySet_Pop as pop
	{ withObject* `Set'
	} -> `SomeObject' stealObject* #}

{# fun PySet_Clear as clear
	{ withObject* `Set'
	} -> `()' checkStatusCode* #}
