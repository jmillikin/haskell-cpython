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
module CPython.Dictionary
	( Dictionary
	, dictionaryType
	, new
	, clear
	, contains
	, copy
	, getItem
	, getItemWithError
	, setItem
	, deleteItem
	, items
	, keys
	, values
	, size
	, merge
	, update
	, mergeFromSeq2
	) where
import CPython.Internal hiding (new)

#include <Python.h>
#include <hscpython-shim.h>

{# fun pure hscpython_PyDict_Type as dictionaryType
	{} -> `Type' peekStaticObject* #}

{# fun PyDict_New as new
	{} -> `Dictionary' stealObject* #}

-- newProxy

{# fun PyDict_Clear as clear
	{ withObject* `Dictionary'
	} -> `()' id #}

{# fun PyDict_Contains as contains
	`ObjectClass key' =>
	{ withObject* `Dictionary'
	, withObject* `key'
	} -> `Bool' checkBoolReturn* #}

{# fun PyDict_Copy as copy
	{ withObject* `Dictionary'
	} -> `Dictionary' stealObject* #}

{# fun PyDict_GetItem as getItem
	`ObjectClass key' =>
	{ withObject* `Dictionary'
	, withObject* `key'
	} -> `Object' peekObject* #}

{# fun PyDict_GetItemWithError as getItemWithError
	`ObjectClass key' =>
	{ withObject* `Dictionary'
	, withObject* `key'
	} -> `Object' peekObject* #}

-- getItemString

{# fun PyDict_SetItem as setItem
	`(ObjectClass key, ObjectClass value)' =>
	{ withObject* `Dictionary'
	, withObject* `key'
	, withObject* `value'
	} -> `()' checkStatusCode* #}

-- setItemString

{# fun PyDict_DelItem as deleteItem
	`ObjectClass key' =>
	{ withObject* `Dictionary'
	, withObject* `key'
	} -> `()' checkStatusCode* #}

-- deleteItemString

{# fun PyDict_Items as items
	{ withObject* `Dictionary'
	} -> `List' stealObject* #}

{# fun PyDict_Keys as keys
	{ withObject* `Dictionary'
	} -> `List' stealObject* #}

{# fun PyDict_Values as values
	{ withObject* `Dictionary'
	} -> `List' stealObject* #}

{# fun PyDict_Size as size
	{ withObject* `Dictionary'
	} -> `Integer' fromIntegral #}

-- next

{# fun PyDict_Merge as merge
	`ObjectClass b' =>
	{ withObject* `Dictionary'
	, withObject* `b'
	, `Bool'
	} -> `()' checkStatusCode* #}

{# fun PyDict_Update as update
	`ObjectClass b' =>
	{ withObject* `Dictionary'
	, withObject* `b'
	} -> `()' checkStatusCode* #}

{# fun PyDict_MergeFromSeq2 as mergeFromSeq2
	`ObjectClass seq2' =>
	{ withObject* `Dictionary'
	, withObject* `seq2'
	, `Bool'
	} -> `()' checkStatusCode* #}
