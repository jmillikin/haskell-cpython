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
module CPython.Type
	( Type
	, typeType
	, check
	, checkExact
	, isSubtype
	) where
import CPython.Internal

#include <Python.h>
#include <hscpython-shim.h>

{# fun hscpython_PyType_Type as typeType
	{} -> `Type' peekStaticObject* #}

{# fun hscpython_PyType_Check as check
	`ObjectClass self ' =>
	{ withObject* `self'
	} -> `Bool' #}

{# fun hscpython_PyType_CheckExact as checkExact
	`ObjectClass self ' =>
	{ withObject* `self'
	} -> `Bool' #}

{# fun PyType_IsSubtype as isSubtype
	{ withObject* `Type'
	, withObject* `Type'
	} -> `Bool' #}
