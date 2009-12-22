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
module CPython.Types.Function
	( Function
	, functionType
	, new
	, getCode
	, getGlobals
	, getModule
	, getDefaults
	, setDefaults
	, getClosure
	, setClosure
	, getAnnotations
	, setAnnotations
	) where
import CPython.Internal hiding (new)
import CPython.Types.Code (Code)
import qualified CPython.Constants as Const

#include <hscpython-shim.h>

newtype Function = Function (ForeignPtr Function)
instance Object Function where
	toObject (Function x) = SomeObject x
	fromForeignPtr = Function

{# fun pure hscpython_PyFunction_Type as functionType
	{} -> `Type' peekStaticObject* #}

{# fun PyFunction_New as new
	{ withObject* `Code'
	, withObject* `Dictionary'
	} -> `Function' stealObject* #}

{# fun PyFunction_GetCode as getCode
	{ withObject* `Function'
	} -> `Code' peekObject* #}

{# fun PyFunction_GetGlobals as getGlobals
	{ withObject* `Function'
	} -> `Dictionary' peekObject* #}

{# fun PyFunction_GetModule as getModule
	{ withObject* `Function'
	} -> `SomeObject' peekObject* #}

withNullableObject :: Object obj => Maybe obj -> (Ptr a -> IO b) -> IO b
withNullableObject Nothing io = do
	none <- Const.none
	withObject none io
withNullableObject (Just obj) io = withObject obj io

peekNullableObject :: Object obj => Ptr a -> IO (Maybe obj)
peekNullableObject = maybePeek peekObject

{# fun PyFunction_GetDefaults as getDefaults
	{ withObject* `Function'
	} -> `Maybe Tuple' peekNullableObject* #}

{# fun PyFunction_SetDefaults as setDefaults
	{ withObject* `Function'
	, withNullableObject* `Maybe Tuple'
	} -> `()' checkStatusCode* #}

{# fun PyFunction_GetClosure as getClosure
	{ withObject* `Function'
	} -> `Maybe Tuple' peekNullableObject* #}

{# fun PyFunction_SetClosure as setClosure
	{ withObject* `Function'
	, withNullableObject* `Maybe Tuple'
	} -> `()' checkStatusCode* #}

{# fun PyFunction_GetAnnotations as getAnnotations
	{ withObject* `Function'
	} -> `Maybe Dictionary' peekNullableObject* #}

{# fun PyFunction_SetAnnotations as setAnnotations
	{ withObject* `Function'
	, withNullableObject* `Maybe Dictionary'
	} -> `()' checkStatusCode* #}
