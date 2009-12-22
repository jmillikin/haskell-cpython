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
module CPython.Protocols.Object
	( Object
	, SomeObject
	, toObject
	, hasAttribute
	, getAttribute
	, setAttribute
	, deleteAttribute
	, Comparison (..)
	, richCompare
	, repr
	, ascii
	, string
	, bytes
	, isInstance
	, isSubclass
	, callable
	, call
	, callObject
	, callMethod
	, hash
	, isTrue
	, not
	, getType
	, typeCheck
	, size
	, getItem
	, setItem
	, deleteItem
	, dir
	, getIterator
	) where
import Prelude hiding (Ordering (..), compare, not)
import CPython.Internal
import qualified CPython.Types.Unicode as U
import qualified CPython.Types.Bytes as B

#include <hscpython-shim.h>

{# fun PyObject_HasAttr as hasAttribute
	`Object self' =>
	{ withObject* `self'
	, withObject* `U.Unicode'
	} -> `Bool' #}

{# fun PyObject_GetAttr as getAttribute
	`Object self' =>
	{ withObject* `self'
	, withObject* `U.Unicode'
	} -> `SomeObject' stealObject* #}

{# fun PyObject_SetAttr as setAttribute
	`(Object self, Object v)' =>
	{ withObject* `self'
	, withObject* `U.Unicode'
	, withObject* `v'
	} -> `()' checkStatusCode* #}

{# fun hscpython_PyObject_DelAttr as deleteAttribute
	`Object self' =>
	{ withObject* `self'
	, withObject* `U.Unicode'
	} -> `()' checkStatusCode* #}

data Comparison = LT | LE | EQ | NE | GT | GE
	deriving (Show)

{# enum HSCPythonComparisonEnum {} #}

comparisonToInt :: Comparison -> CInt
comparisonToInt = fromIntegral . fromEnum . enum where
	enum LT = HSCPYTHON_LT
	enum LE = HSCPYTHON_LE
	enum EQ = HSCPYTHON_EQ
	enum NE = HSCPYTHON_NE
	enum GT = HSCPYTHON_GT
	enum GE = HSCPYTHON_GE

{# fun PyObject_RichCompareBool as richCompare
	`(Object a, Object b)' =>
	{ withObject* `a'
	, withObject* `b'
	, comparisonToInt `Comparison'
	} -> `Bool' checkBoolReturn* #}

{# fun PyObject_Repr as repr
	`Object self' =>
	{ withObject* `self'
	} -> `U.Unicode' stealObject* #}

{# fun PyObject_ASCII as ascii
	`Object self' =>
	{ withObject* `self'
	} -> `U.Unicode' stealObject* #}

{# fun PyObject_Str as string
	`Object self' =>
	{ withObject* `self'
	} -> `U.Unicode' stealObject* #}

{# fun PyObject_Bytes as bytes
	`Object self' =>
	{ withObject* `self'
	} -> `B.Bytes' stealObject* #}

{# fun PyObject_IsInstance as isInstance
	`(Object self, Object cls)' =>
	{ withObject* `self'
	, withObject* `cls'
	} -> `Bool' checkBoolReturn* #}

{# fun PyObject_IsSubclass as isSubclass
	`(Object derived, Object cls)' =>
	{ withObject* `derived'
	, withObject* `cls'
	} -> `Bool' checkBoolReturn* #}

{# fun PyCallable_Check as callable
	`Object self' =>
	{ withObject* `self'
	} -> `Bool' checkBoolReturn* #}

call :: Object self => self -> Tuple -> Maybe Dictionary -> IO SomeObject
call self args kwargs =
	withObject self $ \selfPtr ->
	withObject args $ \argsPtr ->
	maybeWith withObject kwargs $ \kwargsPtr ->
	{# call PyObject_Call as ^ #} selfPtr argsPtr kwargsPtr
	>>= stealObject

callObject :: Object self => self -> Maybe Tuple -> IO SomeObject
callObject self args =
	withObject self $ \selfPtr ->
	maybeWith withObject args $ \argsPtr ->
	{# call PyObject_CallObject as ^ #} selfPtr argsPtr
	>>= stealObject

callMethod :: Object self => self -> U.Unicode -> Tuple -> Maybe Dictionary -> IO SomeObject
callMethod self name args kwargs = do
	method <- getAttribute self name
	call method args kwargs

hash :: Object self => self -> IO Integer
hash self = withObject self $ \ptr -> do
	cRes <- {# call PyObject_Hash as ^ #} ptr
	exceptionIf $ cRes == -1
	return $ toInteger cRes

{# fun PyObject_IsTrue as isTrue
	`Object self' =>
	{ withObject* `self'
	} -> `Bool' checkBoolReturn* #}

{# fun PyObject_Not as not
	`Object self' =>
	{ withObject* `self'
	} -> `Bool' checkBoolReturn* #}

{# fun PyObject_Type as getType
	`Object self' =>
	{ withObject* `self'
	} -> `Type' stealObject* #}

{# fun hscpython_PyObject_TypeCheck as typeCheck
	`Object self' =>
	{ withObject* `self'
	, withObject* `Type'
	} -> `Bool' #}

size :: Object self => self -> IO Integer
size self = withObject self $ \ptr -> do
	cRes <- {# call PyObject_Size as ^ #} ptr
	exceptionIf $ cRes == -1
	return $ toInteger cRes

{# fun PyObject_GetItem as getItem
	`(Object self, Object key)' =>
	{ withObject* `self'
	, withObject* `key'
	} -> `SomeObject' stealObject* #}

{# fun PyObject_SetItem as setItem
	`(Object self, Object key, Object value)' =>
	{ withObject* `self'
	, withObject* `key'
	, withObject* `value'
	} -> `()' checkStatusCode* #}

{# fun PyObject_DelItem as deleteItem
	`(Object self, Object key)' =>
	{ withObject* `self'
	, withObject* `key'
	} -> `()' checkStatusCode* #}

{# fun PyObject_Dir as dir
	`Object self' =>
	{ withObject* `self'
	} -> `List' stealObject* #}

{# fun PyObject_GetIter as getIterator
	`Object self' =>
	{ withObject* `self'
	} -> `SomeObject' stealObject* #}
