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
	, Concrete
	, SomeObject
	
	-- * Types and casting
	, getType
	, isInstance
	, isSubclass
	, toObject
	, cast
	
	-- * Attributes
	, hasAttribute
	, getAttribute
	, setAttribute
	, deleteAttribute
	
	-- * Display and debugging
	, print
	, repr
	, ascii
	, string
	, bytes
	
	-- * Callables
	, callable
	, call
	, callArgs
	, callMethod
	, callMethodArgs
	
	-- * Misc
	, Comparison (..)
	, richCompare
	, toBool
	, hash
	, dir
	, getIterator
	) where
import Prelude hiding (Ordering (..), print)
import qualified Data.Text as T
import System.IO (Handle, hPutStrLn)
import CPython.Internal hiding (toBool)
import qualified CPython.Types.Bytes as B
import qualified CPython.Types.Dictionary as D
import qualified CPython.Types.Tuple as Tuple
import qualified CPython.Types.Unicode as U

#include <hscpython-shim.h>

{# fun PyObject_Type as getType
	`Object self' =>
	{ withObject* `self'
	} -> `Type' stealObject* #}

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

cast :: (Object a, Concrete b) => a -> IO (Maybe b)
cast obj = do
	let castObj = case toObject obj of
		SomeObject ptr -> fromForeignPtr $ castForeignPtr ptr
	validCast <- isInstance obj $ concreteType castObj
	return $ if validCast
		then Just castObj
		else Nothing

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

print :: Object self => self -> Handle -> IO ()
print obj h = repr obj >>= U.fromUnicode >>= (hPutStrLn h . T.unpack)

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

{# fun PyCallable_Check as callable
	`Object self' =>
	{ withObject* `self'
	} -> `Bool' checkBoolReturn* #}

call :: Object self => self -> Tuple -> Dictionary -> IO SomeObject
call self args kwargs =
	withObject self $ \selfPtr ->
	withObject args $ \argsPtr ->
	withObject kwargs $ \kwargsPtr ->
	{# call PyObject_Call as ^ #} selfPtr argsPtr kwargsPtr
	>>= stealObject

callArgs :: Object self => self -> [SomeObject] -> IO SomeObject
callArgs self args = do
	args' <- Tuple.toTuple args
	D.new >>= call self args'

callMethod :: Object self => self -> T.Text -> Tuple -> Dictionary -> IO SomeObject
callMethod self name args kwargs = do
	method <- getAttribute self =<< U.toUnicode name
	call method args kwargs

callMethodArgs :: Object self => self -> T.Text -> [SomeObject] -> IO SomeObject
callMethodArgs self name args = do
	args' <- Tuple.toTuple args
	D.new >>= callMethod self name args'

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

{# fun PyObject_IsTrue as toBool
	`Object self' =>
	{ withObject* `self'
	} -> `Bool' checkBoolReturn* #}

hash :: Object self => self -> IO Integer
hash self = withObject self $ \ptr -> do
	cRes <- {# call PyObject_Hash as ^ #} ptr
	exceptionIf $ cRes == -1
	return $ toInteger cRes

{# fun PyObject_Dir as dir
	`Object self' =>
	{ withObject* `self'
	} -> `List' stealObject* #}

{# fun PyObject_GetIter as getIterator
	`Object self' =>
	{ withObject* `self'
	} -> `SomeObject' stealObject* #}
