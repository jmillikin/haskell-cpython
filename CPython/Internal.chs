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
{-# LANGUAGE DeriveDataTypeable #-}
module CPython.Internal
	(
	-- * FFI support
	  module Foreign
	, module Foreign.C
	, cToBool
	, cFromBool
	
	-- * Fundamental types
	, SomeObject (..)
	, Type (..)
	, Dictionary (..)
	, List (..)
	, Tuple (..)
	
	-- * Objects
	, Object (..)
	, withObject
	, peekObject
	, peekStaticObject
	, stealObject
	, incref
	, decref
	
	-- * Exceptions
	, Exception (..)
	, exceptionIf
	, checkStatusCode
	, checkBoolReturn
	) where
import Control.Applicative ((<$>))
import qualified Control.Exception as E
import Data.Typeable (Typeable)
import Foreign
import Foreign.C

#include <Python.h>
#include <hscpython-shim.h>

cToBool :: CInt -> Bool
cToBool = (/= 0)

cFromBool :: Bool -> CInt
cFromBool x = if x then 1 else 0

data SomeObject = forall a. (Object a) => SomeObject (ForeignPtr a)

class Object a where
	toObject :: a -> SomeObject
	fromForeignPtr :: ForeignPtr a -> a

instance Object SomeObject where
	toObject = id
	fromForeignPtr = SomeObject

newtype Type = Type (ForeignPtr Type)
instance Object Type where
	toObject (Type x) = SomeObject x
	fromForeignPtr = Type

newtype Dictionary = Dictionary (ForeignPtr Dictionary)
instance Object Dictionary where
	toObject (Dictionary x) = SomeObject x
	fromForeignPtr = Dictionary

newtype List = List (ForeignPtr List)
instance Object List where
	toObject (List x) = SomeObject x
	fromForeignPtr = List

newtype Tuple = Tuple (ForeignPtr Tuple)
instance Object Tuple where
	toObject (Tuple x) = SomeObject x
	fromForeignPtr = Tuple

withObject :: Object obj => obj -> (Ptr a -> IO b) -> IO b
withObject obj io = case toObject obj of
	SomeObject ptr -> withForeignPtr ptr (io . castPtr)

peekObject :: Object obj => Ptr a -> IO obj
peekObject ptr = E.bracketOnError incPtr decref mkObj where
	incPtr = incref ptr >> return ptr
	mkObj _ = fromForeignPtr <$> newForeignPtr staticDecref (castPtr ptr)

peekStaticObject :: Object obj => Ptr a -> IO obj
peekStaticObject ptr = fromForeignPtr <$> newForeignPtr_ (castPtr ptr)

unsafeStealObject :: Object obj => Ptr a -> IO obj
unsafeStealObject ptr = fromForeignPtr <$> newForeignPtr staticDecref (castPtr ptr)

stealObject :: Object obj => Ptr a -> IO obj
stealObject ptr = exceptionIf (ptr == nullPtr) >> unsafeStealObject ptr

{# fun hscpython_Py_INCREF as incref
	{ castPtr `Ptr a'
	} -> `()' id #}

{# fun hscpython_Py_DECREF as decref
	{ castPtr `Ptr a'
	} -> `()' id #}

foreign import ccall "hscpython-shim.h &hscpython_Py_DECREF"
	staticDecref :: FunPtr (Ptr a -> IO ())

data Exception = Exception
	{ exceptionType      :: SomeObject
	, exceptionValue     :: Maybe SomeObject
	, exceptionTraceback :: Maybe SomeObject
	}
	deriving (Typeable)

instance Show Exception where
	show _ = "<CPython Exception>"

instance E.Exception Exception

exceptionIf :: Bool -> IO ()
exceptionIf False = return ()
exceptionIf True =
	alloca $ \pType ->
	alloca $ \pValue ->
	alloca $ \pTrace -> do
		{# call PyErr_Fetch as pyErr_Fetch #} pType pValue pTrace
		eType <- unsafeStealObject pType
		eValue <- maybePeek unsafeStealObject pValue
		eTrace <- maybePeek unsafeStealObject pTrace
		E.throwIO $ Exception eType eValue eTrace

checkStatusCode :: CInt -> IO ()
checkStatusCode = exceptionIf . (== -1)

checkBoolReturn :: CInt -> IO Bool
checkBoolReturn x = do
	exceptionIf $ x == -1
	return $ x /= 0
