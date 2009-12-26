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
module CPython.Types.Capsule
	( Capsule
	, capsuleType
	--, new
	, getPointer
	--, getDestructor
	, getContext
	, getName
	, importNamed
	, isValid
	, setPointer
	--, setDestructor
	, setContext
	--, setName
	) where
import Data.Text (Text)
import CPython.Internal hiding (new)

#include <hscpython-shim.h>

-- type Destructor = Ptr () -> IO ()
newtype Capsule = Capsule (ForeignPtr Capsule)

instance Object Capsule where
	toObject (Capsule x) = SomeObject x
	fromForeignPtr = Capsule

instance Concrete Capsule where
	concreteType _ = capsuleType

{# fun pure hscpython_PyCapsule_Type as capsuleType
	{} -> `Type' peekStaticObject* #}

-- new :: Ptr () -> Maybe Text -> Destructor -> IO Capsule
-- new = undefined

getPointer :: Capsule -> Maybe Text -> IO (Ptr ())
getPointer py name =
	withObject py $ \pyPtr ->
	maybeWith withText name $ \namePtr ->
	{# call PyCapsule_GetPointer as ^ #} pyPtr namePtr

-- getDestructor :: Capsule -> IO (Maybe Destructor)
-- getDestructor = undefined

getContext :: Capsule -> IO (Ptr ())
getContext py =
	withObject py $ \pyPtr -> do
	{# call PyErr_Clear as ^ #}
	ptr <- {# call PyCapsule_GetContext as ^ #} pyPtr
	if ptr /= nullPtr
		then return ptr
		else do
			exc <- {# call PyErr_Occurred as ^ #}
			exceptionIf $ exc /= nullPtr
			return ptr

getName :: Capsule -> IO (Maybe Text)
getName py =
	withObject py $ \pyPtr -> do
	{# call PyErr_Clear as ^ #}
	ptr <- {# call PyCapsule_GetName as ^ #} pyPtr
	if ptr /= nullPtr
		then Just `fmap` peekText ptr
		else do
			exc <- {# call PyErr_Occurred as ^ #}
			exceptionIf $ exc /= nullPtr
			return Nothing

importNamed :: Maybe Text -> Bool -> IO (Maybe (Ptr ()))
importNamed name block =
	maybeWith withText name $ \namePtr ->
	let noBlock = cFromBool (not block) in do
	{# call PyErr_Clear as ^ #}
	ptr <- {# call PyCapsule_Import as ^ #} namePtr noBlock
	if ptr /= nullPtr
		then return $ Just ptr
		else do
			exc <- {# call PyErr_Occurred as ^ #}
			exceptionIf $ exc /= nullPtr
			return Nothing

isValid :: Capsule -> Maybe Text -> IO Bool
isValid py name =
	withObject py $ \pyPtr ->
	maybeWith withText name $ \namePtr ->
	{# call PyCapsule_IsValid as ^ #} pyPtr namePtr
	>>= checkBoolReturn

{# fun PyCapsule_SetPointer as setPointer
	{ withObject* `Capsule'
	, id `Ptr ()'
	} -> `()' checkStatusCode* #}

-- setDestructor :: Capsule -> Maybe Destructor -> IO ()
-- setDestructor = undefined

{# fun PyCapsule_SetContext as setContext
	{ withObject* `Capsule'
	, id `Ptr ()'
	} -> `()' checkStatusCode* #}

-- setName :: Capsule -> Maybe Text -> IO ()
-- setName = undefined
