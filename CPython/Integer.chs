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
module CPython.Integer
	( Integer
	, integerType
	, check
	, checkExact
	, toInteger
	, fromInteger
	) where
import Prelude hiding (Integer, toInteger, fromInteger)
import qualified Prelude as Prelude
import CPython.Internal
import qualified CPython.Unicode as U
import qualified CPython.Object as O

#include <Python.h>
#include <hscpython-shim.h>

newtype Integer = Integer (ForeignPtr Integer)
instance ObjectClass Integer where
	toObject (Integer x) = Object x
	fromForeignPtr = Integer

{# fun hscpython_PyLong_Type as integerType
	{} -> `Type' peekStaticObject* #}

{# fun hscpython_PyLong_Check as check
	`ObjectClass self ' =>
	{ withObject* `self'
	} -> `Bool' #}

{# fun hscpython_PyLong_CheckExact as checkExact
	`ObjectClass self ' =>
	{ withObject* `self'
	} -> `Bool' #}

toInteger :: Integer -> IO Prelude.Integer
toInteger py = do
	(long, overflow) <- (withObject py $ \pyPtr ->
		alloca $ \overflowPtr -> do
		poke overflowPtr 0
		long <- {# call PyLong_AsLongAndOverflow as ^ #} pyPtr overflowPtr
		overflow <- peek overflowPtr
		return (long, overflow))
	if overflow == 0
		then return $ Prelude.toInteger long
		else fmap read $ U.toString =<< O.string py

fromInteger :: Prelude.Integer -> IO Integer
fromInteger int = do
	let longlong = fromIntegral int
	let [_, min', max'] = [longlong, minBound, maxBound]
	stealObject =<< if Prelude.toInteger min' < int && int < Prelude.toInteger max'
		then {# call PyLong_FromLongLong as ^ #} longlong
		else withCString (show int) $ \cstr ->
			{# call PyLong_FromString as ^ #} cstr nullPtr 10
