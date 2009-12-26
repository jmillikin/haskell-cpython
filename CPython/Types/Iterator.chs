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
module CPython.Types.Iterator
	( Iterator
	, SequenceIterator
	, sequenceIteratorType
	, sequenceIteratorNew
	
	, CallableIterator
	, callableIteratorType
	, callableIteratorNew
	
	, next
	) where
import CPython.Internal
import CPython.Protocols.Sequence (Sequence)

#include <hscpython-shim.h>

class Object a => Iterator a

newtype SequenceIterator = SequenceIterator (ForeignPtr SequenceIterator)

instance Iterator SequenceIterator
instance Object SequenceIterator where
	toObject (SequenceIterator x) = SomeObject x
	fromForeignPtr = SequenceIterator

instance Concrete SequenceIterator where
	concreteType _ = sequenceIteratorType

newtype CallableIterator = CallableIterator (ForeignPtr CallableIterator)

instance Iterator CallableIterator
instance Object CallableIterator where
	toObject (CallableIterator x) = SomeObject x
	fromForeignPtr = CallableIterator

instance Concrete CallableIterator where
	concreteType _ = callableIteratorType

{# fun pure hscpython_PySeqIter_Type as sequenceIteratorType
	{} -> `Type' peekStaticObject* #}

{# fun pure hscpython_PyCallIter_Type as callableIteratorType
	{} -> `Type' peekStaticObject* #}

-- | Return an iterator that works with a general sequence object, /seq/.
-- The iteration ends when the sequence raises @IndexError@ for the
-- subscripting operation.
-- 
{# fun PySeqIter_New as sequenceIteratorNew
	`Sequence seq' =>
	{ withObject* `seq'
	} -> `SequenceIterator' stealObject* #}

-- | Return a new iterator. The first parameter, /callable/, can be any
-- Python callable object that can be called with no parameters; each call
-- to it should return the next item in the iteration. When /callable/
-- returns a value equal to /sentinel/, the iteration will be terminated.
-- 
{# fun PyCallIter_New as callableIteratorNew
	`(Object callable, Object sentinel)' =>
	{ withObject* `callable'
	, withObject* `sentinel'
	} -> `CallableIterator' stealObject* #}

-- | Return the next value from the iteration, or 'Nothing' if there are no
-- remaining items.
-- 
next :: Iterator iter => iter -> IO (Maybe SomeObject)
next iter =
	withObject iter $ \iterPtr -> do
	raw <- {# call PyIter_Next as ^ #} iterPtr
	if raw == nullPtr
		then do
			err <- {# call PyErr_Occurred as ^ #}
			exceptionIf $ err /= nullPtr
			return Nothing
		else fmap Just $ stealObject raw
