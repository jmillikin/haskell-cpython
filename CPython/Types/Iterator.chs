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
	( SequenceIterator
	, sequenceIteratorType
	, sequenceIteratorNew
	
	, CallableIterator
	, callableIteratorType
	, callableIteratorNew
	) where
import CPython.Internal
import CPython.Protocols.Sequence (Sequence)

#include <hscpython-shim.h>

newtype SequenceIterator = SequenceIterator (ForeignPtr SequenceIterator)

instance Object SequenceIterator where
	toObject (SequenceIterator x) = SomeObject x
	fromForeignPtr = SequenceIterator

instance Concrete SequenceIterator where
	concreteType _ = sequenceIteratorType

newtype CallableIterator = CallableIterator (ForeignPtr CallableIterator)

instance Object CallableIterator where
	toObject (CallableIterator x) = SomeObject x
	fromForeignPtr = CallableIterator

instance Concrete CallableIterator where
	concreteType _ = callableIteratorType

{# fun pure hscpython_PySeqIter_Type as sequenceIteratorType
	{} -> `Type' peekStaticObject* #}

{# fun pure hscpython_PyCallIter_Type as callableIteratorType
	{} -> `Type' peekStaticObject* #}

{# fun PySeqIter_New as sequenceIteratorNew
	`Sequence seq' =>
	{ withObject* `seq'
	} -> `SequenceIterator' stealObject* #}

{# fun PyCallIter_New as callableIteratorNew
	`(Object callable, Object sentinel)' =>
	{ withObject* `callable'
	, withObject* `sentinel'
	} -> `CallableIterator' stealObject* #}
