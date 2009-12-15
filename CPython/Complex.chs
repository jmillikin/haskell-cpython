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
module CPython.Complex
	( Complex
	, complexType
	, toComplex
	, fromComplex
	) where
import qualified Data.Complex as C
import CPython.Internal

#include <Python.h>
#include <hscpython-shim.h>

newtype Complex = Complex (ForeignPtr Complex)
instance ObjectClass Complex where
	toObject (Complex x) = Object x
	fromForeignPtr = Complex

{# fun pure hscpython_PyComplex_Type as complexType
	{} -> `Type' peekStaticObject* #}

toComplex :: Complex -> IO (C.Complex Double)
toComplex py = withObject py $ \pyPtr -> do
	real <- {# call PyComplex_RealAsDouble as ^ #} pyPtr
	imag <- {# call PyComplex_ImagAsDouble as ^ #} pyPtr
	return $ realToFrac real C.:+ realToFrac imag

fromComplex :: C.Complex Double -> IO Complex
fromComplex x = raw >>= stealObject where
	real = realToFrac $ C.realPart x
	imag = realToFrac $ C.imagPart x
	raw = {# call PyComplex_FromDoubles as ^ #} real imag
