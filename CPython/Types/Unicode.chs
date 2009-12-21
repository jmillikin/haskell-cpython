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
module CPython.Types.Unicode
	( Unicode
	, unicodeType
	, toText
	, fromText
	, fromObject
	, format
	) where
import Data.Char (chr, ord)
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import CPython.Internal

#include <Python.h>
#include <hscpython-shim.h>

newtype Unicode = Unicode (ForeignPtr Unicode)
instance Object Unicode where
	toObject (Unicode x) = SomeObject x
	fromForeignPtr = Unicode

{# fun pure hscpython_PyUnicode_Type as unicodeType
	{} -> `Type' peekStaticObject* #}

toText :: Unicode -> IO T.Text
toText obj = withObject obj $ \ptr -> do
	buffer <- {# call hscpython_PyUnicode_AsUnicode #} ptr
	size' <- {# call hscpython_PyUnicode_GetSize #} ptr
#ifdef Py_UNICODE_WIDE
	raw <- peekArray (fromIntegral size) buffer
	return . T.pack $ map (chr . fromIntegral) raw
#else
	TF.fromPtr (castPtr buffer) (fromIntegral size')
#endif

fromText :: T.Text -> IO Unicode
fromText str = withBuffer fromUnicode >>= stealObject where
	fromUnicode ptr len = let
		len' = fromIntegral len
		ptr' = castPtr ptr
		in {# call hscpython_PyUnicode_FromUnicode #} ptr' len'
#ifdef Py_UNICODE_WIDE
	ords = map (fromIntegral . ord) str :: [CUInt]
	withBuffer = withArrayLen ords . flip
#else
	withBuffer = TF.useAsPtr str
#endif

{# fun hscpython_PyUnicode_FromObject as fromObject
	`Object self ' =>
	{ withObject* `self'
	} -> `Unicode' stealObject* #}

{# fun hscpython_PyUnicode_Format as format
	{ withObject* `Unicode'
	, withObject* `Tuple'
	} -> `Unicode' stealObject* #}
