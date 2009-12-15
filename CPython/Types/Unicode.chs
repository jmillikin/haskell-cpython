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
	, toString
	, fromString
	, fromObject
	, format
	) where
import Data.Char (chr, ord)
import CPython.Internal

#include <Python.h>
#include <hscpython-shim.h>

newtype Unicode = Unicode (ForeignPtr Unicode)
instance Object Unicode where
	toObject (Unicode x) = SomeObject x
	fromForeignPtr = Unicode

{# fun pure hscpython_PyUnicode_Type as unicodeType
	{} -> `Type' peekStaticObject* #}

-- Python can be compiled in either UCS-2 or UCS-4 mode, which will change how
-- the string should be decoded.
data UnicodeMode = UCS2 | UCS4

peekUnicodeMode :: CUChar -> UnicodeMode
peekUnicodeMode 0 = UCS2
peekUnicodeMode 1 = UCS4
peekUnicodeMode x = error $ "Invalid unicode mode: " ++ show x

{# fun pure hscpython_unicode_mode as unicodeMode
	{} -> `UnicodeMode' peekUnicodeMode #}

toString :: Unicode -> IO String
toString obj = withObject obj $ \ptr -> do
	buffer <- {# call hscpython_PyUnicode_AS_UNICODE #} ptr
	size <- {# call hscpython_PyUnicode_GET_SIZE #} ptr
	raw <- peekArray (fromIntegral size) buffer
	case unicodeMode of
		UCS2 -> undefined
		UCS4 -> return $ map (chr . fromIntegral) raw

fromString :: String -> IO Unicode
fromString str = withBuffer fromUnicode >>= stealObject where
	ords = map (fromIntegral . ord) str :: [CUInt]
	withBuffer io = case unicodeMode of
		UCS2 -> undefined
		UCS4 -> withArrayLen ords (\len ptr -> io ptr (fromIntegral len))
	fromUnicode = {# call hscpython_PyUnicode_FromUnicode as fromUnicode' #}

{# fun hscpython_PyUnicode_FromObject as fromObject
	`Object self ' =>
	{ withObject* `self'
	} -> `Unicode' stealObject* #}

{# fun hscpython_PyUnicode_Format as format
	{ withObject* `Unicode'
	, withObject* `Tuple'
	} -> `Unicode' stealObject* #}
