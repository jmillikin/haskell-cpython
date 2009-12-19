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
module CPython.System
	( getObject
	, setObject
	, resetWarnOptions
	, addWarnOption
	, setPath
	) where
import CPython.Internal

#include <Python.h>

{# pointer *wchar_t as CWString nocode #}

getObject :: String -> IO (Maybe SomeObject)
getObject name =
	withCString name $ \cstr -> do
	raw <- {# call PySys_GetObject as ^ #} cstr
	maybePeek peekObject raw

-- getFile

setObject :: Object a => String -> Maybe a -> IO ()
setObject name v =
	maybeWith withObject v $ \vPtr ->
	withCString name $ \cstr ->
	{# call PySys_SetObject as ^ #} cstr vPtr
	>>= checkStatusCode

{# fun PySys_ResetWarnOptions as resetWarnOptions
	{} -> `()' id #}

addWarnOption :: String -> IO ()
addWarnOption str =
	withCWString str $ \cwstr ->
	{# call PySys_AddWarnOption as ^ #} cwstr

{# fun PySys_SetPath as setPath
	{ withCWString* `String'
	} -> `()' id #}
