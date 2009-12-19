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
module CPython.Types.Module
	( Module
	, moduleType
	, new
	, getDictionary
	, getName
	, getFilename
	, addObject
	, addIntegerConstant
	, addStringConstant
	, importModule
	, reload
	) where
import Prelude hiding (fromInteger)
import CPython.Internal hiding (new)
import CPython.Types.Integer (fromInteger)
import CPython.Types.Unicode (fromString)

#include <Python.h>
#include <hscpython-shim.h>

newtype Module = Module (ForeignPtr Module)
instance Object Module where
	toObject (Module x) = SomeObject x
	fromForeignPtr = Module

{# fun pure hscpython_PyModule_Type as moduleType
	{} -> `Type' peekStaticObject* #}

{# fun PyModule_New as new
	{ withCString* `String'
	} -> `Module' stealObject* #}

{# fun PyModule_GetDict as getDictionary
	{ withObject* `Module'
	} -> `Dictionary' peekObject* #}

{# fun PyModule_GetName as getName
	{ withObject* `Module'
	} -> `String' peekCString* #}

{# fun PyModule_GetFilename as getFilename
	{ withObject* `Module'
	} -> `String' peekCString* #}

{# fun PyModule_AddObject as addObject
	`Object value' =>
	{ withObject* `Module'
	, withCString* `String'
	, withObject* `value'
	} -> `()' checkStatusCode* #}

addIntegerConstant :: Module -> String -> Integer -> IO ()
addIntegerConstant m name value = fromInteger value >>= addObject m name

addStringConstant :: Module -> String -> String -> IO ()
addStringConstant m name value = fromString value >>= addObject m name

importModule :: String -> IO Module
importModule name = do
	pyName <- fromString name
	withObject pyName $ \namePtr ->
		{# call PyImport_Import as ^ #} namePtr
		>>= stealObject

{# fun PyImport_ReloadModule as reload
	{ withObject* `Module'
	} -> `Module' stealObject* #}
