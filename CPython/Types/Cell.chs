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
module CPython.Types.Cell
	( Cell
	, cellType
	, new
	, get
	, set
	) where
import CPython.Internal hiding (new)

#include <hscpython-shim.h>

newtype Cell = Cell (ForeignPtr Cell)
instance Object Cell where
	toObject (Cell x) = SomeObject x
	fromForeignPtr = Cell

{# fun pure hscpython_PyCell_Type as cellType
	{} -> `Type' peekStaticObject* #}

new :: Object obj => Maybe obj -> IO Cell
new obj =
	maybeWith withObject obj $ \objPtr ->
	{# call PyCell_New as ^ #} objPtr
	>>= stealObject

get :: Cell -> IO (Maybe SomeObject)
get cell =
	withObject cell $ \cellPtr ->
	{# call PyCell_Get as ^ #} cellPtr
	>>= maybePeek stealObject

set :: Object obj => Cell -> Maybe obj -> IO ()
set cell obj =
	withObject cell $ \cellPtr ->
	maybeWith withObject obj $ \objPtr ->
	{# call PyCell_Set as ^ #} cellPtr objPtr
	>>= checkStatusCode
