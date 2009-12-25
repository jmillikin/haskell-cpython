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
	(
	-- * Unicode objects
	  Unicode
	, Encoding
	, ErrorHandling (..)
	, unicodeType
	, toText
	, fromText
	, length
	, fromEncodedObject
	, fromObject
	, encode
	, decode
	
	-- * Methods and slot functions
	, append
	, split
	, splitLines
	, translate
	, join
	, MatchDirection (..)
	, tailMatch
	, FindDirection (..)
	, find
	, count
	, replace
	, compare
	, format
	, contains
	) where
import Prelude hiding (length)
import Control.Exception (ErrorCall (..), throwIO)
import Data.Char (chr, ord)
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import CPython.Internal
import CPython.Types.Bytes (Bytes)

#include <hscpython-shim.h>

newtype Unicode = Unicode (ForeignPtr Unicode)

instance Object Unicode where
	toObject (Unicode x) = SomeObject x
	fromForeignPtr = Unicode

instance Concrete Unicode where
	concreteType _ = unicodeType

type Encoding = T.Text
data ErrorHandling
	= Strict
	| Replace
	| Ignore
	deriving (Show, Eq)

withErrors :: ErrorHandling -> (CString -> IO a) -> IO a
withErrors errors = withCString $ case errors of
	Strict -> "strict"
	Replace -> "replace"
	Ignore -> "ignore"

{# fun pure hscpython_PyUnicode_Type as unicodeType
	{} -> `Type' peekStaticObject* #}

toText :: Unicode -> IO T.Text
toText obj = withObject obj $ \ptr -> do
	buffer <- {# call hscpython_PyUnicode_AsUnicode #} ptr
	size <- {# call hscpython_PyUnicode_GetSize #} ptr
#ifdef Py_UNICODE_WIDE
	raw <- peekArray (fromIntegral size) buffer
	return . T.pack $ map (chr . fromIntegral) raw
#else
	TF.fromPtr (castPtr buffer) (fromIntegral size)
#endif

fromText :: T.Text -> IO Unicode
fromText str = withBuffer fromUnicode >>= stealObject where
	fromUnicode ptr len = let
		len' = fromIntegral len
		ptr' = castPtr ptr
		in {# call hscpython_PyUnicode_FromUnicode #} ptr' len'
#ifdef Py_UNICODE_WIDE
	ords = map (fromIntegral . ord) (T.unpack str) :: [CUInt]
	withBuffer = withArrayLen ords . flip
#else
	withBuffer = TF.useAsPtr str
#endif

{# fun hscpython_PyUnicode_GetSize as length
	{ withObject* `Unicode'
	} -> `Integer' toInteger #}

{# fun hscpython_PyUnicode_FromEncodedObject as fromEncodedObject
	`Object obj' =>
	{ withObject* `obj'
	, withText* `Encoding'
	, withErrors* `ErrorHandling'
	} -> `Unicode' stealObject* #}

{# fun hscpython_PyUnicode_FromObject as fromObject
	`Object self ' =>
	{ withObject* `self'
	} -> `Unicode' stealObject* #}

{# fun hscpython_PyUnicode_AsEncodedString as encode
	{ withObject* `Unicode'
	, withText* `Encoding'
	, withErrors* `ErrorHandling'
	} -> `Bytes' stealObject* #}

decode :: Bytes -> Encoding -> ErrorHandling -> IO Unicode
decode bytes enc errors =
	withObject bytes $ \bytesPtr ->
	withText enc $ \encPtr ->
	withErrors errors $ \errorsPtr ->
	alloca $ \bufferPtr ->
	alloca $ \lenPtr -> do
	cRes <- {# call PyBytes_AsStringAndSize as ^ #} bytesPtr bufferPtr lenPtr
	exceptionIf $ cRes == -1
	buffer <- peek bufferPtr
	len <- peek lenPtr
	{# call hscpython_PyUnicode_Decode #} buffer len encPtr errorsPtr
	>>= stealObject

{# fun hscpython_PyUnicode_Concat as append
	{ withObject* `Unicode'
	, withObject* `Unicode'
	} -> `Unicode' stealObject* #}

split :: Unicode -> Maybe Unicode -> Maybe Integer -> IO List
split = undefined

{# fun hscpython_PyUnicode_Splitlines as splitLines
	{ withObject* `Unicode'
	, `Bool'
	} -> `List' stealObject* #}

{# fun hscpython_PyUnicode_Translate as translate
	`Object table' =>
	{ withObject* `Unicode'
	, withObject* `table'
	, withErrors* `ErrorHandling'
	} -> `Unicode' stealObject* #}

{# fun hscpython_PyUnicode_Join as join
	`Sequence seq' =>
	{ withObject* `Unicode'
	, withObject* `seq'
	} -> `Unicode' stealObject* #}

data MatchDirection = Prefix | Suffix
	deriving (Show, Eq)

tailMatch :: Unicode -> Unicode -> Integer -> Integer -> MatchDirection -> IO Bool
tailMatch str substr start end dir =
	withObject str $ \strPtr ->
	withObject substr $ \substrPtr ->
	let start' = fromInteger start in
	let end' = fromInteger end in
	let dir' = case dir of
		Prefix -> -1
		Suffix -> 1 in
	{# call hscpython_PyUnicode_Tailmatch #} strPtr substrPtr start' end' dir'
	>>= checkBoolReturn

data FindDirection = Forwards | Backwards
	deriving (Show, Eq)

find :: Unicode -> Unicode -> Integer -> Integer -> FindDirection -> IO (Maybe Integer)
find str substr start end dir =
	withObject str $ \strPtr ->
	withObject substr $ \substrPtr -> do
	let start' = fromInteger start
	let end' = fromInteger end
	let dir' = case dir of
		Forwards -> 1
		Backwards -> -1
	cRes <- {# call hscpython_PyUnicode_Find #} strPtr substrPtr start' end' dir'
	exceptionIf $ cRes == -2
	case cRes of
		-1 -> return Nothing
		x | x >= 0 -> return . Just . toInteger $ x
		x -> throwIO . ErrorCall $ "Invalid return code: " ++ show x

count :: Unicode -> Unicode -> Integer -> Integer -> IO Integer
count str substr start end =
	withObject str $ \strPtr ->
	withObject substr $ \substrPtr -> do
	let start' = fromInteger start
	let end' = fromInteger end
	cRes <- {# call hscpython_PyUnicode_Count #} strPtr substrPtr start' end'
	exceptionIf $ cRes == -1
	return $ toInteger cRes

replace :: Unicode -> Unicode -> Unicode -> Maybe Integer -> IO Unicode
replace str substr replstr maxcount =
	withObject str $ \strPtr ->
	withObject substr $ \substrPtr ->
	withObject replstr $ \replstrPtr ->
	let maxcount' = case maxcount of
		Nothing -> -1
		Just x -> fromInteger x in
	{# call hscpython_PyUnicode_Replace #} strPtr substrPtr replstrPtr maxcount'
	>>= stealObject

{# fun hscpython_PyUnicode_Format as format
	{ withObject* `Unicode'
	, withObject* `Tuple'
	} -> `Unicode' stealObject* #}

{# fun hscpython_PyUnicode_Contains as contains
	{ withObject* `Unicode'
	, withObject* `Unicode'
	} -> `Bool' checkBoolReturn* #}
