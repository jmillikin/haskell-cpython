/**
 * Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
**/

#include <hscpython-shim.h>

/* Object */
void hscpython_Py_INCREF (PyObject *o)
{ Py_INCREF (o); }

void hscpython_Py_DECREF (PyObject *o)
{ Py_DECREF (o); }

int hscpython_PyObject_DelAttr(PyObject *o, PyObject *name)
{ return PyObject_DelAttr (o, name); }

int hscpython_PyObject_TypeCheck (PyObject *o, PyTypeObject *type)
{ return PyObject_TypeCheck (o, type); }

/* Types */
PyTypeObject *hscpython_PyType_Type ()
{ return &PyType_Type; }

PyTypeObject *hscpython_PyTuple_Type ()
{ return &PyTuple_Type; }

PyTypeObject *hscpython_PyList_Type ()
{ return &PyList_Type; }

PyTypeObject *hscpython_PyDict_Type ()
{ return &PyDict_Type; }

PyTypeObject *hscpython_PyLong_Type ()
{ return &PyLong_Type; }

PyTypeObject *hscpython_PyFloat_Type ()
{ return &PyFloat_Type; }

PyTypeObject *hscpython_PyComplex_Type ()
{ return &PyComplex_Type; }

PyTypeObject *hscpython_PyUnicode_Type ()
{ return &PyUnicode_Type; }

PyTypeObject *hscpython_PyBytes_Type ()
{ return &PyBytes_Type; }

PyTypeObject *hscpython_PyByteArray_Type ()
{ return &PyByteArray_Type; }

PyTypeObject *hscpython_PyCell_Type ()
{ return &PyCell_Type; }

PyTypeObject *hscpython_PyCode_Type ()
{ return &PyCode_Type; }

PyTypeObject *hscpython_PyFunction_Type ()
{ return &PyFunction_Type; }

PyTypeObject *hscpython_PyInstanceMethod_Type ()
{ return &PyInstanceMethod_Type; }

PyTypeObject *hscpython_PyMethod_Type ()
{ return &PyMethod_Type; }

PyTypeObject *hscpython_PySet_Type ()
{ return &PySet_Type; }

PyTypeObject *hscpython_PyFrozenSet_Type ()
{ return &PyFrozenSet_Type; }

PyTypeObject *hscpython_PySeqIter_Type ()
{ return &PySeqIter_Type; }

PyTypeObject *hscpython_PyCallIter_Type ()
{ return &PyCallIter_Type; }

PyTypeObject *hscpython_PySlice_Type ()
{ return &PySlice_Type; }

PyTypeObject *hscpython_PyModule_Type ()
{ return &PyModule_Type; }

/* Constants */
PyObject *hscpython_Py_None ()
{ return Py_None; }

PyObject *hscpython_Py_True ()
{ return Py_True; }

PyObject *hscpython_Py_False ()
{ return Py_False; }

/* Unicode */
Py_ssize_t hscpython_PyUnicode_GetSize (PyObject *o)
{ return PyUnicode_GetSize (o); }

Py_UNICODE *hscpython_PyUnicode_AsUnicode (PyObject *o)
{ return PyUnicode_AsUnicode (o); }

PyObject *hscpython_PyUnicode_FromUnicode (Py_UNICODE *u, Py_ssize_t size)
{ return PyUnicode_FromUnicode (u, size); }

PyObject *hscpython_PyUnicode_FromEncodedObject (PyObject *o, const char *enc, const char *err)
{ return PyUnicode_FromEncodedObject (o, enc, err); }

PyObject *hscpython_PyUnicode_AsEncodedString (PyObject *o, const char *enc, const char *err)
{ return PyUnicode_AsEncodedString (o, enc, err); }

PyObject *hscpython_PyUnicode_FromObject (PyObject *o)
{ return PyUnicode_FromObject (o); }

PyObject *hscpython_PyUnicode_Decode (const char *s, Py_ssize_t len, const char *enc, const char *err)
{ return PyUnicode_Decode (s, len, enc, err); }

PyObject *hscpython_PyUnicode_Concat (PyObject *l, PyObject *r)
{ return PyUnicode_Concat (l, r); }

PyObject *hscpython_PyUnicode_Split (PyObject *s, PyObject *sep, Py_ssize_t max)
{ return PyUnicode_Split (s, sep, max); }

PyObject *hscpython_PyUnicode_Splitlines (PyObject *s, int keep)
{ return PyUnicode_Splitlines (s, keep); }

PyObject *hscpython_PyUnicode_Translate (PyObject *str, PyObject *table, const char *err)
{ return PyUnicode_Translate (str, table, err); }

PyObject *hscpython_PyUnicode_Join (PyObject *sep, PyObject *seq)
{ return PyUnicode_Join (sep, seq); }

int hscpython_PyUnicode_Tailmatch (PyObject *str, PyObject *substr, Py_ssize_t start, Py_ssize_t end, int dir)
{ return PyUnicode_Tailmatch (str, substr, start, end, dir); }

Py_ssize_t hscpython_PyUnicode_Find (PyObject *str, PyObject *substr, Py_ssize_t start, Py_ssize_t end, int dir)
{ return PyUnicode_Find (str, substr, start, end, dir); }

Py_ssize_t hscpython_PyUnicode_Count (PyObject *str, PyObject *substr, Py_ssize_t start, Py_ssize_t end)
{ return PyUnicode_Count (str, substr, start, end); }

PyObject *hscpython_PyUnicode_Replace (PyObject *str, PyObject *substr, PyObject *replstr, Py_ssize_t max)
{ return PyUnicode_Replace (str, substr, replstr, max); }

PyObject *hscpython_PyUnicode_Format (PyObject *format, PyObject *args)
{ return PyUnicode_Format (format, args); }

int hscpython_PyUnicode_Contains (PyObject *a, PyObject *b)
{ return PyUnicode_Contains (a, b); }
