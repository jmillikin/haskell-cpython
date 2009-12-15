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

/* Tuple */
PyTypeObject *hscpython_PyTuple_Type ()
{ return &PyTuple_Type; }

int hscpython_PyTuple_Check (PyObject *o)
{ return PyTuple_Check (o); }

int hscpython_PyTuple_CheckExact (PyObject *o)
{ return PyTuple_CheckExact (o); }

/* Type */
PyTypeObject *hscpython_PyType_Type ()
{ return &PyType_Type; }

int hscpython_PyType_Check (PyObject *o)
{ return PyType_Check (o); }

int hscpython_PyType_CheckExact (PyObject *o)
{ return PyType_CheckExact (o); }

/* Unicode */
unsigned char hscpython_unicode_mode ()
{
#ifdef Py_UNICODE_WIDE
	return 1;
#else
	return 0;
#endif
}

PyTypeObject *hscpython_PyUnicode_Type ()
{ return &PyUnicode_Type; }

int hscpython_PyUnicode_Check (PyObject *o)
{ return PyUnicode_Check (o); }

int hscpython_PyUnicode_CheckExact (PyObject *o)
{ return PyUnicode_CheckExact (o); }

Py_ssize_t hscpython_PyUnicode_GET_SIZE (PyObject *o)
{ return PyUnicode_GET_SIZE (o); }

Py_UNICODE *hscpython_PyUnicode_AS_UNICODE (PyObject *o)
{ return PyUnicode_AS_UNICODE (o); }

PyObject *hscpython_PyUnicode_FromUnicode (Py_UNICODE *u, Py_ssize_t size)
{ return PyUnicode_FromUnicode (u, size); }

PyObject *hscpython_PyUnicode_FromObject (PyObject *o)
{ return PyUnicode_FromObject (o); }

PyObject *hscpython_PyUnicode_Format (PyObject *format, PyObject *args)
{ return PyUnicode_Format (format, args); }
