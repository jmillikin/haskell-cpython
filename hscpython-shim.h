#include <Python.h>

/* Object */
void hscpython_Py_INCREF (PyObject *);
void hscpython_Py_DECREF (PyObject *);
int hscpython_PyObject_DelAttr(PyObject *, PyObject *);
int hscpython_PyObject_TypeCheck (PyObject *, PyTypeObject *);

enum HSCPythonComparisonEnum
{ HSCPYTHON_LT = Py_LT
, HSCPYTHON_LE = Py_LE
, HSCPYTHON_EQ = Py_EQ
, HSCPYTHON_NE = Py_NE
, HSCPYTHON_GT = Py_GT
, HSCPYTHON_GE = Py_GE
};

/* Tuple */
PyTypeObject *hscpython_PyTuple_Type ();
int hscpython_PyTuple_Check (PyObject *);
int hscpython_PyTuple_CheckExact (PyObject *);

/* Long */
PyTypeObject *hscpython_PyLong_Type ();
int hscpython_PyLong_Check (PyObject *);
int hscpython_PyLong_CheckExact (PyObject *);

/* Float */
PyTypeObject *hscpython_PyFloat_Type ();
int hscpython_PyFloat_Check (PyObject *);
int hscpython_PyFloat_CheckExact (PyObject *);

/* Type */
PyTypeObject *hscpython_PyType_Type ();
int hscpython_PyType_Check (PyObject *);
int hscpython_PyType_CheckExact (PyObject *);

/* Unicode */
unsigned char hscpython_unicode_mode ();
PyTypeObject *hscpython_PyUnicode_Type ();
int hscpython_PyUnicode_Check (PyObject *);
int hscpython_PyUnicode_CheckExact (PyObject *);
Py_ssize_t hscpython_PyUnicode_GET_SIZE (PyObject *);
Py_UNICODE *hscpython_PyUnicode_AS_UNICODE (PyObject *);
PyObject *hscpython_PyUnicode_FromUnicode (Py_UNICODE *, Py_ssize_t);
PyObject *hscpython_PyUnicode_FromObject (PyObject *);
PyObject *hscpython_PyUnicode_Format (PyObject *, PyObject *);
