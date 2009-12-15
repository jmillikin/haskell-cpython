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

/* Types */
PyTypeObject *hscpython_PyType_Type ();
PyTypeObject *hscpython_PyTuple_Type ();
PyTypeObject *hscpython_PyList_Type ();
PyTypeObject *hscpython_PyDict_Type ();
PyTypeObject *hscpython_PyLong_Type ();
PyTypeObject *hscpython_PyFloat_Type ();
PyTypeObject *hscpython_PyComplex_Type ();
PyTypeObject *hscpython_PyUnicode_Type ();
PyTypeObject *hscpython_PyBytes_Type ();

/* Constants */
PyObject *hscpython_Py_None ();
PyObject *hscpython_Py_True ();
PyObject *hscpython_Py_False ();

/* Unicode */
unsigned char hscpython_unicode_mode ();
Py_ssize_t hscpython_PyUnicode_GET_SIZE (PyObject *);
Py_UNICODE *hscpython_PyUnicode_AS_UNICODE (PyObject *);
PyObject *hscpython_PyUnicode_FromUnicode (Py_UNICODE *, Py_ssize_t);
PyObject *hscpython_PyUnicode_FromObject (PyObject *);
PyObject *hscpython_PyUnicode_Format (PyObject *, PyObject *);
