typedef signed char int8_t;
typedef unsigned char uint8_t;
typedef signed short int16_t;
typedef unsigned short uint16_t;
typedef signed int int32_t;
typedef unsigned int uint32_t;
typedef signed long long int64_t;
typedef unsigned long long uint64_t;
typedef signed long long intptr_t;
typedef unsigned long long uintptr_t;
typedef unsigned long long size_t;
typedef signed long long Py_ssize_t;

typedef uint16_t wchar_t;

typedef int pid_t;
typedef uintptr_t Py_uintptr_t;
typedef intptr_t Py_intptr_t;
typedef Py_ssize_t Py_hash_t;
typedef size_t Py_uhash_t;
typedef Py_ssize_t Py_ssize_clean_t;
__declspec(dllimport) void * PyMem_Malloc(size_t size);
__declspec(dllimport) void * PyMem_Calloc(size_t nelem, size_t elsize);
__declspec(dllimport) void * PyMem_Realloc(void *ptr, size_t new_size);
__declspec(dllimport) void PyMem_Free(void *ptr);
typedef struct PyModuleDef PyModuleDef;
typedef struct PyModuleDef_Slot PyModuleDef_Slot;
typedef struct PyMethodDef PyMethodDef;
typedef struct PyGetSetDef PyGetSetDef;
typedef struct PyMemberDef PyMemberDef;
typedef struct _object PyObject;
typedef struct _longobject PyLongObject;
typedef struct _typeobject PyTypeObject;
typedef struct PyCodeObject PyCodeObject;
typedef struct _frame PyFrameObject;
typedef struct _ts PyThreadState;
typedef struct _is PyInterpreterState;
typedef struct {
    void *buf;
    PyObject *obj;
    Py_ssize_t len;
    Py_ssize_t itemsize;
    int readonly;
    int ndim;
    char *format;
    Py_ssize_t *shape;
    Py_ssize_t *strides;
    Py_ssize_t *suboffsets;
    void *internal;
} Py_buffer;
typedef int (*getbufferproc)(PyObject *, Py_buffer *, int);
typedef void (*releasebufferproc)(PyObject *, Py_buffer *);
__declspec(dllimport) int PyObject_CheckBuffer(PyObject *obj);
__declspec(dllimport) int PyObject_GetBuffer(PyObject *obj, Py_buffer *view,
                                   int flags);
__declspec(dllimport) void * PyBuffer_GetPointer(const Py_buffer *view, const Py_ssize_t *indices);
__declspec(dllimport) Py_ssize_t PyBuffer_SizeFromFormat(const char *format);
__declspec(dllimport) int PyBuffer_ToContiguous(void *buf, const Py_buffer *view,
                                      Py_ssize_t len, char order);
__declspec(dllimport) int PyBuffer_FromContiguous(const Py_buffer *view, const void *buf,
                                        Py_ssize_t len, char order);
__declspec(dllimport) int PyObject_CopyData(PyObject *dest, PyObject *src);
__declspec(dllimport) int PyBuffer_IsContiguous(const Py_buffer *view, char fort);
__declspec(dllimport) void PyBuffer_FillContiguousStrides(int ndims,
                                               Py_ssize_t *shape,
                                               Py_ssize_t *strides,
                                               int itemsize,
                                               char fort);
__declspec(dllimport) int PyBuffer_FillInfo(Py_buffer *view, PyObject *o, void *buf,
                                  Py_ssize_t len, int readonly,
                                  int flags);
__declspec(dllimport) void PyBuffer_Release(Py_buffer *view);
struct _object {
    union {
       Py_ssize_t ob_refcnt;
       uint32_t ob_refcnt_split[2];
    };
    PyTypeObject *ob_type;
};
typedef struct {
    PyObject ob_base;
    Py_ssize_t ob_size;
} PyVarObject;
__declspec(dllimport) int Py_Is(PyObject *x, PyObject *y);
static inline Py_ssize_t Py_REFCNT(PyObject *ob) {
    return ob->ob_refcnt;
}
static inline PyTypeObject* Py_TYPE(PyObject *ob) {
    return ob->ob_type;
}
extern __declspec(dllimport) PyTypeObject PyLong_Type;
extern __declspec(dllimport) PyTypeObject PyBool_Type;
static inline Py_ssize_t Py_SIZE(PyObject *ob) {
    (void)( (!!(ob->ob_type != &PyLong_Type)) || (_wassert(L"ob->ob_type != &PyLong_Type", L"D:/miniforge3/include/object.h", (unsigned)(231)), 0) );
    (void)( (!!(ob->ob_type != &PyBool_Type)) || (_wassert(L"ob->ob_type != &PyBool_Type", L"D:/miniforge3/include/object.h", (unsigned)(232)), 0) );
    return ((PyVarObject*)((ob)))->ob_size;
}
static inline __attribute__((always_inline)) int _Py_IsImmortal(PyObject *op)
{
    return ((int32_t)(op->ob_refcnt)) < 0;
}
static inline int Py_IS_TYPE(PyObject *ob, PyTypeObject *type) {
    return Py_TYPE(ob) == type;
}
static inline void Py_SET_REFCNT(PyObject *ob, Py_ssize_t refcnt) {
    if (_Py_IsImmortal(((PyObject*)((ob))))) {
        return;
    }
    ob->ob_refcnt = refcnt;
}
static inline void Py_SET_TYPE(PyObject *ob, PyTypeObject *type) {
    ob->ob_type = type;
}
static inline void Py_SET_SIZE(PyVarObject *ob, Py_ssize_t size) {
    (void)( (!!(ob->ob_base.ob_type != &PyLong_Type)) || (_wassert(L"ob->ob_base.ob_type != &PyLong_Type", L"D:/miniforge3/include/object.h", (unsigned)(280)), 0) );
    (void)( (!!(ob->ob_base.ob_type != &PyBool_Type)) || (_wassert(L"ob->ob_base.ob_type != &PyBool_Type", L"D:/miniforge3/include/object.h", (unsigned)(281)), 0) );
    ob->ob_size = size;
}
typedef PyObject * (*unaryfunc)(PyObject *);
typedef PyObject * (*binaryfunc)(PyObject *, PyObject *);
typedef PyObject * (*ternaryfunc)(PyObject *, PyObject *, PyObject *);
typedef int (*inquiry)(PyObject *);
typedef Py_ssize_t (*lenfunc)(PyObject *);
typedef PyObject *(*ssizeargfunc)(PyObject *, Py_ssize_t);
typedef PyObject *(*ssizessizeargfunc)(PyObject *, Py_ssize_t, Py_ssize_t);
typedef int(*ssizeobjargproc)(PyObject *, Py_ssize_t, PyObject *);
typedef int(*ssizessizeobjargproc)(PyObject *, Py_ssize_t, Py_ssize_t, PyObject *);
typedef int(*objobjargproc)(PyObject *, PyObject *, PyObject *);
typedef int (*objobjproc)(PyObject *, PyObject *);
typedef int (*visitproc)(PyObject *, void *);
typedef int (*traverseproc)(PyObject *, visitproc, void *);
typedef void (*freefunc)(void *);
typedef void (*destructor)(PyObject *);
typedef PyObject *(*getattrfunc)(PyObject *, char *);
typedef PyObject *(*getattrofunc)(PyObject *, PyObject *);
typedef int (*setattrfunc)(PyObject *, char *, PyObject *);
typedef int (*setattrofunc)(PyObject *, PyObject *, PyObject *);
typedef PyObject *(*reprfunc)(PyObject *);
typedef Py_hash_t (*hashfunc)(PyObject *);
typedef PyObject *(*richcmpfunc) (PyObject *, PyObject *, int);
typedef PyObject *(*getiterfunc) (PyObject *);
typedef PyObject *(*iternextfunc) (PyObject *);
typedef PyObject *(*descrgetfunc) (PyObject *, PyObject *, PyObject *);
typedef int (*descrsetfunc) (PyObject *, PyObject *, PyObject *);
typedef int (*initproc)(PyObject *, PyObject *, PyObject *);
typedef PyObject *(*newfunc)(PyTypeObject *, PyObject *, PyObject *);
typedef PyObject *(*allocfunc)(PyTypeObject *, Py_ssize_t);
typedef PyObject *(*vectorcallfunc)(PyObject *callable, PyObject *const *args,
                                    size_t nargsf, PyObject *kwnames);
typedef struct{
    int slot;
    void *pfunc;
} PyType_Slot;
typedef struct{
    const char* name;
    int basicsize;
    int itemsize;
    unsigned int flags;
    PyType_Slot *slots;
} PyType_Spec;
__declspec(dllimport) PyObject* PyType_FromSpec(PyType_Spec*);
__declspec(dllimport) PyObject* PyType_FromSpecWithBases(PyType_Spec*, PyObject*);
__declspec(dllimport) void* PyType_GetSlot(PyTypeObject*, int);
__declspec(dllimport) PyObject* PyType_FromModuleAndSpec(PyObject *, PyType_Spec *, PyObject *);
__declspec(dllimport) PyObject * PyType_GetModule(PyTypeObject *);
__declspec(dllimport) void * PyType_GetModuleState(PyTypeObject *);
__declspec(dllimport) PyObject * PyType_GetName(PyTypeObject *);
__declspec(dllimport) PyObject * PyType_GetQualName(PyTypeObject *);
__declspec(dllimport) PyObject * PyType_FromMetaclass(PyTypeObject*, PyObject*, PyType_Spec*, PyObject*);
__declspec(dllimport) void * PyObject_GetTypeData(PyObject *obj, PyTypeObject *cls);
__declspec(dllimport) Py_ssize_t PyType_GetTypeDataSize(PyTypeObject *cls);
__declspec(dllimport) int PyType_IsSubtype(PyTypeObject *, PyTypeObject *);
static inline int PyObject_TypeCheck(PyObject *ob, PyTypeObject *type) {
    return Py_IS_TYPE(ob, type) || PyType_IsSubtype(Py_TYPE(ob), type);
}
extern __declspec(dllimport) PyTypeObject PyType_Type;
extern __declspec(dllimport) PyTypeObject PyBaseObject_Type;
extern __declspec(dllimport) PyTypeObject PySuper_Type;
__declspec(dllimport) unsigned long PyType_GetFlags(PyTypeObject*);
__declspec(dllimport) int PyType_Ready(PyTypeObject *);
__declspec(dllimport) PyObject * PyType_GenericAlloc(PyTypeObject *, Py_ssize_t);
__declspec(dllimport) PyObject * PyType_GenericNew(PyTypeObject *,
                                               PyObject *, PyObject *);
__declspec(dllimport) unsigned int PyType_ClearCache(void);
__declspec(dllimport) void PyType_Modified(PyTypeObject *);
__declspec(dllimport) PyObject * PyObject_Repr(PyObject *);
__declspec(dllimport) PyObject * PyObject_Str(PyObject *);
__declspec(dllimport) PyObject * PyObject_ASCII(PyObject *);
__declspec(dllimport) PyObject * PyObject_Bytes(PyObject *);
__declspec(dllimport) PyObject * PyObject_RichCompare(PyObject *, PyObject *, int);
__declspec(dllimport) int PyObject_RichCompareBool(PyObject *, PyObject *, int);
__declspec(dllimport) PyObject * PyObject_GetAttrString(PyObject *, const char *);
__declspec(dllimport) int PyObject_SetAttrString(PyObject *, const char *, PyObject *);
__declspec(dllimport) int PyObject_HasAttrString(PyObject *, const char *);
__declspec(dllimport) PyObject * PyObject_GetAttr(PyObject *, PyObject *);
__declspec(dllimport) int PyObject_SetAttr(PyObject *, PyObject *, PyObject *);
__declspec(dllimport) int PyObject_HasAttr(PyObject *, PyObject *);
__declspec(dllimport) PyObject * PyObject_SelfIter(PyObject *);
__declspec(dllimport) PyObject * PyObject_GenericGetAttr(PyObject *, PyObject *);
__declspec(dllimport) int PyObject_GenericSetAttr(PyObject *, PyObject *, PyObject *);
__declspec(dllimport) int PyObject_GenericSetDict(PyObject *, PyObject *, void *);
__declspec(dllimport) Py_hash_t PyObject_Hash(PyObject *);
__declspec(dllimport) Py_hash_t PyObject_HashNotImplemented(PyObject *);
__declspec(dllimport) int PyObject_IsTrue(PyObject *);
__declspec(dllimport) int PyObject_Not(PyObject *);
__declspec(dllimport) int PyCallable_Check(PyObject *);
__declspec(dllimport) void PyObject_ClearWeakRefs(PyObject *);
__declspec(dllimport) PyObject * PyObject_Dir(PyObject *);
__declspec(dllimport) int Py_ReprEnter(PyObject *);
__declspec(dllimport) void Py_ReprLeave(PyObject *);
__declspec(dllimport) void _Py_Dealloc(PyObject *);
__declspec(dllimport) void Py_IncRef(PyObject *);
__declspec(dllimport) void Py_DecRef(PyObject *);
__declspec(dllimport) void _Py_IncRef(PyObject *);
__declspec(dllimport) void _Py_DecRef(PyObject *);
static inline __attribute__((always_inline)) void Py_INCREF(PyObject *op)
{
    _Py_IncRef(op);
}
static inline void Py_DECREF(PyObject *op) {
    _Py_DecRef(op);
}
static inline void Py_XINCREF(PyObject *op)
{
    if (op != ((void*)0)) {
        Py_INCREF(op);
    }
}
static inline void Py_XDECREF(PyObject *op)
{
    if (op != ((void*)0)) {
        Py_DECREF(((PyObject*)((op))));
    }
}
__declspec(dllimport) PyObject* Py_NewRef(PyObject *obj);
__declspec(dllimport) PyObject* Py_XNewRef(PyObject *obj);
static inline PyObject* _Py_NewRef(PyObject *obj)
{
    Py_INCREF(obj);
    return obj;
}
static inline PyObject* _Py_XNewRef(PyObject *obj)
{
    Py_XINCREF(obj);
    return obj;
}
extern __declspec(dllimport) PyObject _Py_NoneStruct;
__declspec(dllimport) int Py_IsNone(PyObject *x);
extern __declspec(dllimport) PyObject _Py_NotImplementedStruct;
typedef enum {
    PYGEN_RETURN = 0,
    PYGEN_ERROR = -1,
    PYGEN_NEXT = 1,
} PySendResult;
static inline int
PyType_HasFeature(PyTypeObject *type, unsigned long feature)
{
    unsigned long flags;
    flags = PyType_GetFlags(type);
    return ((flags & feature) != 0);
}
static inline int PyType_Check(PyObject *op) {
    return PyType_HasFeature((Py_TYPE(op)), ((1UL << 31)));
}
static inline int PyType_CheckExact(PyObject *op) {
    return Py_IS_TYPE(op, &PyType_Type);
}
__declspec(dllimport) void * PyObject_Malloc(size_t size);
__declspec(dllimport) void * PyObject_Calloc(size_t nelem, size_t elsize);
__declspec(dllimport) void * PyObject_Realloc(void *ptr, size_t new_size);
__declspec(dllimport) void PyObject_Free(void *ptr);
__declspec(dllimport) PyObject * PyObject_Init(PyObject *, PyTypeObject *);
__declspec(dllimport) PyVarObject * PyObject_InitVar(PyVarObject *,
                                           PyTypeObject *, Py_ssize_t);
__declspec(dllimport) PyObject * _PyObject_New(PyTypeObject *);
__declspec(dllimport) PyVarObject * _PyObject_NewVar(PyTypeObject *, Py_ssize_t);
__declspec(dllimport) Py_ssize_t PyGC_Collect(void);
__declspec(dllimport) int PyGC_Enable(void);
__declspec(dllimport) int PyGC_Disable(void);
__declspec(dllimport) int PyGC_IsEnabled(void);
__declspec(dllimport) PyVarObject * _PyObject_GC_Resize(PyVarObject *, Py_ssize_t);
__declspec(dllimport) PyObject * _PyObject_GC_New(PyTypeObject *);
__declspec(dllimport) PyVarObject * _PyObject_GC_NewVar(PyTypeObject *, Py_ssize_t);
__declspec(dllimport) void PyObject_GC_Track(void *);
__declspec(dllimport) void PyObject_GC_UnTrack(void *);
__declspec(dllimport) void PyObject_GC_Del(void *);
__declspec(dllimport) int PyObject_GC_IsTracked(PyObject *);
__declspec(dllimport) int PyObject_GC_IsFinalized(PyObject *);
extern __declspec(dllimport) PyTypeObject PyByteArray_Type;
extern __declspec(dllimport) PyTypeObject PyByteArrayIter_Type;
__declspec(dllimport) PyObject * PyByteArray_FromObject(PyObject *);
__declspec(dllimport) PyObject * PyByteArray_Concat(PyObject *, PyObject *);
__declspec(dllimport) PyObject * PyByteArray_FromStringAndSize(const char *, Py_ssize_t);
__declspec(dllimport) Py_ssize_t PyByteArray_Size(PyObject *);
__declspec(dllimport) char * PyByteArray_AsString(PyObject *);
__declspec(dllimport) int PyByteArray_Resize(PyObject *, Py_ssize_t);
extern __declspec(dllimport) PyTypeObject PyBytes_Type;
extern __declspec(dllimport) PyTypeObject PyBytesIter_Type;
__declspec(dllimport) PyObject * PyBytes_FromStringAndSize(const char *, Py_ssize_t);
__declspec(dllimport) PyObject * PyBytes_FromString(const char *);
__declspec(dllimport) PyObject * PyBytes_FromObject(PyObject *);
__declspec(dllimport) PyObject * PyBytes_FromFormatV(const char*, va_list)
                                                                        ;
__declspec(dllimport) PyObject * PyBytes_FromFormat(const char*, ...)
                                                                        ;
__declspec(dllimport) Py_ssize_t PyBytes_Size(PyObject *);
__declspec(dllimport) char * PyBytes_AsString(PyObject *);
__declspec(dllimport) PyObject * PyBytes_Repr(PyObject *, int);
__declspec(dllimport) void PyBytes_Concat(PyObject **, PyObject *);
__declspec(dllimport) void PyBytes_ConcatAndDel(PyObject **, PyObject *);
__declspec(dllimport) PyObject * PyBytes_DecodeEscape(const char *, Py_ssize_t,
                                            const char *, Py_ssize_t,
                                            const char *);
__declspec(dllimport) int PyBytes_AsStringAndSize(
    PyObject *obj,
    char **s,
    Py_ssize_t *len
    );
typedef uint32_t Py_UCS4;
typedef uint16_t Py_UCS2;
typedef uint8_t Py_UCS1;
extern __declspec(dllimport) PyTypeObject PyUnicode_Type;
extern __declspec(dllimport) PyTypeObject PyUnicodeIter_Type;
__declspec(dllimport) PyObject* PyUnicode_FromStringAndSize(
    const char *u,
    Py_ssize_t size
    );
__declspec(dllimport) PyObject* PyUnicode_FromString(
    const char *u
    );
__declspec(dllimport) PyObject* PyUnicode_Substring(
    PyObject *str,
    Py_ssize_t start,
    Py_ssize_t end);
__declspec(dllimport) Py_UCS4* PyUnicode_AsUCS4(
    PyObject *unicode,
    Py_UCS4* buffer,
    Py_ssize_t buflen,
    int copy_null);
__declspec(dllimport) Py_UCS4* PyUnicode_AsUCS4Copy(PyObject *unicode);
__declspec(dllimport) Py_ssize_t PyUnicode_GetLength(
    PyObject *unicode
);
__declspec(dllimport) Py_UCS4 PyUnicode_ReadChar(
    PyObject *unicode,
    Py_ssize_t index
    );
__declspec(dllimport) int PyUnicode_WriteChar(
    PyObject *unicode,
    Py_ssize_t index,
    Py_UCS4 character
    );
__declspec(dllimport) int PyUnicode_Resize(
    PyObject **unicode,
    Py_ssize_t length
    );
__declspec(dllimport) PyObject* PyUnicode_FromEncodedObject(
    PyObject *obj,
    const char *encoding,
    const char *errors
    );
__declspec(dllimport) PyObject* PyUnicode_FromObject(
    PyObject *obj
    );
__declspec(dllimport) PyObject * PyUnicode_FromFormatV(
    const char *format,
    va_list vargs
    );
__declspec(dllimport) PyObject * PyUnicode_FromFormat(
    const char *format,
    ...
    );
__declspec(dllimport) void PyUnicode_InternInPlace(PyObject **);
__declspec(dllimport) PyObject * PyUnicode_InternFromString(
    const char *u
    );
__declspec(dllimport) PyObject* PyUnicode_FromWideChar(
    const wchar_t *w,
    Py_ssize_t size
    );
__declspec(dllimport) Py_ssize_t PyUnicode_AsWideChar(
    PyObject *unicode,
    wchar_t *w,
    Py_ssize_t size
    );
__declspec(dllimport) wchar_t* PyUnicode_AsWideCharString(
    PyObject *unicode,
    Py_ssize_t *size
    );
__declspec(dllimport) PyObject* PyUnicode_FromOrdinal(int ordinal);
__declspec(dllimport) const char* PyUnicode_GetDefaultEncoding(void);
__declspec(dllimport) PyObject* PyUnicode_Decode(
    const char *s,
    Py_ssize_t size,
    const char *encoding,
    const char *errors
    );
__declspec(deprecated( "deprecated in " "3.6")) __declspec(dllimport) PyObject* PyUnicode_AsDecodedObject(
    PyObject *unicode,
    const char *encoding,
    const char *errors
    );
__declspec(deprecated( "deprecated in " "3.6")) __declspec(dllimport) PyObject* PyUnicode_AsDecodedUnicode(
    PyObject *unicode,
    const char *encoding,
    const char *errors
    );
__declspec(deprecated( "deprecated in " "3.6")) __declspec(dllimport) PyObject* PyUnicode_AsEncodedObject(
    PyObject *unicode,
    const char *encoding,
    const char *errors
    );
__declspec(dllimport) PyObject* PyUnicode_AsEncodedString(
    PyObject *unicode,
    const char *encoding,
    const char *errors
    );
__declspec(deprecated( "deprecated in " "3.6")) __declspec(dllimport) PyObject* PyUnicode_AsEncodedUnicode(
    PyObject *unicode,
    const char *encoding,
    const char *errors
    );
__declspec(dllimport) PyObject* PyUnicode_BuildEncodingMap(
    PyObject* string
   );
__declspec(dllimport) PyObject* PyUnicode_DecodeUTF7(
    const char *string,
    Py_ssize_t length,
    const char *errors
    );
__declspec(dllimport) PyObject* PyUnicode_DecodeUTF7Stateful(
    const char *string,
    Py_ssize_t length,
    const char *errors,
    Py_ssize_t *consumed
    );
__declspec(dllimport) PyObject* PyUnicode_DecodeUTF8(
    const char *string,
    Py_ssize_t length,
    const char *errors
    );
__declspec(dllimport) PyObject* PyUnicode_DecodeUTF8Stateful(
    const char *string,
    Py_ssize_t length,
    const char *errors,
    Py_ssize_t *consumed
    );
__declspec(dllimport) PyObject* PyUnicode_AsUTF8String(
    PyObject *unicode
    );
__declspec(dllimport) const char * PyUnicode_AsUTF8AndSize(
    PyObject *unicode,
    Py_ssize_t *size);
__declspec(dllimport) PyObject* PyUnicode_DecodeUTF32(
    const char *string,
    Py_ssize_t length,
    const char *errors,
    int *byteorder
    );
__declspec(dllimport) PyObject* PyUnicode_DecodeUTF32Stateful(
    const char *string,
    Py_ssize_t length,
    const char *errors,
    int *byteorder,
    Py_ssize_t *consumed
    );
__declspec(dllimport) PyObject* PyUnicode_AsUTF32String(
    PyObject *unicode
    );
__declspec(dllimport) PyObject* PyUnicode_DecodeUTF16(
    const char *string,
    Py_ssize_t length,
    const char *errors,
    int *byteorder
    );
__declspec(dllimport) PyObject* PyUnicode_DecodeUTF16Stateful(
    const char *string,
    Py_ssize_t length,
    const char *errors,
    int *byteorder,
    Py_ssize_t *consumed
    );
__declspec(dllimport) PyObject* PyUnicode_AsUTF16String(
    PyObject *unicode
    );
__declspec(dllimport) PyObject* PyUnicode_DecodeUnicodeEscape(
    const char *string,
    Py_ssize_t length,
    const char *errors
    );
__declspec(dllimport) PyObject* PyUnicode_AsUnicodeEscapeString(
    PyObject *unicode
    );
__declspec(dllimport) PyObject* PyUnicode_DecodeRawUnicodeEscape(
    const char *string,
    Py_ssize_t length,
    const char *errors
    );
__declspec(dllimport) PyObject* PyUnicode_AsRawUnicodeEscapeString(
    PyObject *unicode
    );
__declspec(dllimport) PyObject* PyUnicode_DecodeLatin1(
    const char *string,
    Py_ssize_t length,
    const char *errors
    );
__declspec(dllimport) PyObject* PyUnicode_AsLatin1String(
    PyObject *unicode
    );
__declspec(dllimport) PyObject* PyUnicode_DecodeASCII(
    const char *string,
    Py_ssize_t length,
    const char *errors
    );
__declspec(dllimport) PyObject* PyUnicode_AsASCIIString(
    PyObject *unicode
    );
__declspec(dllimport) PyObject* PyUnicode_DecodeCharmap(
    const char *string,
    Py_ssize_t length,
    PyObject *mapping,
    const char *errors
    );
__declspec(dllimport) PyObject* PyUnicode_AsCharmapString(
    PyObject *unicode,
    PyObject *mapping
    );
__declspec(dllimport) PyObject* PyUnicode_DecodeMBCS(
    const char *string,
    Py_ssize_t length,
    const char *errors
    );
__declspec(dllimport) PyObject* PyUnicode_DecodeMBCSStateful(
    const char *string,
    Py_ssize_t length,
    const char *errors,
    Py_ssize_t *consumed
    );
__declspec(dllimport) PyObject* PyUnicode_DecodeCodePageStateful(
    int code_page,
    const char *string,
    Py_ssize_t length,
    const char *errors,
    Py_ssize_t *consumed
    );
__declspec(dllimport) PyObject* PyUnicode_AsMBCSString(
    PyObject *unicode
    );
__declspec(dllimport) PyObject* PyUnicode_EncodeCodePage(
    int code_page,
    PyObject *unicode,
    const char *errors
    );
__declspec(dllimport) PyObject* PyUnicode_DecodeLocaleAndSize(
    const char *str,
    Py_ssize_t len,
    const char *errors);
__declspec(dllimport) PyObject* PyUnicode_DecodeLocale(
    const char *str,
    const char *errors);
__declspec(dllimport) PyObject* PyUnicode_EncodeLocale(
    PyObject *unicode,
    const char *errors
    );
__declspec(dllimport) int PyUnicode_FSConverter(PyObject*, void*);
__declspec(dllimport) int PyUnicode_FSDecoder(PyObject*, void*);
__declspec(dllimport) PyObject* PyUnicode_DecodeFSDefault(
    const char *s
    );
__declspec(dllimport) PyObject* PyUnicode_DecodeFSDefaultAndSize(
    const char *s,
    Py_ssize_t size
    );
__declspec(dllimport) PyObject* PyUnicode_EncodeFSDefault(
    PyObject *unicode
    );
__declspec(dllimport) PyObject* PyUnicode_Concat(
    PyObject *left,
    PyObject *right
    );
__declspec(dllimport) void PyUnicode_Append(
    PyObject **pleft,
    PyObject *right
    );
__declspec(dllimport) void PyUnicode_AppendAndDel(
    PyObject **pleft,
    PyObject *right
    );
__declspec(dllimport) PyObject* PyUnicode_Split(
    PyObject *s,
    PyObject *sep,
    Py_ssize_t maxsplit
    );
__declspec(dllimport) PyObject* PyUnicode_Splitlines(
    PyObject *s,
    int keepends
    );
__declspec(dllimport) PyObject* PyUnicode_Partition(
    PyObject *s,
    PyObject *sep
    );
__declspec(dllimport) PyObject* PyUnicode_RPartition(
    PyObject *s,
    PyObject *sep
    );
__declspec(dllimport) PyObject* PyUnicode_RSplit(
    PyObject *s,
    PyObject *sep,
    Py_ssize_t maxsplit
    );
__declspec(dllimport) PyObject * PyUnicode_Translate(
    PyObject *str,
    PyObject *table,
    const char *errors
    );
__declspec(dllimport) PyObject* PyUnicode_Join(
    PyObject *separator,
    PyObject *seq
    );
__declspec(dllimport) Py_ssize_t PyUnicode_Tailmatch(
    PyObject *str,
    PyObject *substr,
    Py_ssize_t start,
    Py_ssize_t end,
    int direction
    );
__declspec(dllimport) Py_ssize_t PyUnicode_Find(
    PyObject *str,
    PyObject *substr,
    Py_ssize_t start,
    Py_ssize_t end,
    int direction
    );
__declspec(dllimport) Py_ssize_t PyUnicode_FindChar(
    PyObject *str,
    Py_UCS4 ch,
    Py_ssize_t start,
    Py_ssize_t end,
    int direction
    );
__declspec(dllimport) Py_ssize_t PyUnicode_Count(
    PyObject *str,
    PyObject *substr,
    Py_ssize_t start,
    Py_ssize_t end
    );
__declspec(dllimport) PyObject * PyUnicode_Replace(
    PyObject *str,
    PyObject *substr,
    PyObject *replstr,
    Py_ssize_t maxcount
    );
__declspec(dllimport) int PyUnicode_Compare(
    PyObject *left,
    PyObject *right
    );
__declspec(dllimport) int PyUnicode_CompareWithASCIIString(
    PyObject *left,
    const char *right
    );
__declspec(dllimport) PyObject * PyUnicode_RichCompare(
    PyObject *left,
    PyObject *right,
    int op
    );
__declspec(dllimport) PyObject * PyUnicode_Format(
    PyObject *format,
    PyObject *args
    );
__declspec(dllimport) int PyUnicode_Contains(
    PyObject *container,
    PyObject *element
    );
__declspec(dllimport) int PyUnicode_IsIdentifier(PyObject *s);
__declspec(dllimport) PyInterpreterState * PyInterpreterState_New(void);
__declspec(dllimport) void PyInterpreterState_Clear(PyInterpreterState *);
__declspec(dllimport) void PyInterpreterState_Delete(PyInterpreterState *);
__declspec(dllimport) PyInterpreterState * PyInterpreterState_Get(void);
__declspec(dllimport) PyObject * PyInterpreterState_GetDict(PyInterpreterState *);
__declspec(dllimport) int64_t PyInterpreterState_GetID(PyInterpreterState *);
__declspec(dllimport) int PyState_AddModule(PyObject*, PyModuleDef*);
__declspec(dllimport) int PyState_RemoveModule(PyModuleDef*);
__declspec(dllimport) PyObject* PyState_FindModule(PyModuleDef*);
__declspec(dllimport) PyThreadState * PyThreadState_New(PyInterpreterState *);
__declspec(dllimport) void PyThreadState_Clear(PyThreadState *);
__declspec(dllimport) void PyThreadState_Delete(PyThreadState *);
__declspec(dllimport) PyThreadState * PyThreadState_Get(void);
__declspec(dllimport) PyThreadState * PyThreadState_Swap(PyThreadState *);
__declspec(dllimport) PyObject * PyThreadState_GetDict(void);
__declspec(dllimport) int PyThreadState_SetAsyncExc(unsigned long, PyObject *);
__declspec(dllimport) PyInterpreterState* PyThreadState_GetInterpreter(PyThreadState *tstate);
__declspec(dllimport) PyFrameObject* PyThreadState_GetFrame(PyThreadState *tstate);
__declspec(dllimport) uint64_t PyThreadState_GetID(PyThreadState *tstate);
typedef
    enum {PyGILState_LOCKED, PyGILState_UNLOCKED}
        PyGILState_STATE;
__declspec(dllimport) PyGILState_STATE PyGILState_Ensure(void);
__declspec(dllimport) void PyGILState_Release(PyGILState_STATE);
__declspec(dllimport) PyThreadState * PyGILState_GetThisThreadState(void);
__declspec(dllimport) void PyErr_SetNone(PyObject *);
__declspec(dllimport) void PyErr_SetObject(PyObject *, PyObject *);
__declspec(dllimport) void PyErr_SetString(
    PyObject *exception,
    const char *string
    );
__declspec(dllimport) PyObject * PyErr_Occurred(void);
__declspec(dllimport) void PyErr_Clear(void);
__declspec(dllimport) void PyErr_Fetch(PyObject **, PyObject **, PyObject **);
__declspec(dllimport) void PyErr_Restore(PyObject *, PyObject *, PyObject *);
__declspec(dllimport) PyObject * PyErr_GetRaisedException(void);
__declspec(dllimport) void PyErr_SetRaisedException(PyObject *);
__declspec(dllimport) PyObject* PyErr_GetHandledException(void);
__declspec(dllimport) void PyErr_SetHandledException(PyObject *);
__declspec(dllimport) void PyErr_GetExcInfo(PyObject **, PyObject **, PyObject **);
__declspec(dllimport) void PyErr_SetExcInfo(PyObject *, PyObject *, PyObject *);
__declspec(dllimport) void __attribute__((__noreturn__)) Py_FatalError(const char *message);
__declspec(dllimport) int PyErr_GivenExceptionMatches(PyObject *, PyObject *);
__declspec(dllimport) int PyErr_ExceptionMatches(PyObject *);
__declspec(dllimport) void PyErr_NormalizeException(PyObject**, PyObject**, PyObject**);
__declspec(dllimport) int PyException_SetTraceback(PyObject *, PyObject *);
__declspec(dllimport) PyObject * PyException_GetTraceback(PyObject *);
__declspec(dllimport) PyObject * PyException_GetCause(PyObject *);
__declspec(dllimport) void PyException_SetCause(PyObject *, PyObject *);
__declspec(dllimport) PyObject * PyException_GetContext(PyObject *);
__declspec(dllimport) void PyException_SetContext(PyObject *, PyObject *);
__declspec(dllimport) PyObject * PyException_GetArgs(PyObject *);
__declspec(dllimport) void PyException_SetArgs(PyObject *, PyObject *);
__declspec(dllimport) const char * PyExceptionClass_Name(PyObject *);
extern __declspec(dllimport) PyObject * PyExc_BaseException;
extern __declspec(dllimport) PyObject * PyExc_Exception;
extern __declspec(dllimport) PyObject * PyExc_BaseExceptionGroup;
extern __declspec(dllimport) PyObject * PyExc_StopAsyncIteration;
extern __declspec(dllimport) PyObject * PyExc_StopIteration;
extern __declspec(dllimport) PyObject * PyExc_GeneratorExit;
extern __declspec(dllimport) PyObject * PyExc_ArithmeticError;
extern __declspec(dllimport) PyObject * PyExc_LookupError;
extern __declspec(dllimport) PyObject * PyExc_AssertionError;
extern __declspec(dllimport) PyObject * PyExc_AttributeError;
extern __declspec(dllimport) PyObject * PyExc_BufferError;
extern __declspec(dllimport) PyObject * PyExc_EOFError;
extern __declspec(dllimport) PyObject * PyExc_FloatingPointError;
extern __declspec(dllimport) PyObject * PyExc_OSError;
extern __declspec(dllimport) PyObject * PyExc_ImportError;
extern __declspec(dllimport) PyObject * PyExc_ModuleNotFoundError;
extern __declspec(dllimport) PyObject * PyExc_IndexError;
extern __declspec(dllimport) PyObject * PyExc_KeyError;
extern __declspec(dllimport) PyObject * PyExc_KeyboardInterrupt;
extern __declspec(dllimport) PyObject * PyExc_MemoryError;
extern __declspec(dllimport) PyObject * PyExc_NameError;
extern __declspec(dllimport) PyObject * PyExc_OverflowError;
extern __declspec(dllimport) PyObject * PyExc_RuntimeError;
extern __declspec(dllimport) PyObject * PyExc_RecursionError;
extern __declspec(dllimport) PyObject * PyExc_NotImplementedError;
extern __declspec(dllimport) PyObject * PyExc_SyntaxError;
extern __declspec(dllimport) PyObject * PyExc_IndentationError;
extern __declspec(dllimport) PyObject * PyExc_TabError;
extern __declspec(dllimport) PyObject * PyExc_ReferenceError;
extern __declspec(dllimport) PyObject * PyExc_SystemError;
extern __declspec(dllimport) PyObject * PyExc_SystemExit;
extern __declspec(dllimport) PyObject * PyExc_TypeError;
extern __declspec(dllimport) PyObject * PyExc_UnboundLocalError;
extern __declspec(dllimport) PyObject * PyExc_UnicodeError;
extern __declspec(dllimport) PyObject * PyExc_UnicodeEncodeError;
extern __declspec(dllimport) PyObject * PyExc_UnicodeDecodeError;
extern __declspec(dllimport) PyObject * PyExc_UnicodeTranslateError;
extern __declspec(dllimport) PyObject * PyExc_ValueError;
extern __declspec(dllimport) PyObject * PyExc_ZeroDivisionError;
extern __declspec(dllimport) PyObject * PyExc_BlockingIOError;
extern __declspec(dllimport) PyObject * PyExc_BrokenPipeError;
extern __declspec(dllimport) PyObject * PyExc_ChildProcessError;
extern __declspec(dllimport) PyObject * PyExc_ConnectionError;
extern __declspec(dllimport) PyObject * PyExc_ConnectionAbortedError;
extern __declspec(dllimport) PyObject * PyExc_ConnectionRefusedError;
extern __declspec(dllimport) PyObject * PyExc_ConnectionResetError;
extern __declspec(dllimport) PyObject * PyExc_FileExistsError;
extern __declspec(dllimport) PyObject * PyExc_FileNotFoundError;
extern __declspec(dllimport) PyObject * PyExc_InterruptedError;
extern __declspec(dllimport) PyObject * PyExc_IsADirectoryError;
extern __declspec(dllimport) PyObject * PyExc_NotADirectoryError;
extern __declspec(dllimport) PyObject * PyExc_PermissionError;
extern __declspec(dllimport) PyObject * PyExc_ProcessLookupError;
extern __declspec(dllimport) PyObject * PyExc_TimeoutError;
extern __declspec(dllimport) PyObject * PyExc_EnvironmentError;
extern __declspec(dllimport) PyObject * PyExc_IOError;
extern __declspec(dllimport) PyObject * PyExc_WindowsError;
extern __declspec(dllimport) PyObject * PyExc_Warning;
extern __declspec(dllimport) PyObject * PyExc_UserWarning;
extern __declspec(dllimport) PyObject * PyExc_DeprecationWarning;
extern __declspec(dllimport) PyObject * PyExc_PendingDeprecationWarning;
extern __declspec(dllimport) PyObject * PyExc_SyntaxWarning;
extern __declspec(dllimport) PyObject * PyExc_RuntimeWarning;
extern __declspec(dllimport) PyObject * PyExc_FutureWarning;
extern __declspec(dllimport) PyObject * PyExc_ImportWarning;
extern __declspec(dllimport) PyObject * PyExc_UnicodeWarning;
extern __declspec(dllimport) PyObject * PyExc_BytesWarning;
extern __declspec(dllimport) PyObject * PyExc_EncodingWarning;
extern __declspec(dllimport) PyObject * PyExc_ResourceWarning;
__declspec(dllimport) int PyErr_BadArgument(void);
__declspec(dllimport) PyObject * PyErr_NoMemory(void);
__declspec(dllimport) PyObject * PyErr_SetFromErrno(PyObject *);
__declspec(dllimport) PyObject * PyErr_SetFromErrnoWithFilenameObject(
    PyObject *, PyObject *);
__declspec(dllimport) PyObject * PyErr_SetFromErrnoWithFilenameObjects(
    PyObject *, PyObject *, PyObject *);
__declspec(dllimport) PyObject * PyErr_SetFromErrnoWithFilename(
    PyObject *exc,
    const char *filename
    );
__declspec(dllimport) PyObject * PyErr_Format(
    PyObject *exception,
    const char *format,
    ...
    );
__declspec(dllimport) PyObject * PyErr_FormatV(
    PyObject *exception,
    const char *format,
    va_list vargs);
__declspec(dllimport) PyObject * PyErr_SetFromWindowsErrWithFilename(
    int ierr,
    const char *filename
    );
__declspec(dllimport) PyObject * PyErr_SetFromWindowsErr(int);
__declspec(dllimport) PyObject * PyErr_SetExcFromWindowsErrWithFilenameObject(
    PyObject *,int, PyObject *);
__declspec(dllimport) PyObject * PyErr_SetExcFromWindowsErrWithFilenameObjects(
    PyObject *,int, PyObject *, PyObject *);
__declspec(dllimport) PyObject * PyErr_SetExcFromWindowsErrWithFilename(
    PyObject *exc,
    int ierr,
    const char *filename
    );
__declspec(dllimport) PyObject * PyErr_SetExcFromWindowsErr(PyObject *, int);
__declspec(dllimport) PyObject * PyErr_SetImportErrorSubclass(PyObject *, PyObject *,
    PyObject *, PyObject *);
__declspec(dllimport) PyObject * PyErr_SetImportError(PyObject *, PyObject *,
    PyObject *);
__declspec(dllimport) void PyErr_BadInternalCall(void);
__declspec(dllimport) void _PyErr_BadInternalCall(const char *filename, int lineno);
__declspec(dllimport) PyObject * PyErr_NewException(
    const char *name, PyObject *base, PyObject *dict);
__declspec(dllimport) PyObject * PyErr_NewExceptionWithDoc(
    const char *name, const char *doc, PyObject *base, PyObject *dict);
__declspec(dllimport) void PyErr_WriteUnraisable(PyObject *);
__declspec(dllimport) int PyErr_CheckSignals(void);
__declspec(dllimport) void PyErr_SetInterrupt(void);
__declspec(dllimport) int PyErr_SetInterruptEx(int signum);
__declspec(dllimport) void PyErr_SyntaxLocation(
    const char *filename,
    int lineno);
__declspec(dllimport) void PyErr_SyntaxLocationEx(
    const char *filename,
    int lineno,
    int col_offset);
__declspec(dllimport) PyObject * PyErr_ProgramText(
    const char *filename,
    int lineno);
__declspec(dllimport) PyObject * PyUnicodeDecodeError_Create(
    const char *encoding,
    const char *object,
    Py_ssize_t length,
    Py_ssize_t start,
    Py_ssize_t end,
    const char *reason
    );
__declspec(dllimport) PyObject * PyUnicodeEncodeError_GetEncoding(PyObject *);
__declspec(dllimport) PyObject * PyUnicodeDecodeError_GetEncoding(PyObject *);
__declspec(dllimport) PyObject * PyUnicodeEncodeError_GetObject(PyObject *);
__declspec(dllimport) PyObject * PyUnicodeDecodeError_GetObject(PyObject *);
__declspec(dllimport) PyObject * PyUnicodeTranslateError_GetObject(PyObject *);
__declspec(dllimport) int PyUnicodeEncodeError_GetStart(PyObject *, Py_ssize_t *);
__declspec(dllimport) int PyUnicodeDecodeError_GetStart(PyObject *, Py_ssize_t *);
__declspec(dllimport) int PyUnicodeTranslateError_GetStart(PyObject *, Py_ssize_t *);
__declspec(dllimport) int PyUnicodeEncodeError_SetStart(PyObject *, Py_ssize_t);
__declspec(dllimport) int PyUnicodeDecodeError_SetStart(PyObject *, Py_ssize_t);
__declspec(dllimport) int PyUnicodeTranslateError_SetStart(PyObject *, Py_ssize_t);
__declspec(dllimport) int PyUnicodeEncodeError_GetEnd(PyObject *, Py_ssize_t *);
__declspec(dllimport) int PyUnicodeDecodeError_GetEnd(PyObject *, Py_ssize_t *);
__declspec(dllimport) int PyUnicodeTranslateError_GetEnd(PyObject *, Py_ssize_t *);
__declspec(dllimport) int PyUnicodeEncodeError_SetEnd(PyObject *, Py_ssize_t);
__declspec(dllimport) int PyUnicodeDecodeError_SetEnd(PyObject *, Py_ssize_t);
__declspec(dllimport) int PyUnicodeTranslateError_SetEnd(PyObject *, Py_ssize_t);
__declspec(dllimport) PyObject * PyUnicodeEncodeError_GetReason(PyObject *);
__declspec(dllimport) PyObject * PyUnicodeDecodeError_GetReason(PyObject *);
__declspec(dllimport) PyObject * PyUnicodeTranslateError_GetReason(PyObject *);
__declspec(dllimport) int PyUnicodeEncodeError_SetReason(
    PyObject *exc,
    const char *reason
    );
__declspec(dllimport) int PyUnicodeDecodeError_SetReason(
    PyObject *exc,
    const char *reason
    );
__declspec(dllimport) int PyUnicodeTranslateError_SetReason(
    PyObject *exc,
    const char *reason
    );
__declspec(dllimport) int PyOS_snprintf(char *str, size_t size, const char *format, ...)
                                                                ;
__declspec(dllimport) int PyOS_vsnprintf(char *str, size_t size, const char *format, va_list va)
                                                                ;
__declspec(dllimport) PyObject * PyLong_FromLong(long);
__declspec(dllimport) PyObject * PyLong_FromUnsignedLong(unsigned long);
__declspec(dllimport) PyObject * PyLong_FromSize_t(size_t);
__declspec(dllimport) PyObject * PyLong_FromSsize_t(Py_ssize_t);
__declspec(dllimport) PyObject * PyLong_FromDouble(double);
__declspec(dllimport) long PyLong_AsLong(PyObject *);
__declspec(dllimport) long PyLong_AsLongAndOverflow(PyObject *, int *);
__declspec(dllimport) Py_ssize_t PyLong_AsSsize_t(PyObject *);
__declspec(dllimport) size_t PyLong_AsSize_t(PyObject *);
__declspec(dllimport) unsigned long PyLong_AsUnsignedLong(PyObject *);
__declspec(dllimport) unsigned long PyLong_AsUnsignedLongMask(PyObject *);
__declspec(dllimport) PyObject * PyLong_GetInfo(void);
__declspec(dllimport) double PyLong_AsDouble(PyObject *);
__declspec(dllimport) PyObject * PyLong_FromVoidPtr(void *);
__declspec(dllimport) void * PyLong_AsVoidPtr(PyObject *);
__declspec(dllimport) PyObject * PyLong_FromLongLong(long long);
__declspec(dllimport) PyObject * PyLong_FromUnsignedLongLong(unsigned long long);
__declspec(dllimport) long long PyLong_AsLongLong(PyObject *);
__declspec(dllimport) unsigned long long PyLong_AsUnsignedLongLong(PyObject *);
__declspec(dllimport) unsigned long long PyLong_AsUnsignedLongLongMask(PyObject *);
__declspec(dllimport) long long PyLong_AsLongLongAndOverflow(PyObject *, int *);
__declspec(dllimport) PyObject * PyLong_FromString(const char *, char **, int);
__declspec(dllimport) unsigned long PyOS_strtoul(const char *, char **, int);
__declspec(dllimport) long PyOS_strtol(const char *, char **, int);
extern __declspec(dllimport) PyLongObject _Py_FalseStruct;
extern __declspec(dllimport) PyLongObject _Py_TrueStruct;
__declspec(dllimport) int Py_IsTrue(PyObject *x);
__declspec(dllimport) int Py_IsFalse(PyObject *x);
__declspec(dllimport) PyObject * PyBool_FromLong(long);
extern __declspec(dllimport) PyTypeObject PyFloat_Type;
__declspec(dllimport) double PyFloat_GetMax(void);
__declspec(dllimport) double PyFloat_GetMin(void);
__declspec(dllimport) PyObject* PyFloat_GetInfo(void);
__declspec(dllimport) PyObject* PyFloat_FromString(PyObject*);
__declspec(dllimport) PyObject* PyFloat_FromDouble(double);
__declspec(dllimport) double PyFloat_AsDouble(PyObject*);
extern __declspec(dllimport) PyTypeObject PyComplex_Type;
__declspec(dllimport) PyObject * PyComplex_FromDoubles(double real, double imag);
__declspec(dllimport) double PyComplex_RealAsDouble(PyObject *op);
__declspec(dllimport) double PyComplex_ImagAsDouble(PyObject *op);
extern __declspec(dllimport) PyTypeObject PyRange_Type;
extern __declspec(dllimport) PyTypeObject PyRangeIter_Type;
extern __declspec(dllimport) PyTypeObject PyLongRangeIter_Type;
extern __declspec(dllimport) PyTypeObject PyMemoryView_Type;
__declspec(dllimport) PyObject * PyMemoryView_FromObject(PyObject *base);
__declspec(dllimport) PyObject * PyMemoryView_FromMemory(char *mem, Py_ssize_t size,
                                               int flags);
__declspec(dllimport) PyObject * PyMemoryView_FromBuffer(const Py_buffer *info);
__declspec(dllimport) PyObject * PyMemoryView_GetContiguous(PyObject *base,
                                                  int buffertype,
                                                  char order);
extern __declspec(dllimport) PyTypeObject PyTuple_Type;
extern __declspec(dllimport) PyTypeObject PyTupleIter_Type;
__declspec(dllimport) PyObject * PyTuple_New(Py_ssize_t size);
__declspec(dllimport) Py_ssize_t PyTuple_Size(PyObject *);
__declspec(dllimport) PyObject * PyTuple_GetItem(PyObject *, Py_ssize_t);
__declspec(dllimport) int PyTuple_SetItem(PyObject *, Py_ssize_t, PyObject *);
__declspec(dllimport) PyObject * PyTuple_GetSlice(PyObject *, Py_ssize_t, Py_ssize_t);
__declspec(dllimport) PyObject * PyTuple_Pack(Py_ssize_t, ...);
extern __declspec(dllimport) PyTypeObject PyList_Type;
extern __declspec(dllimport) PyTypeObject PyListIter_Type;
extern __declspec(dllimport) PyTypeObject PyListRevIter_Type;
__declspec(dllimport) PyObject * PyList_New(Py_ssize_t size);
__declspec(dllimport) Py_ssize_t PyList_Size(PyObject *);
__declspec(dllimport) PyObject * PyList_GetItem(PyObject *, Py_ssize_t);
__declspec(dllimport) int PyList_SetItem(PyObject *, Py_ssize_t, PyObject *);
__declspec(dllimport) int PyList_Insert(PyObject *, Py_ssize_t, PyObject *);
__declspec(dllimport) int PyList_Append(PyObject *, PyObject *);
__declspec(dllimport) PyObject * PyList_GetSlice(PyObject *, Py_ssize_t, Py_ssize_t);
__declspec(dllimport) int PyList_SetSlice(PyObject *, Py_ssize_t, Py_ssize_t, PyObject *);
__declspec(dllimport) int PyList_Sort(PyObject *);
__declspec(dllimport) int PyList_Reverse(PyObject *);
__declspec(dllimport) PyObject * PyList_AsTuple(PyObject *);
extern __declspec(dllimport) PyTypeObject PyDict_Type;
__declspec(dllimport) PyObject * PyDict_New(void);
__declspec(dllimport) PyObject * PyDict_GetItem(PyObject *mp, PyObject *key);
__declspec(dllimport) PyObject * PyDict_GetItemWithError(PyObject *mp, PyObject *key);
__declspec(dllimport) int PyDict_SetItem(PyObject *mp, PyObject *key, PyObject *item);
__declspec(dllimport) int PyDict_DelItem(PyObject *mp, PyObject *key);
__declspec(dllimport) void PyDict_Clear(PyObject *mp);
__declspec(dllimport) int PyDict_Next(
    PyObject *mp, Py_ssize_t *pos, PyObject **key, PyObject **value);
__declspec(dllimport) PyObject * PyDict_Keys(PyObject *mp);
__declspec(dllimport) PyObject * PyDict_Values(PyObject *mp);
__declspec(dllimport) PyObject * PyDict_Items(PyObject *mp);
__declspec(dllimport) Py_ssize_t PyDict_Size(PyObject *mp);
__declspec(dllimport) PyObject * PyDict_Copy(PyObject *mp);
__declspec(dllimport) int PyDict_Contains(PyObject *mp, PyObject *key);
__declspec(dllimport) int PyDict_Update(PyObject *mp, PyObject *other);
__declspec(dllimport) int PyDict_Merge(PyObject *mp,
                             PyObject *other,
                             int override);
__declspec(dllimport) int PyDict_MergeFromSeq2(PyObject *d,
                                     PyObject *seq2,
                                     int override);
__declspec(dllimport) PyObject * PyDict_GetItemString(PyObject *dp, const char *key);
__declspec(dllimport) int PyDict_SetItemString(PyObject *dp, const char *key, PyObject *item);
__declspec(dllimport) int PyDict_DelItemString(PyObject *dp, const char *key);
__declspec(dllimport) PyObject * PyObject_GenericGetDict(PyObject *, void *);
extern __declspec(dllimport) PyTypeObject PyDictKeys_Type;
extern __declspec(dllimport) PyTypeObject PyDictValues_Type;
extern __declspec(dllimport) PyTypeObject PyDictItems_Type;
extern __declspec(dllimport) PyTypeObject PyDictIterKey_Type;
extern __declspec(dllimport) PyTypeObject PyDictIterValue_Type;
extern __declspec(dllimport) PyTypeObject PyDictIterItem_Type;
extern __declspec(dllimport) PyTypeObject PyDictRevIterKey_Type;
extern __declspec(dllimport) PyTypeObject PyDictRevIterItem_Type;
extern __declspec(dllimport) PyTypeObject PyDictRevIterValue_Type;
extern __declspec(dllimport) PyTypeObject PyEnum_Type;
extern __declspec(dllimport) PyTypeObject PyReversed_Type;
extern __declspec(dllimport) PyTypeObject PySet_Type;
extern __declspec(dllimport) PyTypeObject PyFrozenSet_Type;
extern __declspec(dllimport) PyTypeObject PySetIter_Type;
__declspec(dllimport) PyObject * PySet_New(PyObject *);
__declspec(dllimport) PyObject * PyFrozenSet_New(PyObject *);
__declspec(dllimport) int PySet_Add(PyObject *set, PyObject *key);
__declspec(dllimport) int PySet_Clear(PyObject *set);
__declspec(dllimport) int PySet_Contains(PyObject *anyset, PyObject *key);
__declspec(dllimport) int PySet_Discard(PyObject *set, PyObject *key);
__declspec(dllimport) PyObject * PySet_Pop(PyObject *set);
__declspec(dllimport) Py_ssize_t PySet_Size(PyObject *anyset);
extern __declspec(dllimport) PyTypeObject PyCFunction_Type;
typedef PyObject *(*PyCFunction)(PyObject *, PyObject *);
typedef PyObject *(*_PyCFunctionFast) (PyObject *, PyObject *const *, Py_ssize_t);
typedef PyObject *(*PyCFunctionWithKeywords)(PyObject *, PyObject *,
                                             PyObject *);
typedef PyObject *(*_PyCFunctionFastWithKeywords) (PyObject *,
                                                   PyObject *const *, Py_ssize_t,
                                                   PyObject *);
typedef PyObject *(*PyCMethod)(PyObject *, PyTypeObject *, PyObject *const *,
                               size_t, PyObject *);
__declspec(dllimport) PyCFunction PyCFunction_GetFunction(PyObject *);
__declspec(dllimport) PyObject * PyCFunction_GetSelf(PyObject *);
__declspec(dllimport) int PyCFunction_GetFlags(PyObject *);
__declspec(deprecated( "deprecated in " "3.9")) __declspec(dllimport) PyObject * PyCFunction_Call(PyObject *, PyObject *, PyObject *);
struct PyMethodDef {
    const char *ml_name;
    PyCFunction ml_meth;
    int ml_flags;
    const char *ml_doc;
};
__declspec(dllimport) PyObject * PyCFunction_New(PyMethodDef *, PyObject *);
__declspec(dllimport) PyObject * PyCFunction_NewEx(PyMethodDef *, PyObject *,
                                         PyObject *);
__declspec(dllimport) PyObject * PyCMethod_New(PyMethodDef *, PyObject *,
                                     PyObject *, PyTypeObject *);
extern __declspec(dllimport) PyTypeObject PyModule_Type;
__declspec(dllimport) PyObject * PyModule_NewObject(
    PyObject *name
    );
__declspec(dllimport) PyObject * PyModule_New(
    const char *name
    );
__declspec(dllimport) PyObject * PyModule_GetDict(PyObject *);
__declspec(dllimport) PyObject * PyModule_GetNameObject(PyObject *);
__declspec(dllimport) const char * PyModule_GetName(PyObject *);
__declspec(deprecated( "deprecated in " "3.2")) __declspec(dllimport) const char * PyModule_GetFilename(PyObject *);
__declspec(dllimport) PyObject * PyModule_GetFilenameObject(PyObject *);
__declspec(dllimport) PyModuleDef* PyModule_GetDef(PyObject*);
__declspec(dllimport) void* PyModule_GetState(PyObject*);
__declspec(dllimport) PyObject * PyModuleDef_Init(PyModuleDef*);
extern __declspec(dllimport) PyTypeObject PyModuleDef_Type;
typedef struct PyModuleDef_Base {
  PyObject ob_base;
  PyObject* (*m_init)(void);
  Py_ssize_t m_index;
  PyObject* m_copy;
} PyModuleDef_Base;
struct PyModuleDef_Slot {
    int slot;
    void *value;
};
struct PyModuleDef {
  PyModuleDef_Base m_base;
  const char* m_name;
  const char* m_doc;
  Py_ssize_t m_size;
  PyMethodDef *m_methods;
  PyModuleDef_Slot *m_slots;
  traverseproc m_traverse;
  inquiry m_clear;
  freefunc m_free;
};
__declspec(dllimport) PyObject * PyFile_FromFd(int, const char *, const char *, int,
                                     const char *, const char *,
                                     const char *, int);
__declspec(dllimport) PyObject * PyFile_GetLine(PyObject *, int);
__declspec(dllimport) int PyFile_WriteObject(PyObject *, PyObject *, int);
__declspec(dllimport) int PyFile_WriteString(const char *, PyObject *);
__declspec(dllimport) int PyObject_AsFileDescriptor(PyObject *);
__declspec(deprecated( "deprecated in " "3.12")) extern __declspec(dllimport) const char * Py_FileSystemDefaultEncoding;
__declspec(deprecated( "deprecated in " "3.12")) extern __declspec(dllimport) const char * Py_FileSystemDefaultEncodeErrors;
__declspec(deprecated( "deprecated in " "3.12")) extern __declspec(dllimport) int Py_HasFileSystemDefaultEncoding;
__declspec(deprecated( "deprecated in " "3.12")) extern __declspec(dllimport) int Py_UTF8Mode;
extern __declspec(dllimport) PyTypeObject PyCapsule_Type;
typedef void (*PyCapsule_Destructor)(PyObject *);
__declspec(dllimport) PyObject * PyCapsule_New(
    void *pointer,
    const char *name,
    PyCapsule_Destructor destructor);
__declspec(dllimport) void * PyCapsule_GetPointer(PyObject *capsule, const char *name);
__declspec(dllimport) PyCapsule_Destructor PyCapsule_GetDestructor(PyObject *capsule);
__declspec(dllimport) const char * PyCapsule_GetName(PyObject *capsule);
__declspec(dllimport) void * PyCapsule_GetContext(PyObject *capsule);
__declspec(dllimport) int PyCapsule_IsValid(PyObject *capsule, const char *name);
__declspec(dllimport) int PyCapsule_SetPointer(PyObject *capsule, void *pointer);
__declspec(dllimport) int PyCapsule_SetDestructor(PyObject *capsule, PyCapsule_Destructor destructor);
__declspec(dllimport) int PyCapsule_SetName(PyObject *capsule, const char *name);
__declspec(dllimport) int PyCapsule_SetContext(PyObject *capsule, void *context);
__declspec(dllimport) void * PyCapsule_Import(
    const char *name,
    int no_block);
__declspec(dllimport) int PyFrame_GetLineNumber(PyFrameObject *);
__declspec(dllimport) PyCodeObject * PyFrame_GetCode(PyFrameObject *frame);
__declspec(dllimport) int PyTraceBack_Here(PyFrameObject *);
__declspec(dllimport) int PyTraceBack_Print(PyObject *, PyObject *);
extern __declspec(dllimport) PyTypeObject PyTraceBack_Type;
extern __declspec(dllimport) PyObject _Py_EllipsisObject;
extern __declspec(dllimport) PyTypeObject PySlice_Type;
extern __declspec(dllimport) PyTypeObject PyEllipsis_Type;
__declspec(dllimport) PyObject * PySlice_New(PyObject* start, PyObject* stop,
                                  PyObject* step);
__declspec(dllimport) int PySlice_GetIndices(PyObject *r, Py_ssize_t length,
                                  Py_ssize_t *start, Py_ssize_t *stop, Py_ssize_t *step);
__declspec(deprecated( "deprecated in " "3.7"))
__declspec(dllimport) int PySlice_GetIndicesEx(PyObject *r, Py_ssize_t length,
                                     Py_ssize_t *start, Py_ssize_t *stop,
                                     Py_ssize_t *step,
                                     Py_ssize_t *slicelength);
__declspec(dllimport) int PySlice_Unpack(PyObject *slice,
                               Py_ssize_t *start, Py_ssize_t *stop, Py_ssize_t *step);
__declspec(dllimport) Py_ssize_t PySlice_AdjustIndices(Py_ssize_t length,
                                             Py_ssize_t *start, Py_ssize_t *stop,
                                             Py_ssize_t step);
extern __declspec(dllimport) PyTypeObject PySeqIter_Type;
extern __declspec(dllimport) PyTypeObject PyCallIter_Type;
__declspec(dllimport) PyObject * PySeqIter_New(PyObject *);
__declspec(dllimport) PyObject * PyCallIter_New(PyObject *, PyObject *);
typedef PyObject *(*getter)(PyObject *, void *);
typedef int (*setter)(PyObject *, PyObject *, void *);
struct PyGetSetDef {
    const char *name;
    getter get;
    setter set;
    const char *doc;
    void *closure;
};
extern __declspec(dllimport) PyTypeObject PyClassMethodDescr_Type;
extern __declspec(dllimport) PyTypeObject PyGetSetDescr_Type;
extern __declspec(dllimport) PyTypeObject PyMemberDescr_Type;
extern __declspec(dllimport) PyTypeObject PyMethodDescr_Type;
extern __declspec(dllimport) PyTypeObject PyWrapperDescr_Type;
extern __declspec(dllimport) PyTypeObject PyDictProxy_Type;
extern __declspec(dllimport) PyTypeObject PyProperty_Type;
__declspec(dllimport) PyObject * PyDescr_NewMethod(PyTypeObject *, PyMethodDef *);
__declspec(dllimport) PyObject * PyDescr_NewClassMethod(PyTypeObject *, PyMethodDef *);
__declspec(dllimport) PyObject * PyDescr_NewMember(PyTypeObject *, PyMemberDef *);
__declspec(dllimport) PyObject * PyDescr_NewGetSet(PyTypeObject *, PyGetSetDef *);
__declspec(dllimport) PyObject * PyDictProxy_New(PyObject *);
__declspec(dllimport) PyObject * PyWrapper_New(PyObject *, PyObject *);
struct PyMemberDef {
    const char *name;
    int type;
    Py_ssize_t offset;
    int flags;
    const char *doc;
};
__declspec(dllimport) PyObject * PyMember_GetOne(const char *, PyMemberDef *);
__declspec(dllimport) int PyMember_SetOne(char *, PyMemberDef *, PyObject *);
__declspec(dllimport) PyObject * Py_GenericAlias(PyObject *, PyObject *);
extern __declspec(dllimport) PyTypeObject Py_GenericAliasType;
__declspec(dllimport) int PyErr_WarnEx(
    PyObject *category,
    const char *message,
    Py_ssize_t stack_level);
__declspec(dllimport) int PyErr_WarnFormat(
    PyObject *category,
    Py_ssize_t stack_level,
    const char *format,
    ...);
__declspec(dllimport) int PyErr_ResourceWarning(
    PyObject *source,
    Py_ssize_t stack_level,
    const char *format,
    ...);
__declspec(dllimport) int PyErr_WarnExplicit(
    PyObject *category,
    const char *message,
    const char *filename,
    int lineno,
    const char *module,
    PyObject *registry);
typedef struct _PyWeakReference PyWeakReference;
extern __declspec(dllimport) PyTypeObject _PyWeakref_RefType;
extern __declspec(dllimport) PyTypeObject _PyWeakref_ProxyType;
extern __declspec(dllimport) PyTypeObject _PyWeakref_CallableProxyType;
__declspec(dllimport) PyObject * PyWeakref_NewRef(PyObject *ob,
                                        PyObject *callback);
__declspec(dllimport) PyObject * PyWeakref_NewProxy(PyObject *ob,
                                          PyObject *callback);
__declspec(dllimport) PyObject * PyWeakref_GetObject(PyObject *ref);
typedef struct PyStructSequence_Field {
    const char *name;
    const char *doc;
} PyStructSequence_Field;
typedef struct PyStructSequence_Desc {
    const char *name;
    const char *doc;
    PyStructSequence_Field *fields;
    int n_in_sequence;
} PyStructSequence_Desc;
extern __declspec(dllimport) const char * const PyStructSequence_UnnamedField;
__declspec(dllimport) PyTypeObject* PyStructSequence_NewType(PyStructSequence_Desc *desc);
__declspec(dllimport) PyObject * PyStructSequence_New(PyTypeObject* type);
__declspec(dllimport) void PyStructSequence_SetItem(PyObject*, Py_ssize_t, PyObject*);
__declspec(dllimport) PyObject* PyStructSequence_GetItem(PyObject*, Py_ssize_t);
__declspec(dllimport) int PyCodec_Register(
       PyObject *search_function
       );
__declspec(dllimport) int PyCodec_Unregister(
       PyObject *search_function
       );
__declspec(dllimport) int PyCodec_KnownEncoding(
       const char *encoding
       );
__declspec(dllimport) PyObject * PyCodec_Encode(
       PyObject *object,
       const char *encoding,
       const char *errors
       );
__declspec(dllimport) PyObject * PyCodec_Decode(
       PyObject *object,
       const char *encoding,
       const char *errors
       );
__declspec(dllimport) PyObject * PyCodec_Encoder(
       const char *encoding
       );
__declspec(dllimport) PyObject * PyCodec_Decoder(
       const char *encoding
       );
__declspec(dllimport) PyObject * PyCodec_IncrementalEncoder(
       const char *encoding,
       const char *errors
       );
__declspec(dllimport) PyObject * PyCodec_IncrementalDecoder(
       const char *encoding,
       const char *errors
       );
__declspec(dllimport) PyObject * PyCodec_StreamReader(
       const char *encoding,
       PyObject *stream,
       const char *errors
       );
__declspec(dllimport) PyObject * PyCodec_StreamWriter(
       const char *encoding,
       PyObject *stream,
       const char *errors
       );
__declspec(dllimport) int PyCodec_RegisterError(const char *name, PyObject *error);
__declspec(dllimport) PyObject * PyCodec_LookupError(const char *name);
__declspec(dllimport) PyObject * PyCodec_StrictErrors(PyObject *exc);
__declspec(dllimport) PyObject * PyCodec_IgnoreErrors(PyObject *exc);
__declspec(dllimport) PyObject * PyCodec_ReplaceErrors(PyObject *exc);
__declspec(dllimport) PyObject * PyCodec_XMLCharRefReplaceErrors(PyObject *exc);
__declspec(dllimport) PyObject * PyCodec_BackslashReplaceErrors(PyObject *exc);
__declspec(dllimport) PyObject * PyCodec_NameReplaceErrors(PyObject *exc);
typedef void *PyThread_type_lock;
typedef enum PyLockStatus {
    PY_LOCK_FAILURE = 0,
    PY_LOCK_ACQUIRED = 1,
    PY_LOCK_INTR
} PyLockStatus;
__declspec(dllimport) void PyThread_init_thread(void);
__declspec(dllimport) unsigned long PyThread_start_new_thread(void (*)(void *), void *);
__declspec(dllimport) void __attribute__((__noreturn__)) PyThread_exit_thread(void);
__declspec(dllimport) unsigned long PyThread_get_thread_ident(void);
__declspec(dllimport) unsigned long PyThread_get_thread_native_id(void);
__declspec(dllimport) PyThread_type_lock PyThread_allocate_lock(void);
__declspec(dllimport) void PyThread_free_lock(PyThread_type_lock);
__declspec(dllimport) int PyThread_acquire_lock(PyThread_type_lock, int);
__declspec(dllimport) PyLockStatus PyThread_acquire_lock_timed(PyThread_type_lock,
                                                     long long microseconds,
                                                     int intr_flag);
__declspec(dllimport) void PyThread_release_lock(PyThread_type_lock);
__declspec(dllimport) size_t PyThread_get_stacksize(void);
__declspec(dllimport) int PyThread_set_stacksize(size_t);
__declspec(dllimport) PyObject* PyThread_GetInfo(void);
__declspec(deprecated( "deprecated in " "3.7")) __declspec(dllimport) int PyThread_create_key(void);
__declspec(deprecated( "deprecated in " "3.7")) __declspec(dllimport) void PyThread_delete_key(int key);
__declspec(deprecated( "deprecated in " "3.7")) __declspec(dllimport) int PyThread_set_key_value(int key,
                                                          void *value);
__declspec(deprecated( "deprecated in " "3.7")) __declspec(dllimport) void * PyThread_get_key_value(int key);
__declspec(deprecated( "deprecated in " "3.7")) __declspec(dllimport) void PyThread_delete_key_value(int key);
__declspec(deprecated( "deprecated in " "3.7")) __declspec(dllimport) void PyThread_ReInitTLS(void);
typedef struct _Py_tss_t Py_tss_t;
__declspec(dllimport) Py_tss_t * PyThread_tss_alloc(void);
__declspec(dllimport) void PyThread_tss_free(Py_tss_t *key);
__declspec(dllimport) int PyThread_tss_is_created(Py_tss_t *key);
__declspec(dllimport) int PyThread_tss_create(Py_tss_t *key);
__declspec(dllimport) void PyThread_tss_delete(Py_tss_t *key);
__declspec(dllimport) int PyThread_tss_set(Py_tss_t *key, void *value);
__declspec(dllimport) void * PyThread_tss_get(Py_tss_t *key);
__declspec(dllimport) int _PyArg_Parse_SizeT(PyObject *, const char *, ...);
__declspec(dllimport) int _PyArg_ParseTuple_SizeT(PyObject *, const char *, ...);
__declspec(dllimport) int _PyArg_ParseTupleAndKeywords_SizeT(PyObject *, PyObject *,
                                                  const char *, char **, ...);
__declspec(dllimport) int _PyArg_VaParse_SizeT(PyObject *, const char *, va_list);
__declspec(dllimport) int _PyArg_VaParseTupleAndKeywords_SizeT(PyObject *, PyObject *,
                                                  const char *, char **, va_list);
__declspec(dllimport) int PyArg_ValidateKeywordArguments(PyObject *);
__declspec(dllimport) int PyArg_UnpackTuple(PyObject *, const char *, Py_ssize_t, Py_ssize_t, ...);
__declspec(dllimport) PyObject * _Py_BuildValue_SizeT(const char *, ...);
__declspec(dllimport) PyObject * _Py_BuildValue_SizeT(const char *, ...);
__declspec(dllimport) PyObject * _Py_VaBuildValue_SizeT(const char *, va_list);
__declspec(dllimport) int PyModule_AddObjectRef(PyObject *mod, const char *name, PyObject *value);
__declspec(dllimport) int PyModule_AddObject(PyObject *mod, const char *, PyObject *value);
__declspec(dllimport) int PyModule_AddIntConstant(PyObject *, const char *, long);
__declspec(dllimport) int PyModule_AddStringConstant(PyObject *, const char *, const char *);
__declspec(dllimport) int PyModule_AddType(PyObject *module, PyTypeObject *type);
__declspec(dllimport) int PyModule_SetDocString(PyObject *, const char *);
__declspec(dllimport) int PyModule_AddFunctions(PyObject *, PyMethodDef *);
__declspec(dllimport) int PyModule_ExecDef(PyObject *module, PyModuleDef *def);
__declspec(dllimport) PyObject * PyModule_Create2(PyModuleDef*, int apiver);
__declspec(dllimport) PyObject * PyModule_FromDefAndSpec2(PyModuleDef *def,
                                                PyObject *spec,
                                                int module_api_version);
__declspec(dllimport) PyObject * Py_CompileString(const char *, const char *, int);
__declspec(dllimport) void PyErr_Print(void);
__declspec(dllimport) void PyErr_PrintEx(int);
__declspec(dllimport) void PyErr_Display(PyObject *, PyObject *, PyObject *);
__declspec(dllimport) void PyErr_DisplayException(PyObject *);
extern __declspec(dllimport) int (*PyOS_InputHook)(void);
__declspec(dllimport) void Py_Initialize(void);
__declspec(dllimport) void Py_InitializeEx(int);
__declspec(dllimport) void Py_Finalize(void);
__declspec(dllimport) int Py_FinalizeEx(void);
__declspec(dllimport) int Py_IsInitialized(void);
__declspec(dllimport) PyThreadState * Py_NewInterpreter(void);
__declspec(dllimport) void Py_EndInterpreter(PyThreadState *);
__declspec(dllimport) int Py_AtExit(void (*func)(void));
__declspec(dllimport) void __attribute__((__noreturn__)) Py_Exit(int);
__declspec(dllimport) int Py_Main(int argc, wchar_t **argv);
__declspec(dllimport) int Py_BytesMain(int argc, char **argv);
__declspec(deprecated( "deprecated in " "3.11")) __declspec(dllimport) void Py_SetProgramName(const wchar_t *);
__declspec(dllimport) wchar_t * Py_GetProgramName(void);
__declspec(deprecated( "deprecated in " "3.11")) __declspec(dllimport) void Py_SetPythonHome(const wchar_t *);
__declspec(dllimport) wchar_t * Py_GetPythonHome(void);
__declspec(dllimport) wchar_t * Py_GetProgramFullPath(void);
__declspec(dllimport) wchar_t * Py_GetPrefix(void);
__declspec(dllimport) wchar_t * Py_GetExecPrefix(void);
__declspec(dllimport) wchar_t * Py_GetPath(void);
__declspec(deprecated( "deprecated in " "3.11")) __declspec(dllimport) void Py_SetPath(const wchar_t *);
int _Py_CheckPython3(void);
__declspec(dllimport) const char * Py_GetVersion(void);
__declspec(dllimport) const char * Py_GetPlatform(void);
__declspec(dllimport) const char * Py_GetCopyright(void);
__declspec(dllimport) const char * Py_GetCompiler(void);
__declspec(dllimport) const char * Py_GetBuildInfo(void);
typedef void (*PyOS_sighandler_t)(int);
__declspec(dllimport) PyOS_sighandler_t PyOS_getsig(int);
__declspec(dllimport) PyOS_sighandler_t PyOS_setsig(int, PyOS_sighandler_t);
extern __declspec(dllimport) const unsigned long Py_Version;
__declspec(dllimport) PyObject * PyEval_EvalCode(PyObject *, PyObject *, PyObject *);
__declspec(dllimport) PyObject * PyEval_EvalCodeEx(PyObject *co,
                                         PyObject *globals,
                                         PyObject *locals,
                                         PyObject *const *args, int argc,
                                         PyObject *const *kwds, int kwdc,
                                         PyObject *const *defs, int defc,
                                         PyObject *kwdefs, PyObject *closure);
__declspec(deprecated( "deprecated in " "3.9")) __declspec(dllimport) PyObject * PyEval_CallObjectWithKeywords(
    PyObject *callable,
    PyObject *args,
    PyObject *kwargs);
__declspec(deprecated( "deprecated in " "3.9")) __declspec(dllimport) PyObject * PyEval_CallFunction(
    PyObject *callable, const char *format, ...);
__declspec(deprecated( "deprecated in " "3.9")) __declspec(dllimport) PyObject * PyEval_CallMethod(
    PyObject *obj, const char *name, const char *format, ...);
__declspec(dllimport) PyObject * PyEval_GetBuiltins(void);
__declspec(dllimport) PyObject * PyEval_GetGlobals(void);
__declspec(dllimport) PyObject * PyEval_GetLocals(void);
__declspec(dllimport) PyFrameObject * PyEval_GetFrame(void);
__declspec(dllimport) int Py_AddPendingCall(int (*func)(void *), void *arg);
__declspec(dllimport) int Py_MakePendingCalls(void);
__declspec(dllimport) void Py_SetRecursionLimit(int);
__declspec(dllimport) int Py_GetRecursionLimit(void);
__declspec(dllimport) int Py_EnterRecursiveCall(const char *where);
__declspec(dllimport) void Py_LeaveRecursiveCall(void);
__declspec(dllimport) const char * PyEval_GetFuncName(PyObject *);
__declspec(dllimport) const char * PyEval_GetFuncDesc(PyObject *);
__declspec(dllimport) PyObject * PyEval_EvalFrame(PyFrameObject *);
__declspec(dllimport) PyObject * PyEval_EvalFrameEx(PyFrameObject *f, int exc);
__declspec(dllimport) PyThreadState * PyEval_SaveThread(void);
__declspec(dllimport) void PyEval_RestoreThread(PyThreadState *);
__declspec(deprecated( "deprecated in " "3.9")) __declspec(dllimport) int PyEval_ThreadsInitialized(void);
__declspec(deprecated( "deprecated in " "3.9")) __declspec(dllimport) void PyEval_InitThreads(void);
__declspec(deprecated( "deprecated in " "3.2")) __declspec(dllimport) void PyEval_AcquireLock(void);
__declspec(deprecated( "deprecated in " "3.2")) __declspec(dllimport) void PyEval_ReleaseLock(void);
__declspec(dllimport) void PyEval_AcquireThread(PyThreadState *tstate);
__declspec(dllimport) void PyEval_ReleaseThread(PyThreadState *tstate);
__declspec(dllimport) PyObject * PySys_GetObject(const char *);
__declspec(dllimport) int PySys_SetObject(const char *, PyObject *);
__declspec(deprecated( "deprecated in " "3.11")) __declspec(dllimport) void PySys_SetArgv(int, wchar_t **);
__declspec(deprecated( "deprecated in " "3.11")) __declspec(dllimport) void PySys_SetArgvEx(int, wchar_t **, int);
__declspec(deprecated( "deprecated in " "3.11")) __declspec(dllimport) void PySys_SetPath(const wchar_t *);
__declspec(dllimport) void PySys_WriteStdout(const char *format, ...)
                                                         ;
__declspec(dllimport) void PySys_WriteStderr(const char *format, ...)
                                                         ;
__declspec(dllimport) void PySys_FormatStdout(const char *format, ...);
__declspec(dllimport) void PySys_FormatStderr(const char *format, ...);
__declspec(dllimport) void PySys_ResetWarnOptions(void);
__declspec(deprecated( "deprecated in " "3.11")) __declspec(dllimport) void PySys_AddWarnOption(const wchar_t *);
__declspec(deprecated( "deprecated in " "3.11")) __declspec(dllimport) void PySys_AddWarnOptionUnicode(PyObject *);
__declspec(deprecated( "deprecated in " "3.11")) __declspec(dllimport) int PySys_HasWarnOptions(void);
__declspec(deprecated( "deprecated in " "3.11")) __declspec(dllimport) void PySys_AddXOption(const wchar_t *);
__declspec(dllimport) PyObject * PySys_GetXOptions(void);
__declspec(dllimport) PyObject * PyOS_FSPath(PyObject *path);
__declspec(dllimport) int PyOS_InterruptOccurred(void);
__declspec(deprecated( "deprecated in " "3.7")) __declspec(dllimport) void PyOS_AfterFork(void);
__declspec(dllimport) long PyImport_GetMagicNumber(void);
__declspec(dllimport) const char * PyImport_GetMagicTag(void);
__declspec(dllimport) PyObject * PyImport_ExecCodeModule(
    const char *name,
    PyObject *co
    );
__declspec(dllimport) PyObject * PyImport_ExecCodeModuleEx(
    const char *name,
    PyObject *co,
    const char *pathname
    );
__declspec(dllimport) PyObject * PyImport_ExecCodeModuleWithPathnames(
    const char *name,
    PyObject *co,
    const char *pathname,
    const char *cpathname
    );
__declspec(dllimport) PyObject * PyImport_ExecCodeModuleObject(
    PyObject *name,
    PyObject *co,
    PyObject *pathname,
    PyObject *cpathname
    );
__declspec(dllimport) PyObject * PyImport_GetModuleDict(void);
__declspec(dllimport) PyObject * PyImport_GetModule(PyObject *name);
__declspec(dllimport) PyObject * PyImport_AddModuleObject(
    PyObject *name
    );
__declspec(dllimport) PyObject * PyImport_AddModule(
    const char *name
    );
__declspec(dllimport) PyObject * PyImport_ImportModule(
    const char *name
    );
__declspec(dllimport) PyObject * PyImport_ImportModuleNoBlock(
    const char *name
    );
__declspec(dllimport) PyObject * PyImport_ImportModuleLevel(
    const char *name,
    PyObject *globals,
    PyObject *locals,
    PyObject *fromlist,
    int level
    );
__declspec(dllimport) PyObject * PyImport_ImportModuleLevelObject(
    PyObject *name,
    PyObject *globals,
    PyObject *locals,
    PyObject *fromlist,
    int level
    );
__declspec(dllimport) PyObject * PyImport_GetImporter(PyObject *path);
__declspec(dllimport) PyObject * PyImport_Import(PyObject *name);
__declspec(dllimport) PyObject * PyImport_ReloadModule(PyObject *m);
__declspec(dllimport) int PyImport_ImportFrozenModuleObject(
    PyObject *name
    );
__declspec(dllimport) int PyImport_ImportFrozenModule(
    const char *name
    );
__declspec(dllimport) int PyImport_AppendInittab(
    const char *name,
    PyObject* (*initfunc)(void)
    );
__declspec(dllimport) PyObject * PyObject_CallNoArgs(PyObject *func);
__declspec(dllimport) PyObject * PyObject_Call(PyObject *callable,
                                     PyObject *args, PyObject *kwargs);
__declspec(dllimport) PyObject * PyObject_CallObject(PyObject *callable,
                                           PyObject *args);
__declspec(dllimport) PyObject * _PyObject_CallFunction_SizeT(PyObject *callable,
                                             const char *format, ...);
__declspec(dllimport) PyObject * _PyObject_CallMethod_SizeT(PyObject *obj,
                                           const char *name,
                                           const char *format, ...);
__declspec(dllimport) PyObject * _PyObject_CallFunction_SizeT(PyObject *callable,
                                                    const char *format,
                                                    ...);
__declspec(dllimport) PyObject * _PyObject_CallMethod_SizeT(PyObject *obj,
                                                  const char *name,
                                                  const char *format,
                                                  ...);
__declspec(dllimport) PyObject * PyObject_CallFunctionObjArgs(PyObject *callable,
                                                    ...);
__declspec(dllimport) PyObject * PyObject_CallMethodObjArgs(
    PyObject *obj,
    PyObject *name,
    ...);
__declspec(dllimport) Py_ssize_t PyVectorcall_NARGS(size_t nargsf);
__declspec(dllimport) PyObject * PyVectorcall_Call(PyObject *callable, PyObject *tuple, PyObject *dict);
__declspec(dllimport) PyObject * PyObject_Vectorcall(
    PyObject *callable,
    PyObject *const *args,
    size_t nargsf,
    PyObject *kwnames);
__declspec(dllimport) PyObject * PyObject_VectorcallMethod(
    PyObject *name, PyObject *const *args,
    size_t nargsf, PyObject *kwnames);
__declspec(dllimport) PyObject * PyObject_Type(PyObject *o);
__declspec(dllimport) Py_ssize_t PyObject_Size(PyObject *o);
__declspec(dllimport) Py_ssize_t PyObject_Length(PyObject *o);
__declspec(dllimport) PyObject * PyObject_GetItem(PyObject *o, PyObject *key);
__declspec(dllimport) int PyObject_SetItem(PyObject *o, PyObject *key, PyObject *v);
__declspec(dllimport) int PyObject_DelItemString(PyObject *o, const char *key);
__declspec(dllimport) int PyObject_DelItem(PyObject *o, PyObject *key);
__declspec(deprecated( "deprecated in " "3.0"))
__declspec(dllimport) int PyObject_AsCharBuffer(PyObject *obj,
                                      const char **buffer,
                                      Py_ssize_t *buffer_len);
__declspec(deprecated( "deprecated in " "3.0")) __declspec(dllimport) int PyObject_CheckReadBuffer(PyObject *obj);
__declspec(deprecated( "deprecated in " "3.0"))
__declspec(dllimport) int PyObject_AsReadBuffer(PyObject *obj,
                                      const void **buffer,
                                      Py_ssize_t *buffer_len);
__declspec(deprecated( "deprecated in " "3.0"))
__declspec(dllimport) int PyObject_AsWriteBuffer(PyObject *obj,
                                       void **buffer,
                                       Py_ssize_t *buffer_len);
__declspec(dllimport) PyObject * PyObject_Format(PyObject *obj,
                                       PyObject *format_spec);
__declspec(dllimport) PyObject * PyObject_GetIter(PyObject *);
__declspec(dllimport) PyObject * PyObject_GetAIter(PyObject *);
__declspec(dllimport) int PyIter_Check(PyObject *);
__declspec(dllimport) int PyAIter_Check(PyObject *);
__declspec(dllimport) PyObject * PyIter_Next(PyObject *);
__declspec(dllimport) PySendResult PyIter_Send(PyObject *, PyObject *, PyObject **);
__declspec(dllimport) int PyNumber_Check(PyObject *o);
__declspec(dllimport) PyObject * PyNumber_Add(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_Subtract(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_Multiply(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_MatrixMultiply(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_FloorDivide(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_TrueDivide(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_Remainder(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_Divmod(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_Power(PyObject *o1, PyObject *o2,
                                      PyObject *o3);
__declspec(dllimport) PyObject * PyNumber_Negative(PyObject *o);
__declspec(dllimport) PyObject * PyNumber_Positive(PyObject *o);
__declspec(dllimport) PyObject * PyNumber_Absolute(PyObject *o);
__declspec(dllimport) PyObject * PyNumber_Invert(PyObject *o);
__declspec(dllimport) PyObject * PyNumber_Lshift(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_Rshift(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_And(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_Xor(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_Or(PyObject *o1, PyObject *o2);
__declspec(dllimport) int PyIndex_Check(PyObject *);
__declspec(dllimport) PyObject * PyNumber_Index(PyObject *o);
__declspec(dllimport) Py_ssize_t PyNumber_AsSsize_t(PyObject *o, PyObject *exc);
__declspec(dllimport) PyObject * PyNumber_Long(PyObject *o);
__declspec(dllimport) PyObject * PyNumber_Float(PyObject *o);
__declspec(dllimport) PyObject * PyNumber_InPlaceAdd(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_InPlaceSubtract(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_InPlaceMultiply(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_InPlaceMatrixMultiply(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_InPlaceFloorDivide(PyObject *o1,
                                                   PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_InPlaceTrueDivide(PyObject *o1,
                                                  PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_InPlaceRemainder(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_InPlacePower(PyObject *o1, PyObject *o2,
                                             PyObject *o3);
__declspec(dllimport) PyObject * PyNumber_InPlaceLshift(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_InPlaceRshift(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_InPlaceAnd(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_InPlaceXor(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_InPlaceOr(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PyNumber_ToBase(PyObject *n, int base);
__declspec(dllimport) int PySequence_Check(PyObject *o);
__declspec(dllimport) Py_ssize_t PySequence_Size(PyObject *o);
__declspec(dllimport) Py_ssize_t PySequence_Length(PyObject *o);
__declspec(dllimport) PyObject * PySequence_Concat(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PySequence_Repeat(PyObject *o, Py_ssize_t count);
__declspec(dllimport) PyObject * PySequence_GetItem(PyObject *o, Py_ssize_t i);
__declspec(dllimport) PyObject * PySequence_GetSlice(PyObject *o, Py_ssize_t i1, Py_ssize_t i2);
__declspec(dllimport) int PySequence_SetItem(PyObject *o, Py_ssize_t i, PyObject *v);
__declspec(dllimport) int PySequence_DelItem(PyObject *o, Py_ssize_t i);
__declspec(dllimport) int PySequence_SetSlice(PyObject *o, Py_ssize_t i1, Py_ssize_t i2,
                                    PyObject *v);
__declspec(dllimport) int PySequence_DelSlice(PyObject *o, Py_ssize_t i1, Py_ssize_t i2);
__declspec(dllimport) PyObject * PySequence_Tuple(PyObject *o);
__declspec(dllimport) PyObject * PySequence_List(PyObject *o);
__declspec(dllimport) PyObject * PySequence_Fast(PyObject *o, const char* m);
__declspec(dllimport) Py_ssize_t PySequence_Count(PyObject *o, PyObject *value);
__declspec(dllimport) int PySequence_Contains(PyObject *seq, PyObject *ob);
__declspec(dllimport) int PySequence_In(PyObject *o, PyObject *value);
__declspec(dllimport) Py_ssize_t PySequence_Index(PyObject *o, PyObject *value);
__declspec(dllimport) PyObject * PySequence_InPlaceConcat(PyObject *o1, PyObject *o2);
__declspec(dllimport) PyObject * PySequence_InPlaceRepeat(PyObject *o, Py_ssize_t count);
__declspec(dllimport) int PyMapping_Check(PyObject *o);
__declspec(dllimport) Py_ssize_t PyMapping_Size(PyObject *o);
__declspec(dllimport) Py_ssize_t PyMapping_Length(PyObject *o);
__declspec(dllimport) int PyMapping_HasKeyString(PyObject *o, const char *key);
__declspec(dllimport) int PyMapping_HasKey(PyObject *o, PyObject *key);
__declspec(dllimport) PyObject * PyMapping_Keys(PyObject *o);
__declspec(dllimport) PyObject * PyMapping_Values(PyObject *o);
__declspec(dllimport) PyObject * PyMapping_Items(PyObject *o);
__declspec(dllimport) PyObject * PyMapping_GetItemString(PyObject *o,
                                               const char *key);
__declspec(dllimport) int PyMapping_SetItemString(PyObject *o, const char *key,
                                        PyObject *value);
__declspec(dllimport) int PyObject_IsInstance(PyObject *object, PyObject *typeorclass);
__declspec(dllimport) int PyObject_IsSubclass(PyObject *object, PyObject *typeorclass);
extern __declspec(dllimport) PyTypeObject PyFilter_Type;
extern __declspec(dllimport) PyTypeObject PyMap_Type;
extern __declspec(dllimport) PyTypeObject PyZip_Type;
__declspec(dllimport) double PyOS_string_to_double(const char *str,
                                         char **endptr,
                                         PyObject *overflow_exception);
__declspec(dllimport) char * PyOS_double_to_string(double val,
                                         char format_code,
                                         int precision,
                                         int flags,
                                         int *type);
__declspec(dllimport) int PyOS_mystrnicmp(const char *, const char *, Py_ssize_t);
__declspec(dllimport) int PyOS_mystricmp(const char *, const char *);
__declspec(dllimport) wchar_t * Py_DecodeLocale(
    const char *arg,
    size_t *size);
__declspec(dllimport) char* Py_EncodeLocale(
    const wchar_t *text,
    size_t *error_pos);
