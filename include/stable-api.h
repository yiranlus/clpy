typedef signed char int8_t;
typedef unsigned char uint8_t;
typedef signed short int16_t;
typedef unsigned short uint16_t;
typedef signed int int32_t;
typedef unsigned int uint32_t;
typedef signed int wchar_t;
typedef signed long int64_t;
typedef unsigned long uint64_t;
typedef signed long intptr_t;
typedef unsigned long uintptr_t;
typedef unsigned long size_t;
typedef signed long Py_ssize_t;















typedef uintptr_t Py_uintptr_t;
typedef intptr_t Py_intptr_t;
typedef ssize_t Py_ssize_t;
typedef Py_ssize_t Py_hash_t;


typedef size_t Py_uhash_t;


typedef Py_ssize_t Py_ssize_clean_t;








__attribute__ ((visibility ("default"))) void * PyMem_Malloc(size_t size);
__attribute__ ((visibility ("default"))) void * PyMem_Calloc(size_t nelem, size_t elsize);
__attribute__ ((visibility ("default"))) void * PyMem_Realloc(void *ptr, size_t new_size);
__attribute__ ((visibility ("default"))) void PyMem_Free(void *ptr);
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


__attribute__ ((visibility ("default"))) int PyObject_CheckBuffer(PyObject *obj);






__attribute__ ((visibility ("default"))) int PyObject_GetBuffer(PyObject *obj, Py_buffer *view,
                                   int flags);



__attribute__ ((visibility ("default"))) void * PyBuffer_GetPointer(const Py_buffer *view, const Py_ssize_t *indices);



__attribute__ ((visibility ("default"))) Py_ssize_t PyBuffer_SizeFromFormat(const char *format);


__attribute__ ((visibility ("default"))) int PyBuffer_ToContiguous(void *buf, const Py_buffer *view,
                                      Py_ssize_t len, char order);

__attribute__ ((visibility ("default"))) int PyBuffer_FromContiguous(const Py_buffer *view, const void *buf,
                                        Py_ssize_t len, char order);
__attribute__ ((visibility ("default"))) int PyObject_CopyData(PyObject *dest, PyObject *src);


__attribute__ ((visibility ("default"))) int PyBuffer_IsContiguous(const Py_buffer *view, char fort);





__attribute__ ((visibility ("default"))) void PyBuffer_FillContiguousStrides(int ndims,
                                               Py_ssize_t *shape,
                                               Py_ssize_t *strides,
                                               int itemsize,
                                               char fort);






__attribute__ ((visibility ("default"))) int PyBuffer_FillInfo(Py_buffer *view, PyObject *o, void *buf,
                                  Py_ssize_t len, int readonly,
                                  int flags);


__attribute__ ((visibility ("default"))) void PyBuffer_Release(Py_buffer *view);
struct _object {
   
    Py_ssize_t ob_refcnt;
    PyTypeObject *ob_type;
};




typedef struct {
    PyObject ob_base;
    Py_ssize_t ob_size;
} PyVarObject;






__attribute__ ((visibility ("default"))) int Py_Is(PyObject *x, PyObject *y);



static inline Py_ssize_t Py_REFCNT(PyObject *ob) {
    return ob->ob_refcnt;
}






static inline PyTypeObject* Py_TYPE(PyObject *ob) {
    return ob->ob_type;
}





static inline Py_ssize_t Py_SIZE(PyObject *ob) {
    PyVarObject *var_ob = ((PyVarObject*)((ob)));
    return var_ob->ob_size;
}





static inline int Py_IS_TYPE(PyObject *ob, PyTypeObject *type) {
    return Py_TYPE(ob) == type;
}





static inline void Py_SET_REFCNT(PyObject *ob, Py_ssize_t refcnt) {
    ob->ob_refcnt = refcnt;
}





static inline void Py_SET_TYPE(PyObject *ob, PyTypeObject *type) {
    ob->ob_type = type;
}





static inline void Py_SET_SIZE(PyVarObject *ob, Py_ssize_t size) {
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

__attribute__ ((visibility ("default"))) PyObject* PyType_FromSpec(PyType_Spec*);

__attribute__ ((visibility ("default"))) PyObject* PyType_FromSpecWithBases(PyType_Spec*, PyObject*);


__attribute__ ((visibility ("default"))) void* PyType_GetSlot(PyTypeObject*, int);


__attribute__ ((visibility ("default"))) PyObject* PyType_FromModuleAndSpec(PyObject *, PyType_Spec *, PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyType_GetModule(PyTypeObject *);
__attribute__ ((visibility ("default"))) void * PyType_GetModuleState(PyTypeObject *);


__attribute__ ((visibility ("default"))) PyObject * PyType_GetName(PyTypeObject *);
__attribute__ ((visibility ("default"))) PyObject * PyType_GetQualName(PyTypeObject *);



__attribute__ ((visibility ("default"))) int PyType_IsSubtype(PyTypeObject *, PyTypeObject *);

static inline int PyObject_TypeCheck(PyObject *ob, PyTypeObject *type) {
    return Py_IS_TYPE(ob, type) || PyType_IsSubtype(Py_TYPE(ob), type);
}




extern __attribute__ ((visibility ("default"))) PyTypeObject PyType_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyBaseObject_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PySuper_Type;

__attribute__ ((visibility ("default"))) unsigned long PyType_GetFlags(PyTypeObject*);

__attribute__ ((visibility ("default"))) int PyType_Ready(PyTypeObject *);
__attribute__ ((visibility ("default"))) PyObject * PyType_GenericAlloc(PyTypeObject *, Py_ssize_t);
__attribute__ ((visibility ("default"))) PyObject * PyType_GenericNew(PyTypeObject *,
                                               PyObject *, PyObject *);
__attribute__ ((visibility ("default"))) unsigned int PyType_ClearCache(void);
__attribute__ ((visibility ("default"))) void PyType_Modified(PyTypeObject *);


__attribute__ ((visibility ("default"))) PyObject * PyObject_Repr(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyObject_Str(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyObject_ASCII(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyObject_Bytes(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyObject_RichCompare(PyObject *, PyObject *, int);
__attribute__ ((visibility ("default"))) int PyObject_RichCompareBool(PyObject *, PyObject *, int);
__attribute__ ((visibility ("default"))) PyObject * PyObject_GetAttrString(PyObject *, const char *);
__attribute__ ((visibility ("default"))) int PyObject_SetAttrString(PyObject *, const char *, PyObject *);
__attribute__ ((visibility ("default"))) int PyObject_HasAttrString(PyObject *, const char *);
__attribute__ ((visibility ("default"))) PyObject * PyObject_GetAttr(PyObject *, PyObject *);
__attribute__ ((visibility ("default"))) int PyObject_SetAttr(PyObject *, PyObject *, PyObject *);
__attribute__ ((visibility ("default"))) int PyObject_HasAttr(PyObject *, PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyObject_SelfIter(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyObject_GenericGetAttr(PyObject *, PyObject *);
__attribute__ ((visibility ("default"))) int PyObject_GenericSetAttr(PyObject *, PyObject *, PyObject *);

__attribute__ ((visibility ("default"))) int PyObject_GenericSetDict(PyObject *, PyObject *, void *);

__attribute__ ((visibility ("default"))) Py_hash_t PyObject_Hash(PyObject *);
__attribute__ ((visibility ("default"))) Py_hash_t PyObject_HashNotImplemented(PyObject *);
__attribute__ ((visibility ("default"))) int PyObject_IsTrue(PyObject *);
__attribute__ ((visibility ("default"))) int PyObject_Not(PyObject *);
__attribute__ ((visibility ("default"))) int PyCallable_Check(PyObject *);
__attribute__ ((visibility ("default"))) void PyObject_ClearWeakRefs(PyObject *);






__attribute__ ((visibility ("default"))) PyObject * PyObject_Dir(PyObject *);
__attribute__ ((visibility ("default"))) int Py_ReprEnter(PyObject *);
__attribute__ ((visibility ("default"))) void Py_ReprLeave(PyObject *);
__attribute__ ((visibility ("default"))) void _Py_Dealloc(PyObject *);





__attribute__ ((visibility ("default"))) void Py_IncRef(PyObject *);
__attribute__ ((visibility ("default"))) void Py_DecRef(PyObject *);



__attribute__ ((visibility ("default"))) void _Py_IncRef(PyObject *);
__attribute__ ((visibility ("default"))) void _Py_DecRef(PyObject *);

static inline void Py_INCREF(PyObject *op)
{
    op->ob_refcnt++;

}
static inline void Py_DECREF(PyObject *op)
{


    if (--op->ob_refcnt == 0) {
        _Py_Dealloc(op);
    }
}
static inline void Py_XINCREF(PyObject *op)
{
    if (op != 
                     ) {
        Py_INCREF(op);
    }
}




static inline void Py_XDECREF(PyObject *op)
{
    if (op != 
                     ) {
        Py_DECREF(((PyObject*)((op))));
    }
}






__attribute__ ((visibility ("default"))) PyObject* Py_NewRef(PyObject *obj);


__attribute__ ((visibility ("default"))) PyObject* Py_XNewRef(PyObject *obj);

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
extern __attribute__ ((visibility ("default"))) PyObject _Py_NoneStruct;



__attribute__ ((visibility ("default"))) int Py_IsNone(PyObject *x);
extern __attribute__ ((visibility ("default"))) PyObject _Py_NotImplementedStruct;
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
    return PyType_HasFeature(Py_TYPE(op), (1UL << 31));
}







static inline int PyType_CheckExact(PyObject *op) {
    return Py_IS_TYPE(op, &PyType_Type);
}
__attribute__ ((visibility ("default"))) void * PyObject_Malloc(size_t size);

__attribute__ ((visibility ("default"))) void * PyObject_Calloc(size_t nelem, size_t elsize);

__attribute__ ((visibility ("default"))) void * PyObject_Realloc(void *ptr, size_t new_size);
__attribute__ ((visibility ("default"))) void PyObject_Free(void *ptr);
__attribute__ ((visibility ("default"))) PyObject * PyObject_Init(PyObject *, PyTypeObject *);
__attribute__ ((visibility ("default"))) PyVarObject * PyObject_InitVar(PyVarObject *,
                                           PyTypeObject *, Py_ssize_t);







__attribute__ ((visibility ("default"))) PyObject * _PyObject_New(PyTypeObject *);
__attribute__ ((visibility ("default"))) PyVarObject * _PyObject_NewVar(PyTypeObject *, Py_ssize_t);
__attribute__ ((visibility ("default"))) Py_ssize_t PyGC_Collect(void);

__attribute__ ((visibility ("default"))) int PyGC_Enable(void);
__attribute__ ((visibility ("default"))) int PyGC_Disable(void);
__attribute__ ((visibility ("default"))) int PyGC_IsEnabled(void);




__attribute__ ((visibility ("default"))) PyVarObject * _PyObject_GC_Resize(PyVarObject *, Py_ssize_t);





__attribute__ ((visibility ("default"))) PyObject * _PyObject_GC_New(PyTypeObject *);
__attribute__ ((visibility ("default"))) PyVarObject * _PyObject_GC_NewVar(PyTypeObject *, Py_ssize_t);




__attribute__ ((visibility ("default"))) void PyObject_GC_Track(void *);




__attribute__ ((visibility ("default"))) void PyObject_GC_UnTrack(void *);

__attribute__ ((visibility ("default"))) void PyObject_GC_Del(void *);






__attribute__ ((visibility ("default"))) int PyObject_GC_IsTracked(PyObject *);
__attribute__ ((visibility ("default"))) int PyObject_GC_IsFinalized(PyObject *);
extern __attribute__ ((visibility ("default"))) PyTypeObject PyByteArray_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyByteArrayIter_Type;






__attribute__ ((visibility ("default"))) PyObject * PyByteArray_FromObject(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyByteArray_Concat(PyObject *, PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyByteArray_FromStringAndSize(const char *, Py_ssize_t);
__attribute__ ((visibility ("default"))) Py_ssize_t PyByteArray_Size(PyObject *);
__attribute__ ((visibility ("default"))) char * PyByteArray_AsString(PyObject *);
__attribute__ ((visibility ("default"))) int PyByteArray_Resize(PyObject *, Py_ssize_t);
extern __attribute__ ((visibility ("default"))) PyTypeObject PyBytes_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyBytesIter_Type;





__attribute__ ((visibility ("default"))) PyObject * PyBytes_FromStringAndSize(const char *, Py_ssize_t);
__attribute__ ((visibility ("default"))) PyObject * PyBytes_FromString(const char *);
__attribute__ ((visibility ("default"))) PyObject * PyBytes_FromObject(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyBytes_FromFormatV(const char*, va_list)
                                __attribute__((format(printf, 1, 0)));
__attribute__ ((visibility ("default"))) PyObject * PyBytes_FromFormat(const char*, ...)
                                __attribute__((format(printf, 1, 2)));
__attribute__ ((visibility ("default"))) Py_ssize_t PyBytes_Size(PyObject *);
__attribute__ ((visibility ("default"))) char * PyBytes_AsString(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyBytes_Repr(PyObject *, int);
__attribute__ ((visibility ("default"))) void PyBytes_Concat(PyObject **, PyObject *);
__attribute__ ((visibility ("default"))) void PyBytes_ConcatAndDel(PyObject **, PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyBytes_DecodeEscape(const char *, Py_ssize_t,
                                            const char *, Py_ssize_t,
                                            const char *);





__attribute__ ((visibility ("default"))) int PyBytes_AsStringAndSize(
    PyObject *obj,
    char **s,
    Py_ssize_t *len
    );

typedef uint32_t Py_UCS4;
typedef uint16_t Py_UCS2;
typedef uint8_t Py_UCS1;






extern __attribute__ ((visibility ("default"))) PyTypeObject PyUnicode_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyUnicodeIter_Type;
__attribute__ ((visibility ("default"))) PyObject* PyUnicode_FromStringAndSize(
    const char *u,
    Py_ssize_t size
    );



__attribute__ ((visibility ("default"))) PyObject* PyUnicode_FromString(
    const char *u
    );


__attribute__ ((visibility ("default"))) PyObject* PyUnicode_Substring(
    PyObject *str,
    Py_ssize_t start,
    Py_ssize_t end);
__attribute__ ((visibility ("default"))) Py_UCS4* PyUnicode_AsUCS4(
    PyObject *unicode,
    Py_UCS4* buffer,
    Py_ssize_t buflen,
    int copy_null);




__attribute__ ((visibility ("default"))) Py_UCS4* PyUnicode_AsUCS4Copy(PyObject *unicode);





__attribute__ ((visibility ("default"))) Py_ssize_t PyUnicode_GetLength(
    PyObject *unicode
);





__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) Py_ssize_t PyUnicode_GetSize(
    PyObject *unicode
    );




__attribute__ ((visibility ("default"))) Py_UCS4 PyUnicode_ReadChar(
    PyObject *unicode,
    Py_ssize_t index
    );






__attribute__ ((visibility ("default"))) int PyUnicode_WriteChar(
    PyObject *unicode,
    Py_ssize_t index,
    Py_UCS4 character
    );
__attribute__ ((visibility ("default"))) int PyUnicode_Resize(
    PyObject **unicode,
    Py_ssize_t length
    );
__attribute__ ((visibility ("default"))) PyObject* PyUnicode_FromEncodedObject(
    PyObject *obj,
    const char *encoding,
    const char *errors
    );
__attribute__ ((visibility ("default"))) PyObject* PyUnicode_FromObject(
    PyObject *obj
    );

__attribute__ ((visibility ("default"))) PyObject * PyUnicode_FromFormatV(
    const char *format,
    va_list vargs
    );
__attribute__ ((visibility ("default"))) PyObject * PyUnicode_FromFormat(
    const char *format,
    ...
    );

__attribute__ ((visibility ("default"))) void PyUnicode_InternInPlace(PyObject **);
__attribute__ ((visibility ("default"))) PyObject * PyUnicode_InternFromString(
    const char *u
    );



__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) void PyUnicode_InternImmortal(PyObject **);
__attribute__ ((visibility ("default"))) PyObject* PyUnicode_FromWideChar(
    const wchar_t *w,
    Py_ssize_t size
    );
__attribute__ ((visibility ("default"))) Py_ssize_t PyUnicode_AsWideChar(
    PyObject *unicode,
    wchar_t *w,
    Py_ssize_t size
    );
__attribute__ ((visibility ("default"))) wchar_t* PyUnicode_AsWideCharString(
    PyObject *unicode,
    Py_ssize_t *size
    );
__attribute__ ((visibility ("default"))) PyObject* PyUnicode_FromOrdinal(int ordinal);
__attribute__ ((visibility ("default"))) const char* PyUnicode_GetDefaultEncoding(void);






__attribute__ ((visibility ("default"))) PyObject* PyUnicode_Decode(
    const char *s,
    Py_ssize_t size,
    const char *encoding,
    const char *errors
    );
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) PyObject* PyUnicode_AsDecodedObject(
    PyObject *unicode,
    const char *encoding,
    const char *errors
    );
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) PyObject* PyUnicode_AsDecodedUnicode(
    PyObject *unicode,
    const char *encoding,
    const char *errors
    );
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) PyObject* PyUnicode_AsEncodedObject(
    PyObject *unicode,
    const char *encoding,
    const char *errors
    );




__attribute__ ((visibility ("default"))) PyObject* PyUnicode_AsEncodedString(
    PyObject *unicode,
    const char *encoding,
    const char *errors
    );
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) PyObject* PyUnicode_AsEncodedUnicode(
    PyObject *unicode,
    const char *encoding,
    const char *errors
    );



__attribute__ ((visibility ("default"))) PyObject* PyUnicode_BuildEncodingMap(
    PyObject* string
   );



__attribute__ ((visibility ("default"))) PyObject* PyUnicode_DecodeUTF7(
    const char *string,
    Py_ssize_t length,
    const char *errors
    );

__attribute__ ((visibility ("default"))) PyObject* PyUnicode_DecodeUTF7Stateful(
    const char *string,
    Py_ssize_t length,
    const char *errors,
    Py_ssize_t *consumed
    );



__attribute__ ((visibility ("default"))) PyObject* PyUnicode_DecodeUTF8(
    const char *string,
    Py_ssize_t length,
    const char *errors
    );

__attribute__ ((visibility ("default"))) PyObject* PyUnicode_DecodeUTF8Stateful(
    const char *string,
    Py_ssize_t length,
    const char *errors,
    Py_ssize_t *consumed
    );

__attribute__ ((visibility ("default"))) PyObject* PyUnicode_AsUTF8String(
    PyObject *unicode
    );
__attribute__ ((visibility ("default"))) const char * PyUnicode_AsUTF8AndSize(
    PyObject *unicode,
    Py_ssize_t *size);
__attribute__ ((visibility ("default"))) PyObject* PyUnicode_DecodeUTF32(
    const char *string,
    Py_ssize_t length,
    const char *errors,
    int *byteorder


    );

__attribute__ ((visibility ("default"))) PyObject* PyUnicode_DecodeUTF32Stateful(
    const char *string,
    Py_ssize_t length,
    const char *errors,
    int *byteorder,


    Py_ssize_t *consumed
    );




__attribute__ ((visibility ("default"))) PyObject* PyUnicode_AsUTF32String(
    PyObject *unicode
    );
__attribute__ ((visibility ("default"))) PyObject* PyUnicode_DecodeUTF16(
    const char *string,
    Py_ssize_t length,
    const char *errors,
    int *byteorder


    );

__attribute__ ((visibility ("default"))) PyObject* PyUnicode_DecodeUTF16Stateful(
    const char *string,
    Py_ssize_t length,
    const char *errors,
    int *byteorder,


    Py_ssize_t *consumed
    );




__attribute__ ((visibility ("default"))) PyObject* PyUnicode_AsUTF16String(
    PyObject *unicode
    );



__attribute__ ((visibility ("default"))) PyObject* PyUnicode_DecodeUnicodeEscape(
    const char *string,
    Py_ssize_t length,
    const char *errors
    );

__attribute__ ((visibility ("default"))) PyObject* PyUnicode_AsUnicodeEscapeString(
    PyObject *unicode
    );



__attribute__ ((visibility ("default"))) PyObject* PyUnicode_DecodeRawUnicodeEscape(
    const char *string,
    Py_ssize_t length,
    const char *errors
    );

__attribute__ ((visibility ("default"))) PyObject* PyUnicode_AsRawUnicodeEscapeString(
    PyObject *unicode
    );





__attribute__ ((visibility ("default"))) PyObject* PyUnicode_DecodeLatin1(
    const char *string,
    Py_ssize_t length,
    const char *errors
    );

__attribute__ ((visibility ("default"))) PyObject* PyUnicode_AsLatin1String(
    PyObject *unicode
    );







__attribute__ ((visibility ("default"))) PyObject* PyUnicode_DecodeASCII(
    const char *string,
    Py_ssize_t length,
    const char *errors
    );

__attribute__ ((visibility ("default"))) PyObject* PyUnicode_AsASCIIString(
    PyObject *unicode
    );
__attribute__ ((visibility ("default"))) PyObject* PyUnicode_DecodeCharmap(
    const char *string,
    Py_ssize_t length,
    PyObject *mapping,
    const char *errors
    );

__attribute__ ((visibility ("default"))) PyObject* PyUnicode_AsCharmapString(
    PyObject *unicode,
    PyObject *mapping
    );
__attribute__ ((visibility ("default"))) PyObject* PyUnicode_DecodeLocaleAndSize(
    const char *str,
    Py_ssize_t len,
    const char *errors);




__attribute__ ((visibility ("default"))) PyObject* PyUnicode_DecodeLocale(
    const char *str,
    const char *errors);






__attribute__ ((visibility ("default"))) PyObject* PyUnicode_EncodeLocale(
    PyObject *unicode,
    const char *errors
    );







__attribute__ ((visibility ("default"))) int PyUnicode_FSConverter(PyObject*, void*);




__attribute__ ((visibility ("default"))) int PyUnicode_FSDecoder(PyObject*, void*);
__attribute__ ((visibility ("default"))) PyObject* PyUnicode_DecodeFSDefault(
    const char *s
    );
__attribute__ ((visibility ("default"))) PyObject* PyUnicode_DecodeFSDefaultAndSize(
    const char *s,
    Py_ssize_t size
    );
__attribute__ ((visibility ("default"))) PyObject* PyUnicode_EncodeFSDefault(
    PyObject *unicode
    );
__attribute__ ((visibility ("default"))) PyObject* PyUnicode_Concat(
    PyObject *left,
    PyObject *right
    );




__attribute__ ((visibility ("default"))) void PyUnicode_Append(
    PyObject **pleft,
    PyObject *right
    );




__attribute__ ((visibility ("default"))) void PyUnicode_AppendAndDel(
    PyObject **pleft,
    PyObject *right
    );
__attribute__ ((visibility ("default"))) PyObject* PyUnicode_Split(
    PyObject *s,
    PyObject *sep,
    Py_ssize_t maxsplit
    );






__attribute__ ((visibility ("default"))) PyObject* PyUnicode_Splitlines(
    PyObject *s,
    int keepends
    );



__attribute__ ((visibility ("default"))) PyObject* PyUnicode_Partition(
    PyObject *s,
    PyObject *sep
    );




__attribute__ ((visibility ("default"))) PyObject* PyUnicode_RPartition(
    PyObject *s,
    PyObject *sep
    );
__attribute__ ((visibility ("default"))) PyObject* PyUnicode_RSplit(
    PyObject *s,
    PyObject *sep,
    Py_ssize_t maxsplit
    );
__attribute__ ((visibility ("default"))) PyObject * PyUnicode_Translate(
    PyObject *str,
    PyObject *table,
    const char *errors
    );




__attribute__ ((visibility ("default"))) PyObject* PyUnicode_Join(
    PyObject *separator,
    PyObject *seq
    );




__attribute__ ((visibility ("default"))) Py_ssize_t PyUnicode_Tailmatch(
    PyObject *str,
    PyObject *substr,
    Py_ssize_t start,
    Py_ssize_t end,
    int direction
    );





__attribute__ ((visibility ("default"))) Py_ssize_t PyUnicode_Find(
    PyObject *str,
    PyObject *substr,
    Py_ssize_t start,
    Py_ssize_t end,
    int direction
    );



__attribute__ ((visibility ("default"))) Py_ssize_t PyUnicode_FindChar(
    PyObject *str,
    Py_UCS4 ch,
    Py_ssize_t start,
    Py_ssize_t end,
    int direction
    );




__attribute__ ((visibility ("default"))) Py_ssize_t PyUnicode_Count(
    PyObject *str,
    PyObject *substr,
    Py_ssize_t start,
    Py_ssize_t end
    );




__attribute__ ((visibility ("default"))) PyObject * PyUnicode_Replace(
    PyObject *str,
    PyObject *substr,
    PyObject *replstr,
    Py_ssize_t maxcount

    );





__attribute__ ((visibility ("default"))) int PyUnicode_Compare(
    PyObject *left,
    PyObject *right
    );







__attribute__ ((visibility ("default"))) int PyUnicode_CompareWithASCIIString(
    PyObject *left,
    const char *right
    );
__attribute__ ((visibility ("default"))) PyObject * PyUnicode_RichCompare(
    PyObject *left,
    PyObject *right,
    int op
    );




__attribute__ ((visibility ("default"))) PyObject * PyUnicode_Format(
    PyObject *format,
    PyObject *args
    );







__attribute__ ((visibility ("default"))) int PyUnicode_Contains(
    PyObject *container,
    PyObject *element
    );



__attribute__ ((visibility ("default"))) int PyUnicode_IsIdentifier(PyObject *s);
__attribute__ ((visibility ("default"))) PyInterpreterState * PyInterpreterState_New(void);
__attribute__ ((visibility ("default"))) void PyInterpreterState_Clear(PyInterpreterState *);
__attribute__ ((visibility ("default"))) void PyInterpreterState_Delete(PyInterpreterState *);
__attribute__ ((visibility ("default"))) PyInterpreterState * PyInterpreterState_Get(void);




__attribute__ ((visibility ("default"))) PyObject * PyInterpreterState_GetDict(PyInterpreterState *);




__attribute__ ((visibility ("default"))) int64_t PyInterpreterState_GetID(PyInterpreterState *);






__attribute__ ((visibility ("default"))) int PyState_AddModule(PyObject*, PyModuleDef*);
__attribute__ ((visibility ("default"))) int PyState_RemoveModule(PyModuleDef*);

__attribute__ ((visibility ("default"))) PyObject* PyState_FindModule(PyModuleDef*);

__attribute__ ((visibility ("default"))) PyThreadState * PyThreadState_New(PyInterpreterState *);
__attribute__ ((visibility ("default"))) void PyThreadState_Clear(PyThreadState *);
__attribute__ ((visibility ("default"))) void PyThreadState_Delete(PyThreadState *);
__attribute__ ((visibility ("default"))) PyThreadState * PyThreadState_Get(void);




__attribute__ ((visibility ("default"))) PyThreadState * PyThreadState_Swap(PyThreadState *);
__attribute__ ((visibility ("default"))) PyObject * PyThreadState_GetDict(void);
__attribute__ ((visibility ("default"))) int PyThreadState_SetAsyncExc(unsigned long, PyObject *);



__attribute__ ((visibility ("default"))) PyInterpreterState* PyThreadState_GetInterpreter(PyThreadState *tstate);
__attribute__ ((visibility ("default"))) PyFrameObject* PyThreadState_GetFrame(PyThreadState *tstate);
__attribute__ ((visibility ("default"))) uint64_t PyThreadState_GetID(PyThreadState *tstate);


typedef
    enum {PyGILState_LOCKED, PyGILState_UNLOCKED}
        PyGILState_STATE;
__attribute__ ((visibility ("default"))) PyGILState_STATE PyGILState_Ensure(void);
__attribute__ ((visibility ("default"))) void PyGILState_Release(PyGILState_STATE);







__attribute__ ((visibility ("default"))) PyThreadState * PyGILState_GetThisThreadState(void);
__attribute__ ((visibility ("default"))) void PyErr_SetNone(PyObject *);
__attribute__ ((visibility ("default"))) void PyErr_SetObject(PyObject *, PyObject *);
__attribute__ ((visibility ("default"))) void PyErr_SetString(
    PyObject *exception,
    const char *string
    );
__attribute__ ((visibility ("default"))) PyObject * PyErr_Occurred(void);
__attribute__ ((visibility ("default"))) void PyErr_Clear(void);
__attribute__ ((visibility ("default"))) void PyErr_Fetch(PyObject **, PyObject **, PyObject **);
__attribute__ ((visibility ("default"))) void PyErr_Restore(PyObject *, PyObject *, PyObject *);

__attribute__ ((visibility ("default"))) PyObject* PyErr_GetHandledException(void);
__attribute__ ((visibility ("default"))) void PyErr_SetHandledException(PyObject *);


__attribute__ ((visibility ("default"))) void PyErr_GetExcInfo(PyObject **, PyObject **, PyObject **);
__attribute__ ((visibility ("default"))) void PyErr_SetExcInfo(PyObject *, PyObject *, PyObject *);







__attribute__ ((visibility ("default"))) void __attribute__((__noreturn__)) Py_FatalError(const char *message);


__attribute__ ((visibility ("default"))) int PyErr_GivenExceptionMatches(PyObject *, PyObject *);
__attribute__ ((visibility ("default"))) int PyErr_ExceptionMatches(PyObject *);
__attribute__ ((visibility ("default"))) void PyErr_NormalizeException(PyObject**, PyObject**, PyObject**);


__attribute__ ((visibility ("default"))) int PyException_SetTraceback(PyObject *, PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyException_GetTraceback(PyObject *);


__attribute__ ((visibility ("default"))) PyObject * PyException_GetCause(PyObject *);
__attribute__ ((visibility ("default"))) void PyException_SetCause(PyObject *, PyObject *);


__attribute__ ((visibility ("default"))) PyObject * PyException_GetContext(PyObject *);
__attribute__ ((visibility ("default"))) void PyException_SetContext(PyObject *, PyObject *);
__attribute__ ((visibility ("default"))) const char * PyExceptionClass_Name(PyObject *);
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_BaseException;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_Exception;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_BaseExceptionGroup;

extern __attribute__ ((visibility ("default"))) PyObject * PyExc_StopAsyncIteration;

extern __attribute__ ((visibility ("default"))) PyObject * PyExc_StopIteration;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_GeneratorExit;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_ArithmeticError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_LookupError;

extern __attribute__ ((visibility ("default"))) PyObject * PyExc_AssertionError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_AttributeError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_BufferError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_EOFError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_FloatingPointError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_OSError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_ImportError;

extern __attribute__ ((visibility ("default"))) PyObject * PyExc_ModuleNotFoundError;

extern __attribute__ ((visibility ("default"))) PyObject * PyExc_IndexError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_KeyError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_KeyboardInterrupt;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_MemoryError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_NameError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_OverflowError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_RuntimeError;

extern __attribute__ ((visibility ("default"))) PyObject * PyExc_RecursionError;

extern __attribute__ ((visibility ("default"))) PyObject * PyExc_NotImplementedError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_SyntaxError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_IndentationError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_TabError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_ReferenceError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_SystemError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_SystemExit;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_TypeError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_UnboundLocalError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_UnicodeError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_UnicodeEncodeError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_UnicodeDecodeError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_UnicodeTranslateError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_ValueError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_ZeroDivisionError;


extern __attribute__ ((visibility ("default"))) PyObject * PyExc_BlockingIOError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_BrokenPipeError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_ChildProcessError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_ConnectionError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_ConnectionAbortedError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_ConnectionRefusedError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_ConnectionResetError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_FileExistsError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_FileNotFoundError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_InterruptedError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_IsADirectoryError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_NotADirectoryError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_PermissionError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_ProcessLookupError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_TimeoutError;




extern __attribute__ ((visibility ("default"))) PyObject * PyExc_EnvironmentError;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_IOError;





extern __attribute__ ((visibility ("default"))) PyObject * PyExc_Warning;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_UserWarning;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_DeprecationWarning;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_PendingDeprecationWarning;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_SyntaxWarning;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_RuntimeWarning;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_FutureWarning;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_ImportWarning;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_UnicodeWarning;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_BytesWarning;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_EncodingWarning;
extern __attribute__ ((visibility ("default"))) PyObject * PyExc_ResourceWarning;




__attribute__ ((visibility ("default"))) int PyErr_BadArgument(void);
__attribute__ ((visibility ("default"))) PyObject * PyErr_NoMemory(void);
__attribute__ ((visibility ("default"))) PyObject * PyErr_SetFromErrno(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyErr_SetFromErrnoWithFilenameObject(
    PyObject *, PyObject *);

__attribute__ ((visibility ("default"))) PyObject * PyErr_SetFromErrnoWithFilenameObjects(
    PyObject *, PyObject *, PyObject *);

__attribute__ ((visibility ("default"))) PyObject * PyErr_SetFromErrnoWithFilename(
    PyObject *exc,
    const char *filename
    );

__attribute__ ((visibility ("default"))) PyObject * PyErr_Format(
    PyObject *exception,
    const char *format,
    ...
    );

__attribute__ ((visibility ("default"))) PyObject * PyErr_FormatV(
    PyObject *exception,
    const char *format,
    va_list vargs);
__attribute__ ((visibility ("default"))) PyObject * PyErr_SetImportErrorSubclass(PyObject *, PyObject *,
    PyObject *, PyObject *);


__attribute__ ((visibility ("default"))) PyObject * PyErr_SetImportError(PyObject *, PyObject *,
    PyObject *);



__attribute__ ((visibility ("default"))) void PyErr_BadInternalCall(void);
__attribute__ ((visibility ("default"))) void _PyErr_BadInternalCall(const char *filename, int lineno);





__attribute__ ((visibility ("default"))) PyObject * PyErr_NewException(
    const char *name, PyObject *base, PyObject *dict);
__attribute__ ((visibility ("default"))) PyObject * PyErr_NewExceptionWithDoc(
    const char *name, const char *doc, PyObject *base, PyObject *dict);
__attribute__ ((visibility ("default"))) void PyErr_WriteUnraisable(PyObject *);



__attribute__ ((visibility ("default"))) int PyErr_CheckSignals(void);
__attribute__ ((visibility ("default"))) void PyErr_SetInterrupt(void);

__attribute__ ((visibility ("default"))) int PyErr_SetInterruptEx(int signum);



__attribute__ ((visibility ("default"))) void PyErr_SyntaxLocation(
    const char *filename,
    int lineno);
__attribute__ ((visibility ("default"))) void PyErr_SyntaxLocationEx(
    const char *filename,
    int lineno,
    int col_offset);
__attribute__ ((visibility ("default"))) PyObject * PyErr_ProgramText(
    const char *filename,
    int lineno);





__attribute__ ((visibility ("default"))) PyObject * PyUnicodeDecodeError_Create(
    const char *encoding,
    const char *object,
    Py_ssize_t length,
    Py_ssize_t start,
    Py_ssize_t end,
    const char *reason
    );


__attribute__ ((visibility ("default"))) PyObject * PyUnicodeEncodeError_GetEncoding(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyUnicodeDecodeError_GetEncoding(PyObject *);


__attribute__ ((visibility ("default"))) PyObject * PyUnicodeEncodeError_GetObject(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyUnicodeDecodeError_GetObject(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyUnicodeTranslateError_GetObject(PyObject *);



__attribute__ ((visibility ("default"))) int PyUnicodeEncodeError_GetStart(PyObject *, Py_ssize_t *);
__attribute__ ((visibility ("default"))) int PyUnicodeDecodeError_GetStart(PyObject *, Py_ssize_t *);
__attribute__ ((visibility ("default"))) int PyUnicodeTranslateError_GetStart(PyObject *, Py_ssize_t *);



__attribute__ ((visibility ("default"))) int PyUnicodeEncodeError_SetStart(PyObject *, Py_ssize_t);
__attribute__ ((visibility ("default"))) int PyUnicodeDecodeError_SetStart(PyObject *, Py_ssize_t);
__attribute__ ((visibility ("default"))) int PyUnicodeTranslateError_SetStart(PyObject *, Py_ssize_t);



__attribute__ ((visibility ("default"))) int PyUnicodeEncodeError_GetEnd(PyObject *, Py_ssize_t *);
__attribute__ ((visibility ("default"))) int PyUnicodeDecodeError_GetEnd(PyObject *, Py_ssize_t *);
__attribute__ ((visibility ("default"))) int PyUnicodeTranslateError_GetEnd(PyObject *, Py_ssize_t *);



__attribute__ ((visibility ("default"))) int PyUnicodeEncodeError_SetEnd(PyObject *, Py_ssize_t);
__attribute__ ((visibility ("default"))) int PyUnicodeDecodeError_SetEnd(PyObject *, Py_ssize_t);
__attribute__ ((visibility ("default"))) int PyUnicodeTranslateError_SetEnd(PyObject *, Py_ssize_t);


__attribute__ ((visibility ("default"))) PyObject * PyUnicodeEncodeError_GetReason(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyUnicodeDecodeError_GetReason(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyUnicodeTranslateError_GetReason(PyObject *);



__attribute__ ((visibility ("default"))) int PyUnicodeEncodeError_SetReason(
    PyObject *exc,
    const char *reason
    );
__attribute__ ((visibility ("default"))) int PyUnicodeDecodeError_SetReason(
    PyObject *exc,
    const char *reason
    );
__attribute__ ((visibility ("default"))) int PyUnicodeTranslateError_SetReason(
    PyObject *exc,
    const char *reason
    );

__attribute__ ((visibility ("default"))) int PyOS_snprintf(char *str, size_t size, const char *format, ...)
                        __attribute__((format(printf, 3, 4)));
__attribute__ ((visibility ("default"))) int PyOS_vsnprintf(char *str, size_t size, const char *format, va_list va)
                        __attribute__((format(printf, 3, 0)));
extern __attribute__ ((visibility ("default"))) PyTypeObject PyLong_Type;





__attribute__ ((visibility ("default"))) PyObject * PyLong_FromLong(long);
__attribute__ ((visibility ("default"))) PyObject * PyLong_FromUnsignedLong(unsigned long);
__attribute__ ((visibility ("default"))) PyObject * PyLong_FromSize_t(size_t);
__attribute__ ((visibility ("default"))) PyObject * PyLong_FromSsize_t(Py_ssize_t);
__attribute__ ((visibility ("default"))) PyObject * PyLong_FromDouble(double);
__attribute__ ((visibility ("default"))) long PyLong_AsLong(PyObject *);
__attribute__ ((visibility ("default"))) long PyLong_AsLongAndOverflow(PyObject *, int *);
__attribute__ ((visibility ("default"))) Py_ssize_t PyLong_AsSsize_t(PyObject *);
__attribute__ ((visibility ("default"))) size_t PyLong_AsSize_t(PyObject *);
__attribute__ ((visibility ("default"))) unsigned long PyLong_AsUnsignedLong(PyObject *);
__attribute__ ((visibility ("default"))) unsigned long PyLong_AsUnsignedLongMask(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyLong_GetInfo(void);
static inline int
PyLong_AsPid(PyObject *obj)
{
    int overflow;
    long result = PyLong_AsLongAndOverflow(obj, &overflow);
    if (overflow || result > 0x7fffffff 
                                    || result < 
                                                       ) {
        PyErr_SetString(PyExc_OverflowError,
                        "Python int too large to convert to C int");
        return -1;
    }
    return (int)result;
}
__attribute__ ((visibility ("default"))) double PyLong_AsDouble(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyLong_FromVoidPtr(void *);
__attribute__ ((visibility ("default"))) void * PyLong_AsVoidPtr(PyObject *);

__attribute__ ((visibility ("default"))) PyObject * PyLong_FromLongLong(long long);
__attribute__ ((visibility ("default"))) PyObject * PyLong_FromUnsignedLongLong(unsigned long long);
__attribute__ ((visibility ("default"))) long long PyLong_AsLongLong(PyObject *);
__attribute__ ((visibility ("default"))) unsigned long long PyLong_AsUnsignedLongLong(PyObject *);
__attribute__ ((visibility ("default"))) unsigned long long PyLong_AsUnsignedLongLongMask(PyObject *);
__attribute__ ((visibility ("default"))) long long PyLong_AsLongLongAndOverflow(PyObject *, int *);

__attribute__ ((visibility ("default"))) PyObject * PyLong_FromString(const char *, char **, int);




__attribute__ ((visibility ("default"))) unsigned long PyOS_strtoul(const char *, char **, int);
__attribute__ ((visibility ("default"))) long PyOS_strtol(const char *, char **, int);
extern __attribute__ ((visibility ("default"))) PyTypeObject PyBool_Type;







extern __attribute__ ((visibility ("default"))) PyLongObject _Py_FalseStruct;
extern __attribute__ ((visibility ("default"))) PyLongObject _Py_TrueStruct;






__attribute__ ((visibility ("default"))) int Py_IsTrue(PyObject *x);



__attribute__ ((visibility ("default"))) int Py_IsFalse(PyObject *x);







__attribute__ ((visibility ("default"))) PyObject * PyBool_FromLong(long);
extern __attribute__ ((visibility ("default"))) PyTypeObject PyFloat_Type;
__attribute__ ((visibility ("default"))) double PyFloat_GetMax(void);
__attribute__ ((visibility ("default"))) double PyFloat_GetMin(void);
__attribute__ ((visibility ("default"))) PyObject* PyFloat_GetInfo(void);


__attribute__ ((visibility ("default"))) PyObject* PyFloat_FromString(PyObject*);


__attribute__ ((visibility ("default"))) PyObject* PyFloat_FromDouble(double);



__attribute__ ((visibility ("default"))) double PyFloat_AsDouble(PyObject*);
extern __attribute__ ((visibility ("default"))) PyTypeObject PyComplex_Type;




__attribute__ ((visibility ("default"))) PyObject * PyComplex_FromDoubles(double real, double imag);

__attribute__ ((visibility ("default"))) double PyComplex_RealAsDouble(PyObject *op);
__attribute__ ((visibility ("default"))) double PyComplex_ImagAsDouble(PyObject *op);
extern __attribute__ ((visibility ("default"))) PyTypeObject PyRange_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyRangeIter_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyLongRangeIter_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyMemoryView_Type;
__attribute__ ((visibility ("default"))) PyObject * PyMemoryView_FromObject(PyObject *base);

__attribute__ ((visibility ("default"))) PyObject * PyMemoryView_FromMemory(char *mem, Py_ssize_t size,
                                               int flags);


__attribute__ ((visibility ("default"))) PyObject * PyMemoryView_FromBuffer(const Py_buffer *info);

__attribute__ ((visibility ("default"))) PyObject * PyMemoryView_GetContiguous(PyObject *base,
                                                  int buffertype,
                                                  char order);
extern __attribute__ ((visibility ("default"))) PyTypeObject PyTuple_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyTupleIter_Type;





__attribute__ ((visibility ("default"))) PyObject * PyTuple_New(Py_ssize_t size);
__attribute__ ((visibility ("default"))) Py_ssize_t PyTuple_Size(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyTuple_GetItem(PyObject *, Py_ssize_t);
__attribute__ ((visibility ("default"))) int PyTuple_SetItem(PyObject *, Py_ssize_t, PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyTuple_GetSlice(PyObject *, Py_ssize_t, Py_ssize_t);
__attribute__ ((visibility ("default"))) PyObject * PyTuple_Pack(Py_ssize_t, ...);
extern __attribute__ ((visibility ("default"))) PyTypeObject PyList_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyListIter_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyListRevIter_Type;





__attribute__ ((visibility ("default"))) PyObject * PyList_New(Py_ssize_t size);
__attribute__ ((visibility ("default"))) Py_ssize_t PyList_Size(PyObject *);

__attribute__ ((visibility ("default"))) PyObject * PyList_GetItem(PyObject *, Py_ssize_t);
__attribute__ ((visibility ("default"))) int PyList_SetItem(PyObject *, Py_ssize_t, PyObject *);
__attribute__ ((visibility ("default"))) int PyList_Insert(PyObject *, Py_ssize_t, PyObject *);
__attribute__ ((visibility ("default"))) int PyList_Append(PyObject *, PyObject *);

__attribute__ ((visibility ("default"))) PyObject * PyList_GetSlice(PyObject *, Py_ssize_t, Py_ssize_t);
__attribute__ ((visibility ("default"))) int PyList_SetSlice(PyObject *, Py_ssize_t, Py_ssize_t, PyObject *);

__attribute__ ((visibility ("default"))) int PyList_Sort(PyObject *);
__attribute__ ((visibility ("default"))) int PyList_Reverse(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyList_AsTuple(PyObject *);
extern __attribute__ ((visibility ("default"))) PyTypeObject PyDict_Type;





__attribute__ ((visibility ("default"))) PyObject * PyDict_New(void);
__attribute__ ((visibility ("default"))) PyObject * PyDict_GetItem(PyObject *mp, PyObject *key);
__attribute__ ((visibility ("default"))) PyObject * PyDict_GetItemWithError(PyObject *mp, PyObject *key);
__attribute__ ((visibility ("default"))) int PyDict_SetItem(PyObject *mp, PyObject *key, PyObject *item);
__attribute__ ((visibility ("default"))) int PyDict_DelItem(PyObject *mp, PyObject *key);
__attribute__ ((visibility ("default"))) void PyDict_Clear(PyObject *mp);
__attribute__ ((visibility ("default"))) int PyDict_Next(
    PyObject *mp, Py_ssize_t *pos, PyObject **key, PyObject **value);
__attribute__ ((visibility ("default"))) PyObject * PyDict_Keys(PyObject *mp);
__attribute__ ((visibility ("default"))) PyObject * PyDict_Values(PyObject *mp);
__attribute__ ((visibility ("default"))) PyObject * PyDict_Items(PyObject *mp);
__attribute__ ((visibility ("default"))) Py_ssize_t PyDict_Size(PyObject *mp);
__attribute__ ((visibility ("default"))) PyObject * PyDict_Copy(PyObject *mp);
__attribute__ ((visibility ("default"))) int PyDict_Contains(PyObject *mp, PyObject *key);


__attribute__ ((visibility ("default"))) int PyDict_Update(PyObject *mp, PyObject *other);






__attribute__ ((visibility ("default"))) int PyDict_Merge(PyObject *mp,
                             PyObject *other,
                             int override);






__attribute__ ((visibility ("default"))) int PyDict_MergeFromSeq2(PyObject *d,
                                     PyObject *seq2,
                                     int override);

__attribute__ ((visibility ("default"))) PyObject * PyDict_GetItemString(PyObject *dp, const char *key);
__attribute__ ((visibility ("default"))) int PyDict_SetItemString(PyObject *dp, const char *key, PyObject *item);
__attribute__ ((visibility ("default"))) int PyDict_DelItemString(PyObject *dp, const char *key);

__attribute__ ((visibility ("default"))) PyObject * PyObject_GenericGetDict(PyObject *, void *);




extern __attribute__ ((visibility ("default"))) PyTypeObject PyDictKeys_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyDictValues_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyDictItems_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyDictIterKey_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyDictIterValue_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyDictIterItem_Type;

extern __attribute__ ((visibility ("default"))) PyTypeObject PyDictRevIterKey_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyDictRevIterItem_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyDictRevIterValue_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyEnum_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyReversed_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PySet_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyFrozenSet_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PySetIter_Type;

__attribute__ ((visibility ("default"))) PyObject * PySet_New(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyFrozenSet_New(PyObject *);

__attribute__ ((visibility ("default"))) int PySet_Add(PyObject *set, PyObject *key);
__attribute__ ((visibility ("default"))) int PySet_Clear(PyObject *set);
__attribute__ ((visibility ("default"))) int PySet_Contains(PyObject *anyset, PyObject *key);
__attribute__ ((visibility ("default"))) int PySet_Discard(PyObject *set, PyObject *key);
__attribute__ ((visibility ("default"))) PyObject * PySet_Pop(PyObject *set);
__attribute__ ((visibility ("default"))) Py_ssize_t PySet_Size(PyObject *anyset);
extern __attribute__ ((visibility ("default"))) PyTypeObject PyCFunction_Type;




typedef PyObject *(*PyCFunction)(PyObject *, PyObject *);
typedef PyObject *(*_PyCFunctionFast) (PyObject *, PyObject *const *, Py_ssize_t);
typedef PyObject *(*PyCFunctionWithKeywords)(PyObject *, PyObject *,
                                             PyObject *);
typedef PyObject *(*_PyCFunctionFastWithKeywords) (PyObject *,
                                                   PyObject *const *, Py_ssize_t,
                                                   PyObject *);
typedef PyObject *(*PyCMethod)(PyObject *, PyTypeObject *, PyObject *const *,
                               size_t, PyObject *);
__attribute__ ((visibility ("default"))) PyCFunction PyCFunction_GetFunction(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyCFunction_GetSelf(PyObject *);
__attribute__ ((visibility ("default"))) int PyCFunction_GetFlags(PyObject *);

__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) PyObject * PyCFunction_Call(PyObject *, PyObject *, PyObject *);

struct PyMethodDef {
    const char *ml_name;
    PyCFunction ml_meth;
    int ml_flags;

    const char *ml_doc;
};




__attribute__ ((visibility ("default"))) PyObject * PyCFunction_New(PyMethodDef *, PyObject *);



__attribute__ ((visibility ("default"))) PyObject * PyCFunction_NewEx(PyMethodDef *, PyObject *,
                                         PyObject *);



__attribute__ ((visibility ("default"))) PyObject * PyCMethod_New(PyMethodDef *, PyObject *,
                                     PyObject *, PyTypeObject *);
extern __attribute__ ((visibility ("default"))) PyTypeObject PyModule_Type;





__attribute__ ((visibility ("default"))) PyObject * PyModule_NewObject(
    PyObject *name
    );

__attribute__ ((visibility ("default"))) PyObject * PyModule_New(
    const char *name
    );
__attribute__ ((visibility ("default"))) PyObject * PyModule_GetDict(PyObject *);

__attribute__ ((visibility ("default"))) PyObject * PyModule_GetNameObject(PyObject *);

__attribute__ ((visibility ("default"))) const char * PyModule_GetName(PyObject *);
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) const char * PyModule_GetFilename(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyModule_GetFilenameObject(PyObject *);





__attribute__ ((visibility ("default"))) PyModuleDef* PyModule_GetDef(PyObject*);
__attribute__ ((visibility ("default"))) void* PyModule_GetState(PyObject*);



__attribute__ ((visibility ("default"))) PyObject * PyModuleDef_Init(PyModuleDef*);
extern __attribute__ ((visibility ("default"))) PyTypeObject PyModuleDef_Type;


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
__attribute__ ((visibility ("default"))) PyObject * PyFile_FromFd(int, const char *, const char *, int,
                                     const char *, const char *,
                                     const char *, int);
__attribute__ ((visibility ("default"))) PyObject * PyFile_GetLine(PyObject *, int);
__attribute__ ((visibility ("default"))) int PyFile_WriteObject(PyObject *, PyObject *, int);
__attribute__ ((visibility ("default"))) int PyFile_WriteString(const char *, PyObject *);
__attribute__ ((visibility ("default"))) int PyObject_AsFileDescriptor(PyObject *);




extern __attribute__ ((visibility ("default"))) const char * Py_FileSystemDefaultEncoding;

extern __attribute__ ((visibility ("default"))) const char * Py_FileSystemDefaultEncodeErrors;

extern __attribute__ ((visibility ("default"))) int Py_HasFileSystemDefaultEncoding;


extern __attribute__ ((visibility ("default"))) int Py_UTF8Mode;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyCapsule_Type;

typedef void (*PyCapsule_Destructor)(PyObject *);




__attribute__ ((visibility ("default"))) PyObject * PyCapsule_New(
    void *pointer,
    const char *name,
    PyCapsule_Destructor destructor);

__attribute__ ((visibility ("default"))) void * PyCapsule_GetPointer(PyObject *capsule, const char *name);

__attribute__ ((visibility ("default"))) PyCapsule_Destructor PyCapsule_GetDestructor(PyObject *capsule);

__attribute__ ((visibility ("default"))) const char * PyCapsule_GetName(PyObject *capsule);

__attribute__ ((visibility ("default"))) void * PyCapsule_GetContext(PyObject *capsule);

__attribute__ ((visibility ("default"))) int PyCapsule_IsValid(PyObject *capsule, const char *name);

__attribute__ ((visibility ("default"))) int PyCapsule_SetPointer(PyObject *capsule, void *pointer);

__attribute__ ((visibility ("default"))) int PyCapsule_SetDestructor(PyObject *capsule, PyCapsule_Destructor destructor);

__attribute__ ((visibility ("default"))) int PyCapsule_SetName(PyObject *capsule, const char *name);

__attribute__ ((visibility ("default"))) int PyCapsule_SetContext(PyObject *capsule, void *context);

__attribute__ ((visibility ("default"))) void * PyCapsule_Import(
    const char *name,
    int no_block);
__attribute__ ((visibility ("default"))) int PyFrame_GetLineNumber(PyFrameObject *);

__attribute__ ((visibility ("default"))) PyCodeObject * PyFrame_GetCode(PyFrameObject *frame);
__attribute__ ((visibility ("default"))) int PyTraceBack_Here(PyFrameObject *);
__attribute__ ((visibility ("default"))) int PyTraceBack_Print(PyObject *, PyObject *);


extern __attribute__ ((visibility ("default"))) PyTypeObject PyTraceBack_Type;
extern __attribute__ ((visibility ("default"))) PyObject _Py_EllipsisObject;
extern __attribute__ ((visibility ("default"))) PyTypeObject PySlice_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyEllipsis_Type;



__attribute__ ((visibility ("default"))) PyObject * PySlice_New(PyObject* start, PyObject* stop,
                                  PyObject* step);






__attribute__ ((visibility ("default"))) int PySlice_GetIndices(PyObject *r, Py_ssize_t length,
                                  Py_ssize_t *start, Py_ssize_t *stop, Py_ssize_t *step);
__attribute__((__deprecated__))
__attribute__ ((visibility ("default"))) int PySlice_GetIndicesEx(PyObject *r, Py_ssize_t length,
                                     Py_ssize_t *start, Py_ssize_t *stop,
                                     Py_ssize_t *step,
                                     Py_ssize_t *slicelength);







__attribute__ ((visibility ("default"))) int PySlice_Unpack(PyObject *slice,
                               Py_ssize_t *start, Py_ssize_t *stop, Py_ssize_t *step);
__attribute__ ((visibility ("default"))) Py_ssize_t PySlice_AdjustIndices(Py_ssize_t length,
                                             Py_ssize_t *start, Py_ssize_t *stop,
                                             Py_ssize_t step);







extern __attribute__ ((visibility ("default"))) PyTypeObject PySeqIter_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyCallIter_Type;






__attribute__ ((visibility ("default"))) PyObject * PySeqIter_New(PyObject *);




__attribute__ ((visibility ("default"))) PyObject * PyCallIter_New(PyObject *, PyObject *);







typedef PyObject *(*getter)(PyObject *, void *);
typedef int (*setter)(PyObject *, PyObject *, void *);

struct PyGetSetDef {
    const char *name;
    getter get;
    setter set;
    const char *doc;
    void *closure;
};

extern __attribute__ ((visibility ("default"))) PyTypeObject PyClassMethodDescr_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyGetSetDescr_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyMemberDescr_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyMethodDescr_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyWrapperDescr_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyDictProxy_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyProperty_Type;

__attribute__ ((visibility ("default"))) PyObject * PyDescr_NewMethod(PyTypeObject *, PyMethodDef *);
__attribute__ ((visibility ("default"))) PyObject * PyDescr_NewClassMethod(PyTypeObject *, PyMethodDef *);
__attribute__ ((visibility ("default"))) PyObject * PyDescr_NewMember(PyTypeObject *, PyMemberDef *);
__attribute__ ((visibility ("default"))) PyObject * PyDescr_NewGetSet(PyTypeObject *, PyGetSetDef *);

__attribute__ ((visibility ("default"))) PyObject * PyDictProxy_New(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyWrapper_New(PyObject *, PyObject *);







__attribute__ ((visibility ("default"))) PyObject * Py_GenericAlias(PyObject *, PyObject *);
extern __attribute__ ((visibility ("default"))) PyTypeObject Py_GenericAliasType;






__attribute__ ((visibility ("default"))) int PyErr_WarnEx(
    PyObject *category,
    const char *message,
    Py_ssize_t stack_level);

__attribute__ ((visibility ("default"))) int PyErr_WarnFormat(
    PyObject *category,
    Py_ssize_t stack_level,
    const char *format,
    ...);



__attribute__ ((visibility ("default"))) int PyErr_ResourceWarning(
    PyObject *source,
    Py_ssize_t stack_level,
    const char *format,
    ...);


__attribute__ ((visibility ("default"))) int PyErr_WarnExplicit(
    PyObject *category,
    const char *message,
    const char *filename,
    int lineno,
    const char *module,
    PyObject *registry);
typedef struct _PyWeakReference PyWeakReference;

extern __attribute__ ((visibility ("default"))) PyTypeObject _PyWeakref_RefType;
extern __attribute__ ((visibility ("default"))) PyTypeObject _PyWeakref_ProxyType;
extern __attribute__ ((visibility ("default"))) PyTypeObject _PyWeakref_CallableProxyType;
__attribute__ ((visibility ("default"))) PyObject * PyWeakref_NewRef(PyObject *ob,
                                        PyObject *callback);
__attribute__ ((visibility ("default"))) PyObject * PyWeakref_NewProxy(PyObject *ob,
                                          PyObject *callback);
__attribute__ ((visibility ("default"))) PyObject * PyWeakref_GetObject(PyObject *ref);
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

extern __attribute__ ((visibility ("default"))) const char * const PyStructSequence_UnnamedField;







__attribute__ ((visibility ("default"))) PyTypeObject* PyStructSequence_NewType(PyStructSequence_Desc *desc);

__attribute__ ((visibility ("default"))) PyObject * PyStructSequence_New(PyTypeObject* type);
__attribute__ ((visibility ("default"))) void PyStructSequence_SetItem(PyObject*, Py_ssize_t, PyObject*);
__attribute__ ((visibility ("default"))) PyObject* PyStructSequence_GetItem(PyObject*, Py_ssize_t);
__attribute__ ((visibility ("default"))) int PyCodec_Register(
       PyObject *search_function
       );





__attribute__ ((visibility ("default"))) int PyCodec_Unregister(
       PyObject *search_function
       );
__attribute__ ((visibility ("default"))) int PyCodec_KnownEncoding(
       const char *encoding
       );
__attribute__ ((visibility ("default"))) PyObject * PyCodec_Encode(
       PyObject *object,
       const char *encoding,
       const char *errors
       );
__attribute__ ((visibility ("default"))) PyObject * PyCodec_Decode(
       PyObject *object,
       const char *encoding,
       const char *errors
       );
__attribute__ ((visibility ("default"))) PyObject * PyCodec_Encoder(
       const char *encoding
       );



__attribute__ ((visibility ("default"))) PyObject * PyCodec_Decoder(
       const char *encoding
       );



__attribute__ ((visibility ("default"))) PyObject * PyCodec_IncrementalEncoder(
       const char *encoding,
       const char *errors
       );



__attribute__ ((visibility ("default"))) PyObject * PyCodec_IncrementalDecoder(
       const char *encoding,
       const char *errors
       );



__attribute__ ((visibility ("default"))) PyObject * PyCodec_StreamReader(
       const char *encoding,
       PyObject *stream,
       const char *errors
       );



__attribute__ ((visibility ("default"))) PyObject * PyCodec_StreamWriter(
       const char *encoding,
       PyObject *stream,
       const char *errors
       );
__attribute__ ((visibility ("default"))) int PyCodec_RegisterError(const char *name, PyObject *error);




__attribute__ ((visibility ("default"))) PyObject * PyCodec_LookupError(const char *name);


__attribute__ ((visibility ("default"))) PyObject * PyCodec_StrictErrors(PyObject *exc);


__attribute__ ((visibility ("default"))) PyObject * PyCodec_IgnoreErrors(PyObject *exc);


__attribute__ ((visibility ("default"))) PyObject * PyCodec_ReplaceErrors(PyObject *exc);


__attribute__ ((visibility ("default"))) PyObject * PyCodec_XMLCharRefReplaceErrors(PyObject *exc);


__attribute__ ((visibility ("default"))) PyObject * PyCodec_BackslashReplaceErrors(PyObject *exc);



__attribute__ ((visibility ("default"))) PyObject * PyCodec_NameReplaceErrors(PyObject *exc);



typedef void *PyThread_type_lock;







typedef enum PyLockStatus {
    PY_LOCK_FAILURE = 0,
    PY_LOCK_ACQUIRED = 1,
    PY_LOCK_INTR
} PyLockStatus;

__attribute__ ((visibility ("default"))) void PyThread_init_thread(void);
__attribute__ ((visibility ("default"))) unsigned long PyThread_start_new_thread(void (*)(void *), void *);
__attribute__ ((visibility ("default"))) void __attribute__((__noreturn__)) PyThread_exit_thread(void);
__attribute__ ((visibility ("default"))) unsigned long PyThread_get_thread_ident(void);



__attribute__ ((visibility ("default"))) unsigned long PyThread_get_thread_native_id(void);


__attribute__ ((visibility ("default"))) PyThread_type_lock PyThread_allocate_lock(void);
__attribute__ ((visibility ("default"))) void PyThread_free_lock(PyThread_type_lock);
__attribute__ ((visibility ("default"))) int PyThread_acquire_lock(PyThread_type_lock, int);
__attribute__ ((visibility ("default"))) PyLockStatus PyThread_acquire_lock_timed(PyThread_type_lock,
                                                     long long microseconds,
                                                     int intr_flag);

__attribute__ ((visibility ("default"))) void PyThread_release_lock(PyThread_type_lock);

__attribute__ ((visibility ("default"))) size_t PyThread_get_stacksize(void);
__attribute__ ((visibility ("default"))) int PyThread_set_stacksize(size_t);


__attribute__ ((visibility ("default"))) PyObject* PyThread_GetInfo(void);
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) int PyThread_create_key(void);
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) void PyThread_delete_key(int key);
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) int PyThread_set_key_value(int key,
                                                          void *value);
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) void * PyThread_get_key_value(int key);
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) void PyThread_delete_key_value(int key);


__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) void PyThread_ReInitTLS(void);






typedef struct _Py_tss_t Py_tss_t;

__attribute__ ((visibility ("default"))) Py_tss_t * PyThread_tss_alloc(void);
__attribute__ ((visibility ("default"))) void PyThread_tss_free(Py_tss_t *key);


__attribute__ ((visibility ("default"))) int PyThread_tss_is_created(Py_tss_t *key);
__attribute__ ((visibility ("default"))) int PyThread_tss_create(Py_tss_t *key);
__attribute__ ((visibility ("default"))) void PyThread_tss_delete(Py_tss_t *key);
__attribute__ ((visibility ("default"))) int PyThread_tss_set(Py_tss_t *key, void *value);
__attribute__ ((visibility ("default"))) void * PyThread_tss_get(Py_tss_t *key);
__attribute__ ((visibility ("default"))) int _PyArg_Parse_SizeT(PyObject *, const char *, ...);
__attribute__ ((visibility ("default"))) int _PyArg_ParseTuple_SizeT(PyObject *, const char *, ...);
__attribute__ ((visibility ("default"))) int _PyArg_ParseTupleAndKeywords_SizeT(PyObject *, PyObject *,
                                                  const char *, char **, ...);
__attribute__ ((visibility ("default"))) int _PyArg_VaParse_SizeT(PyObject *, const char *, va_list);
__attribute__ ((visibility ("default"))) int _PyArg_VaParseTupleAndKeywords_SizeT(PyObject *, PyObject *,
                                                  const char *, char **, va_list);

__attribute__ ((visibility ("default"))) int PyArg_ValidateKeywordArguments(PyObject *);
__attribute__ ((visibility ("default"))) int PyArg_UnpackTuple(PyObject *, const char *, Py_ssize_t, Py_ssize_t, ...);
__attribute__ ((visibility ("default"))) PyObject * _Py_BuildValue_SizeT(const char *, ...);
__attribute__ ((visibility ("default"))) PyObject * _Py_BuildValue_SizeT(const char *, ...);




__attribute__ ((visibility ("default"))) PyObject * _Py_VaBuildValue_SizeT(const char *, va_list);





__attribute__ ((visibility ("default"))) int PyModule_AddObjectRef(PyObject *mod, const char *name, PyObject *value);




__attribute__ ((visibility ("default"))) int PyModule_AddObject(PyObject *mod, const char *, PyObject *value);

__attribute__ ((visibility ("default"))) int PyModule_AddIntConstant(PyObject *, const char *, long);
__attribute__ ((visibility ("default"))) int PyModule_AddStringConstant(PyObject *, const char *, const char *);



__attribute__ ((visibility ("default"))) int PyModule_AddType(PyObject *module, PyTypeObject *type);







__attribute__ ((visibility ("default"))) int PyModule_SetDocString(PyObject *, const char *);
__attribute__ ((visibility ("default"))) int PyModule_AddFunctions(PyObject *, PyMethodDef *);
__attribute__ ((visibility ("default"))) int PyModule_ExecDef(PyObject *module, PyModuleDef *def);
__attribute__ ((visibility ("default"))) PyObject * PyModule_Create2(PyModuleDef*, int apiver);
__attribute__ ((visibility ("default"))) PyObject * PyModule_FromDefAndSpec2(PyModuleDef *def,
                                                PyObject *spec,
                                                int module_api_version);
__attribute__ ((visibility ("default"))) PyObject * Py_CompileString(const char *, const char *, int);

__attribute__ ((visibility ("default"))) void PyErr_Print(void);
__attribute__ ((visibility ("default"))) void PyErr_PrintEx(int);
__attribute__ ((visibility ("default"))) void PyErr_Display(PyObject *, PyObject *, PyObject *);



extern __attribute__ ((visibility ("default"))) int (*PyOS_InputHook)(void);
__attribute__ ((visibility ("default"))) void Py_Initialize(void);
__attribute__ ((visibility ("default"))) void Py_InitializeEx(int);
__attribute__ ((visibility ("default"))) void Py_Finalize(void);

__attribute__ ((visibility ("default"))) int Py_FinalizeEx(void);

__attribute__ ((visibility ("default"))) int Py_IsInitialized(void);


__attribute__ ((visibility ("default"))) PyThreadState * Py_NewInterpreter(void);
__attribute__ ((visibility ("default"))) void Py_EndInterpreter(PyThreadState *);





__attribute__ ((visibility ("default"))) int Py_AtExit(void (*func)(void));

__attribute__ ((visibility ("default"))) void __attribute__((__noreturn__)) Py_Exit(int);


__attribute__ ((visibility ("default"))) int Py_Main(int argc, wchar_t **argv);
__attribute__ ((visibility ("default"))) int Py_BytesMain(int argc, char **argv);


__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) void Py_SetProgramName(const wchar_t *);
__attribute__ ((visibility ("default"))) wchar_t * Py_GetProgramName(void);

__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) void Py_SetPythonHome(const wchar_t *);
__attribute__ ((visibility ("default"))) wchar_t * Py_GetPythonHome(void);

__attribute__ ((visibility ("default"))) wchar_t * Py_GetProgramFullPath(void);

__attribute__ ((visibility ("default"))) wchar_t * Py_GetPrefix(void);
__attribute__ ((visibility ("default"))) wchar_t * Py_GetExecPrefix(void);
__attribute__ ((visibility ("default"))) wchar_t * Py_GetPath(void);
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) void Py_SetPath(const wchar_t *);





__attribute__ ((visibility ("default"))) const char * Py_GetVersion(void);
__attribute__ ((visibility ("default"))) const char * Py_GetPlatform(void);
__attribute__ ((visibility ("default"))) const char * Py_GetCopyright(void);
__attribute__ ((visibility ("default"))) const char * Py_GetCompiler(void);
__attribute__ ((visibility ("default"))) const char * Py_GetBuildInfo(void);


typedef void (*PyOS_sighandler_t)(int);
__attribute__ ((visibility ("default"))) PyOS_sighandler_t PyOS_getsig(int);
__attribute__ ((visibility ("default"))) PyOS_sighandler_t PyOS_setsig(int, PyOS_sighandler_t);


extern __attribute__ ((visibility ("default"))) const unsigned long Py_Version;
__attribute__ ((visibility ("default"))) PyObject * PyEval_EvalCode(PyObject *, PyObject *, PyObject *);

__attribute__ ((visibility ("default"))) PyObject * PyEval_EvalCodeEx(PyObject *co,
                                         PyObject *globals,
                                         PyObject *locals,
                                         PyObject *const *args, int argc,
                                         PyObject *const *kwds, int kwdc,
                                         PyObject *const *defs, int defc,
                                         PyObject *kwdefs, PyObject *closure);
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) PyObject * PyEval_CallObjectWithKeywords(
    PyObject *callable,
    PyObject *args,
    PyObject *kwargs);





__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) PyObject * PyEval_CallFunction(
    PyObject *callable, const char *format, ...);
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) PyObject * PyEval_CallMethod(
    PyObject *obj, const char *name, const char *format, ...);

__attribute__ ((visibility ("default"))) PyObject * PyEval_GetBuiltins(void);
__attribute__ ((visibility ("default"))) PyObject * PyEval_GetGlobals(void);
__attribute__ ((visibility ("default"))) PyObject * PyEval_GetLocals(void);
__attribute__ ((visibility ("default"))) PyFrameObject * PyEval_GetFrame(void);

__attribute__ ((visibility ("default"))) int Py_AddPendingCall(int (*func)(void *), void *arg);
__attribute__ ((visibility ("default"))) int Py_MakePendingCalls(void);
__attribute__ ((visibility ("default"))) void Py_SetRecursionLimit(int);
__attribute__ ((visibility ("default"))) int Py_GetRecursionLimit(void);

__attribute__ ((visibility ("default"))) int Py_EnterRecursiveCall(const char *where);
__attribute__ ((visibility ("default"))) void Py_LeaveRecursiveCall(void);

__attribute__ ((visibility ("default"))) const char * PyEval_GetFuncName(PyObject *);
__attribute__ ((visibility ("default"))) const char * PyEval_GetFuncDesc(PyObject *);

__attribute__ ((visibility ("default"))) PyObject * PyEval_EvalFrame(PyFrameObject *);
__attribute__ ((visibility ("default"))) PyObject * PyEval_EvalFrameEx(PyFrameObject *f, int exc);
__attribute__ ((visibility ("default"))) PyThreadState * PyEval_SaveThread(void);
__attribute__ ((visibility ("default"))) void PyEval_RestoreThread(PyThreadState *);

__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) int PyEval_ThreadsInitialized(void);
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) void PyEval_InitThreads(void);




__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) void PyEval_AcquireLock(void);
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) void PyEval_ReleaseLock(void);
__attribute__ ((visibility ("default"))) void PyEval_AcquireThread(PyThreadState *tstate);
__attribute__ ((visibility ("default"))) void PyEval_ReleaseThread(PyThreadState *tstate);
__attribute__ ((visibility ("default"))) PyObject * PySys_GetObject(const char *);
__attribute__ ((visibility ("default"))) int PySys_SetObject(const char *, PyObject *);

__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) void PySys_SetArgv(int, wchar_t **);
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) void PySys_SetArgvEx(int, wchar_t **, int);
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) void PySys_SetPath(const wchar_t *);

__attribute__ ((visibility ("default"))) void PySys_WriteStdout(const char *format, ...)
                 __attribute__((format(printf, 1, 2)));
__attribute__ ((visibility ("default"))) void PySys_WriteStderr(const char *format, ...)
                 __attribute__((format(printf, 1, 2)));
__attribute__ ((visibility ("default"))) void PySys_FormatStdout(const char *format, ...);
__attribute__ ((visibility ("default"))) void PySys_FormatStderr(const char *format, ...);

__attribute__ ((visibility ("default"))) void PySys_ResetWarnOptions(void);
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) void PySys_AddWarnOption(const wchar_t *);
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) void PySys_AddWarnOptionUnicode(PyObject *);
__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) int PySys_HasWarnOptions(void);

__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) void PySys_AddXOption(const wchar_t *);
__attribute__ ((visibility ("default"))) PyObject * PySys_GetXOptions(void);
__attribute__ ((visibility ("default"))) PyObject * PyOS_FSPath(PyObject *path);






__attribute__ ((visibility ("default"))) int PyOS_InterruptOccurred(void);


__attribute__ ((visibility ("default"))) void PyOS_BeforeFork(void);
__attribute__ ((visibility ("default"))) void PyOS_AfterFork_Parent(void);
__attribute__ ((visibility ("default"))) void PyOS_AfterFork_Child(void);



__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) void PyOS_AfterFork(void);
__attribute__ ((visibility ("default"))) long PyImport_GetMagicNumber(void);
__attribute__ ((visibility ("default"))) const char * PyImport_GetMagicTag(void);
__attribute__ ((visibility ("default"))) PyObject * PyImport_ExecCodeModule(
    const char *name,
    PyObject *co
    );
__attribute__ ((visibility ("default"))) PyObject * PyImport_ExecCodeModuleEx(
    const char *name,
    PyObject *co,
    const char *pathname
    );
__attribute__ ((visibility ("default"))) PyObject * PyImport_ExecCodeModuleWithPathnames(
    const char *name,
    PyObject *co,
    const char *pathname,
    const char *cpathname
    );

__attribute__ ((visibility ("default"))) PyObject * PyImport_ExecCodeModuleObject(
    PyObject *name,
    PyObject *co,
    PyObject *pathname,
    PyObject *cpathname
    );

__attribute__ ((visibility ("default"))) PyObject * PyImport_GetModuleDict(void);

__attribute__ ((visibility ("default"))) PyObject * PyImport_GetModule(PyObject *name);


__attribute__ ((visibility ("default"))) PyObject * PyImport_AddModuleObject(
    PyObject *name
    );

__attribute__ ((visibility ("default"))) PyObject * PyImport_AddModule(
    const char *name
    );
__attribute__ ((visibility ("default"))) PyObject * PyImport_ImportModule(
    const char *name
    );
__attribute__ ((visibility ("default"))) PyObject * PyImport_ImportModuleNoBlock(
    const char *name
    );
__attribute__ ((visibility ("default"))) PyObject * PyImport_ImportModuleLevel(
    const char *name,
    PyObject *globals,
    PyObject *locals,
    PyObject *fromlist,
    int level
    );

__attribute__ ((visibility ("default"))) PyObject * PyImport_ImportModuleLevelObject(
    PyObject *name,
    PyObject *globals,
    PyObject *locals,
    PyObject *fromlist,
    int level
    );





__attribute__ ((visibility ("default"))) PyObject * PyImport_GetImporter(PyObject *path);
__attribute__ ((visibility ("default"))) PyObject * PyImport_Import(PyObject *name);
__attribute__ ((visibility ("default"))) PyObject * PyImport_ReloadModule(PyObject *m);

__attribute__ ((visibility ("default"))) int PyImport_ImportFrozenModuleObject(
    PyObject *name
    );

__attribute__ ((visibility ("default"))) int PyImport_ImportFrozenModule(
    const char *name
    );

__attribute__ ((visibility ("default"))) int PyImport_AppendInittab(
    const char *name,
    PyObject* (*initfunc)(void)
    );
__attribute__ ((visibility ("default"))) PyObject * PyObject_CallNoArgs(PyObject *func);
__attribute__ ((visibility ("default"))) PyObject * PyObject_Call(PyObject *callable,
                                     PyObject *args, PyObject *kwargs);
__attribute__ ((visibility ("default"))) PyObject * PyObject_CallObject(PyObject *callable,
                                           PyObject *args);
__attribute__ ((visibility ("default"))) PyObject * _PyObject_CallFunction_SizeT(PyObject *callable,
                                             const char *format, ...);
__attribute__ ((visibility ("default"))) PyObject * _PyObject_CallMethod_SizeT(PyObject *obj,
                                           const char *name,
                                           const char *format, ...);

__attribute__ ((visibility ("default"))) PyObject * _PyObject_CallFunction_SizeT(PyObject *callable,
                                                    const char *format,
                                                    ...);

__attribute__ ((visibility ("default"))) PyObject * _PyObject_CallMethod_SizeT(PyObject *obj,
                                                  const char *name,
                                                  const char *format,
                                                  ...);
__attribute__ ((visibility ("default"))) PyObject * PyObject_CallFunctionObjArgs(PyObject *callable,
                                                    ...);
__attribute__ ((visibility ("default"))) PyObject * PyObject_CallMethodObjArgs(
    PyObject *obj,
    PyObject *name,
    ...);
__attribute__ ((visibility ("default"))) PyObject * PyObject_Type(PyObject *o);
__attribute__ ((visibility ("default"))) Py_ssize_t PyObject_Size(PyObject *o);




__attribute__ ((visibility ("default"))) Py_ssize_t PyObject_Length(PyObject *o);






__attribute__ ((visibility ("default"))) PyObject * PyObject_GetItem(PyObject *o, PyObject *key);







__attribute__ ((visibility ("default"))) int PyObject_SetItem(PyObject *o, PyObject *key, PyObject *v);





__attribute__ ((visibility ("default"))) int PyObject_DelItemString(PyObject *o, const char *key);





__attribute__ ((visibility ("default"))) int PyObject_DelItem(PyObject *o, PyObject *key);
__attribute__((__deprecated__))
__attribute__ ((visibility ("default"))) int PyObject_AsCharBuffer(PyObject *obj,
                                      const char **buffer,
                                      Py_ssize_t *buffer_len);





__attribute__((__deprecated__)) __attribute__ ((visibility ("default"))) int PyObject_CheckReadBuffer(PyObject *obj);







__attribute__((__deprecated__))
__attribute__ ((visibility ("default"))) int PyObject_AsReadBuffer(PyObject *obj,
                                      const void **buffer,
                                      Py_ssize_t *buffer_len);







__attribute__((__deprecated__))
__attribute__ ((visibility ("default"))) int PyObject_AsWriteBuffer(PyObject *obj,
                                       void **buffer,
                                       Py_ssize_t *buffer_len);






__attribute__ ((visibility ("default"))) PyObject * PyObject_Format(PyObject *obj,
                                       PyObject *format_spec);







__attribute__ ((visibility ("default"))) PyObject * PyObject_GetIter(PyObject *);




__attribute__ ((visibility ("default"))) PyObject * PyObject_GetAIter(PyObject *);




__attribute__ ((visibility ("default"))) int PyIter_Check(PyObject *);




__attribute__ ((visibility ("default"))) int PyAIter_Check(PyObject *);
__attribute__ ((visibility ("default"))) PyObject * PyIter_Next(PyObject *);
__attribute__ ((visibility ("default"))) PySendResult PyIter_Send(PyObject *, PyObject *, PyObject **);
__attribute__ ((visibility ("default"))) int PyNumber_Check(PyObject *o);




__attribute__ ((visibility ("default"))) PyObject * PyNumber_Add(PyObject *o1, PyObject *o2);




__attribute__ ((visibility ("default"))) PyObject * PyNumber_Subtract(PyObject *o1, PyObject *o2);




__attribute__ ((visibility ("default"))) PyObject * PyNumber_Multiply(PyObject *o1, PyObject *o2);



__attribute__ ((visibility ("default"))) PyObject * PyNumber_MatrixMultiply(PyObject *o1, PyObject *o2);






__attribute__ ((visibility ("default"))) PyObject * PyNumber_FloorDivide(PyObject *o1, PyObject *o2);





__attribute__ ((visibility ("default"))) PyObject * PyNumber_TrueDivide(PyObject *o1, PyObject *o2);




__attribute__ ((visibility ("default"))) PyObject * PyNumber_Remainder(PyObject *o1, PyObject *o2);






__attribute__ ((visibility ("default"))) PyObject * PyNumber_Divmod(PyObject *o1, PyObject *o2);





__attribute__ ((visibility ("default"))) PyObject * PyNumber_Power(PyObject *o1, PyObject *o2,
                                      PyObject *o3);




__attribute__ ((visibility ("default"))) PyObject * PyNumber_Negative(PyObject *o);




__attribute__ ((visibility ("default"))) PyObject * PyNumber_Positive(PyObject *o);




__attribute__ ((visibility ("default"))) PyObject * PyNumber_Absolute(PyObject *o);




__attribute__ ((visibility ("default"))) PyObject * PyNumber_Invert(PyObject *o);




__attribute__ ((visibility ("default"))) PyObject * PyNumber_Lshift(PyObject *o1, PyObject *o2);





__attribute__ ((visibility ("default"))) PyObject * PyNumber_Rshift(PyObject *o1, PyObject *o2);





__attribute__ ((visibility ("default"))) PyObject * PyNumber_And(PyObject *o1, PyObject *o2);




__attribute__ ((visibility ("default"))) PyObject * PyNumber_Xor(PyObject *o1, PyObject *o2);





__attribute__ ((visibility ("default"))) PyObject * PyNumber_Or(PyObject *o1, PyObject *o2);



__attribute__ ((visibility ("default"))) int PyIndex_Check(PyObject *);



__attribute__ ((visibility ("default"))) PyObject * PyNumber_Index(PyObject *o);







__attribute__ ((visibility ("default"))) Py_ssize_t PyNumber_AsSsize_t(PyObject *o, PyObject *exc);





__attribute__ ((visibility ("default"))) PyObject * PyNumber_Long(PyObject *o);





__attribute__ ((visibility ("default"))) PyObject * PyNumber_Float(PyObject *o);
__attribute__ ((visibility ("default"))) PyObject * PyNumber_InPlaceAdd(PyObject *o1, PyObject *o2);





__attribute__ ((visibility ("default"))) PyObject * PyNumber_InPlaceSubtract(PyObject *o1, PyObject *o2);





__attribute__ ((visibility ("default"))) PyObject * PyNumber_InPlaceMultiply(PyObject *o1, PyObject *o2);



__attribute__ ((visibility ("default"))) PyObject * PyNumber_InPlaceMatrixMultiply(PyObject *o1, PyObject *o2);






__attribute__ ((visibility ("default"))) PyObject * PyNumber_InPlaceFloorDivide(PyObject *o1,
                                                   PyObject *o2);





__attribute__ ((visibility ("default"))) PyObject * PyNumber_InPlaceTrueDivide(PyObject *o1,
                                                  PyObject *o2);





__attribute__ ((visibility ("default"))) PyObject * PyNumber_InPlaceRemainder(PyObject *o1, PyObject *o2);






__attribute__ ((visibility ("default"))) PyObject * PyNumber_InPlacePower(PyObject *o1, PyObject *o2,
                                             PyObject *o3);





__attribute__ ((visibility ("default"))) PyObject * PyNumber_InPlaceLshift(PyObject *o1, PyObject *o2);





__attribute__ ((visibility ("default"))) PyObject * PyNumber_InPlaceRshift(PyObject *o1, PyObject *o2);





__attribute__ ((visibility ("default"))) PyObject * PyNumber_InPlaceAnd(PyObject *o1, PyObject *o2);





__attribute__ ((visibility ("default"))) PyObject * PyNumber_InPlaceXor(PyObject *o1, PyObject *o2);





__attribute__ ((visibility ("default"))) PyObject * PyNumber_InPlaceOr(PyObject *o1, PyObject *o2);





__attribute__ ((visibility ("default"))) PyObject * PyNumber_ToBase(PyObject *n, int base);
__attribute__ ((visibility ("default"))) int PySequence_Check(PyObject *o);


__attribute__ ((visibility ("default"))) Py_ssize_t PySequence_Size(PyObject *o);



__attribute__ ((visibility ("default"))) Py_ssize_t PySequence_Length(PyObject *o);






__attribute__ ((visibility ("default"))) PyObject * PySequence_Concat(PyObject *o1, PyObject *o2);





__attribute__ ((visibility ("default"))) PyObject * PySequence_Repeat(PyObject *o, Py_ssize_t count);




__attribute__ ((visibility ("default"))) PyObject * PySequence_GetItem(PyObject *o, Py_ssize_t i);




__attribute__ ((visibility ("default"))) PyObject * PySequence_GetSlice(PyObject *o, Py_ssize_t i1, Py_ssize_t i2);





__attribute__ ((visibility ("default"))) int PySequence_SetItem(PyObject *o, Py_ssize_t i, PyObject *v);




__attribute__ ((visibility ("default"))) int PySequence_DelItem(PyObject *o, Py_ssize_t i);





__attribute__ ((visibility ("default"))) int PySequence_SetSlice(PyObject *o, Py_ssize_t i1, Py_ssize_t i2,
                                    PyObject *v);





__attribute__ ((visibility ("default"))) int PySequence_DelSlice(PyObject *o, Py_ssize_t i1, Py_ssize_t i2);




__attribute__ ((visibility ("default"))) PyObject * PySequence_Tuple(PyObject *o);



__attribute__ ((visibility ("default"))) PyObject * PySequence_List(PyObject *o);
__attribute__ ((visibility ("default"))) PyObject * PySequence_Fast(PyObject *o, const char* m);
__attribute__ ((visibility ("default"))) Py_ssize_t PySequence_Count(PyObject *o, PyObject *value);





__attribute__ ((visibility ("default"))) int PySequence_Contains(PyObject *seq, PyObject *ob);







__attribute__ ((visibility ("default"))) int PySequence_In(PyObject *o, PyObject *value);
__attribute__ ((visibility ("default"))) Py_ssize_t PySequence_Index(PyObject *o, PyObject *value);
__attribute__ ((visibility ("default"))) PyObject * PySequence_InPlaceConcat(PyObject *o1, PyObject *o2);





__attribute__ ((visibility ("default"))) PyObject * PySequence_InPlaceRepeat(PyObject *o, Py_ssize_t count);







__attribute__ ((visibility ("default"))) int PyMapping_Check(PyObject *o);



__attribute__ ((visibility ("default"))) Py_ssize_t PyMapping_Size(PyObject *o);



__attribute__ ((visibility ("default"))) Py_ssize_t PyMapping_Length(PyObject *o);
__attribute__ ((visibility ("default"))) int PyMapping_HasKeyString(PyObject *o, const char *key);






__attribute__ ((visibility ("default"))) int PyMapping_HasKey(PyObject *o, PyObject *key);



__attribute__ ((visibility ("default"))) PyObject * PyMapping_Keys(PyObject *o);



__attribute__ ((visibility ("default"))) PyObject * PyMapping_Values(PyObject *o);




__attribute__ ((visibility ("default"))) PyObject * PyMapping_Items(PyObject *o);




__attribute__ ((visibility ("default"))) PyObject * PyMapping_GetItemString(PyObject *o,
                                               const char *key);





__attribute__ ((visibility ("default"))) int PyMapping_SetItemString(PyObject *o, const char *key,
                                        PyObject *value);


__attribute__ ((visibility ("default"))) int PyObject_IsInstance(PyObject *object, PyObject *typeorclass);


__attribute__ ((visibility ("default"))) int PyObject_IsSubclass(PyObject *object, PyObject *typeorclass);






extern __attribute__ ((visibility ("default"))) PyTypeObject PyFilter_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyMap_Type;
extern __attribute__ ((visibility ("default"))) PyTypeObject PyZip_Type;
__attribute__ ((visibility ("default"))) double PyOS_string_to_double(const char *str,
                                         char **endptr,
                                         PyObject *overflow_exception);



__attribute__ ((visibility ("default"))) char * PyOS_double_to_string(double val,
                                         char format_code,
                                         int precision,
                                         int flags,
                                         int *type);







__attribute__ ((visibility ("default"))) int PyOS_mystrnicmp(const char *, const char *, Py_ssize_t);
__attribute__ ((visibility ("default"))) int PyOS_mystricmp(const char *, const char *);







__attribute__ ((visibility ("default"))) wchar_t * Py_DecodeLocale(
    const char *arg,
    size_t *size);

__attribute__ ((visibility ("default"))) char* Py_EncodeLocale(
    const wchar_t *text,
    size_t *error_pos);
