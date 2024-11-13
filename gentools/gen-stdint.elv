var CPP = (external clang-cpp)
var CC = (external clang)
var PythonInclude = "D:/miniforge3/include"

$CPP PythonRaw.h -o PythonPre.h -I$PythonInclude

# Compile stdint_gen
$CC stdint_gen.cpp -o stdint_gen.exe

./stdint_gen.exe > Python.h
echo "" >> Python.h
#echo "typedef unsigned int *uintptr_t;" >> Python.h
#echo "typedef signed int *intptr_t;" >> Python.h
echo "typedef uint16_t wchar_t;" >> Python.h
echo "" >> Python.h

e:hy remove-sys.hy >> Python.h

cp ./Python.h ../include/Python.h
