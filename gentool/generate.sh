#!/bin/sh
if [ -z "$CXX" ]; then
    CXX=gcc
fi

$CXX stdint-gen.cpp -o stdint-gen
./stdint-gen > stable-api.h
perl generate.pl >> stable-api.h
