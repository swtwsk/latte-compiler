#!/bin/bash

STACK=stack
BUILD="gcc -m32 -o run_out out.o lib/runtime.o"
if [ "$#" -eq 2 ] && [ $2 -eq 1 ]; then
    STACK=/home/students/inf/PUBLIC/MRJP/Stack/stack
    BUILD="ld -melf_i386 -o run_out out.o lib_students/*.o lib_students/libc.a"
fi

for g in $1/*.lat
do
    filename=$(basename -- "$g")
    filename="${filename%.*}"

    ./latc_x86 $g
    if [ -f $1/$filename.input ]; then
        $1/$filename < $1/$filename.input > out.output2
    else
        $1/$filename > out.output2
    fi
    diff out.output2 $1/$filename.output
done
