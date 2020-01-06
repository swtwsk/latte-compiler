#!/bin/bash

STACK=stack
BUILD="gcc -m32 -o run_out out.o lib/runtime.o"
if [ "$#" -eq 2 ] && [ $2 -eq 1 ]; then
    STACK=/home/students/inf/PUBLIC/MRJP/Stack/stack
    BUILD="ld -melf_i386 -o run_out out.o lib_students/*.o lib_students/libc.a"
fi

for g in $1/*.lat
do
    echo $g
    filename=$(basename -- "$g")
    # directory=$(dirname -- "$1")
    extension="${filename##*.}"
    filename="${filename%.*}"

    $STACK run < $g 1> out.asm
    nasm -f elf32 out.asm
    #gcc -m32 -o run_out out.o lib/runtime.o
    $BUILD
    if [ -f $1/$filename.input ]; then
        ./run_out < "$1/$filename.input" > out.output2
    else
        ./run_out > out.output2
    fi
    diff out.output2 $1/$filename.output
done
