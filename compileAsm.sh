#!/bin/bash

STUDENTS_CHECK="$(uname -n)"
BUILD="gcc -m32 lib/runtime.o -o "
if [ "$STUDENTS_CHECK" = "students" ]; then
    echo "Students machine discovered. Changing build command..."
    BUILD="ld -melf_i386 lib_students/*.o lib_students/libc.a -o run_out out.o"
fi

filename=$(basename -- "$1")
directory=$(dirname -- "$1")
filename="${filename%.*}"

nasm -f elf32 $1
eval "$BUILD $directory/$filename $directory/$filename.o"
