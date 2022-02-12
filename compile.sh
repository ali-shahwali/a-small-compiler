#!/bin/bash
make
rm -rf build
mkdir build
./main $1 > ./build/$2.asm --asm 
nasm -felf64 ./build/$2.asm -o ./build/$2 
gcc -no-pie ./build/$2 -o ./build/$2.o
rm ./build/$2.asm
rm ./build/$2