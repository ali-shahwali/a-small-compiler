# A small compiler for a C-like language

## **How to run**
### Dependencies
- OCaml compiler
- NASM compiler
- GCC

Run the following in your shell
```bash
$ bash compile.sh <your_c_file.cpp> <output_name>
```
The compiled file will be in the build folder that is created.

*Example*
```bash
$ bash compile.sh s_test.cpp main
$ cd build
$ ./main
$ echo $?
30
```

It can only handle very simple C/C++ programs.