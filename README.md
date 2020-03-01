
# NativeCodeAnalysis

This is R-level code to query and analyze C and C++ code.
It uses both the Rllvm and RCIndex packages to access aspects of the native code.
This is in some ways analogous to the CodeAnalysis, codetools, and other packages for analyzing R
code, but for native code.

It provides R-level functions rather than C/C++-level implementations such as with the excellent
rchk tool that finds, e.g., PROTECT/UNPROTECT errors in C/C++ code.
The reason for this choice is that writing R code is more time-efficient and makes the code more
accessible and extensible to others.


The package provides protoypes for
+ identifying the R type of the return from a .Call() or .External() call, including
   the types (and names) of the elements of a returned list;
+ inferring the types of the parameters of a .Call() or .External() routine  based   on 
  how each parameter is 
  + coerced (e.g. `Rf_asReal()`) or
  + accessed (e.g. `INTEGER(x)[0]` or `VECTOR_ELT(x, 0))`,  or
  + used, e.g., `SET_NAMES(ans, x)`;
+ determining the length/dimension of an R object based on the code that  constructs it
  or how it is used in the routine;
+ finding  the class of the return value.
  
We can also borrow information from the R code that calls a routine
to see how it coerces the arguments to different types and what length/dimension
information is available from that R code that we can utilize and/or validate when
analyzing the C code.


# Utilities
+ examples/IRMakefile - stand-aside makefile that complements a package's Makevars file so that we
                        can create the IR files from the .c/.cpp source files.
						
# Documentation

+ examples/Packages.xml - how to analyze C/C++ code in packages.
+ examples/RInternalLLVM.xml - Rllvm approach to analyzing R base/internal code
+ examples/RInternal.xml  - RCIndex approach to analyzing R base/internal code
+ tests/R_FunTab.xml - how to read the R_FunTab array in names.c that maps primitive/internal  functions to C routines
+ examples/toupper.md - analysis of the toupper() function as it relates to C code.


