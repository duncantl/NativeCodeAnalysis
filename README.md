
# NativeCodeAnalysis

This is R-level code to query and analyze C and C++ code.
It uses both the Rllvm and RCIndex packages to access aspects of the native code.
This is in some ways analogous to the CodeAnalysis, codetools, and other packages for analyzing R
code, but for native code.

It provides R-level functions rather than C/C++-level implementations such as with the excellent
rchk tool that finds, e.g., PROTECT/UNPROTECT errors in C/C++ code.
The reason for this choice is that writing R code is more time-efficient and makes the code more
accessible and extensible to others.

