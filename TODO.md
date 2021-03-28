# TODO

+ compReturnType
   +  Need to keep the RNULL class on a return of R_NilValue and also not merge the two return values.
       + see CRAN2/Pkgs/zfa - table_e1   
   +  length on distory/src/gromov_distmatrix.  Getting NULL but we need to walk through the casts.   
          + see CRAN2/Pkgs/zfa - table_e1   	 Complex computation.   Note that this is C++ code
            and so has InvokeInst due to possible throws.


+ When a routine returns a list with names, determine the names.
   + cusp/src/cuspn.nc and the cuspnc routine.

+ Determine the length of an R object when it is used as an argument in inferParamType.

+ Represent Rf_as*() as a "coercion to" type rather than "is a specific type already".

+ Follow use of arguments to subroutines called from top-level entry point.
   + expint package and expint_do_expint with .External().

## Low Priority

* Add docs for `find*` functions.





+ xts
```
make -f ~/GitWorkingArea/NativeCodeAnalysis/examples/IRMakefile merge.ir rbind.ir totalcols.ir
```
