
#include <Rdefines.h>


// Stupid function to do
//   a[] + b[1]
SEXP
foo(SEXP a, SEXP b)
{

    if(TYPEOF(a) != INTSXP) {
	PROBLEM  "not an integer"
	    ERROR;
    }

    if(Rf_length(b) != 1) {
	PROBLEM  "b is not a scalar"
	    ERROR;
    }

    SEXP ans;
    size_t n = Rf_length(a);
    PROTECT(ans = allocVector(REALSXP, n));
    for(int i = 0 ; i < n; i++)
	REAL(ans)[i] = INTEGER(a)[i] + REAL(b)[0];
    
    UNPROTECT(1);
    return(ans);
}
