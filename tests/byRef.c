
#include <Rdefines.h>


int
bar(SEXP x, SEXP *ans)
{

    if(REAL(x)[0] > 0) {
	SEXP tmp = NEW_NUMERIC(Rf_length(x));
	for(int i = 0; i < Rf_length(x); i++)
	    REAL(tmp)[i] = REAL(x)[i] * 2.0;
	*ans = tmp;
	return(1);
    }

    return(0);
}


SEXP
foo(SEXP x)
{
    SEXP ans;
    if(!bar(x, &ans)) {
	ans = ScalarReal(1.0);
    }

    return(ans);
}


int
other(SEXP x, SEXP *ans)
{

    int ok = 1;
    
    if(TYPEOF(x) == REALSXP) {
	*ans = ScalarReal(3.1415);
    } else if(TYPEOF(x) == INTSXP) {
	*ans = ScalarInteger(2);
    } else
	ok = 0;

    return(ok);
}

SEXP
doit(SEXP x)
  {
      SEXP ans;
      if(other(x, &ans) == 0) {
	  ans = Rf_length(x) > 0 ? NEW_CHARACTER(2) : R_NilValue;
      }
      return(ans);
}
