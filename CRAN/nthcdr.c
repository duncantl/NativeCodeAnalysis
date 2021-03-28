#include <Rinternals.h>

SEXP nthcdr(SEXP s, int n);


SEXP
R_nthcdr(SEXP x, SEXP ri)
{
    if(!isFrame(x))
	return(ScalarLogical(0));
    return(nthcdr(x, INTEGER(ri)[0]));
}


SEXP
R_envCAR(SEXP x)
{
    return(CAR(x));
}
