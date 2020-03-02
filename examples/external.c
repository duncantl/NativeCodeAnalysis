#define USE_RINTERNALS 1
#include <Rdefines.h>

SEXP
R_External(SEXP args)
{
    SEXP a = CADR(args);
    SEXP b = CADDR(args);
    SEXP c = CADDDR(args);
    SEXP d = CAD4R(args);

    return(d);
}


SEXP
R_externalPtr(SEXP x)
{
    void *p = R_ExternalPtrAddr(x);
    return(R_NilValue);
}
