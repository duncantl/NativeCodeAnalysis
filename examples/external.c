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
