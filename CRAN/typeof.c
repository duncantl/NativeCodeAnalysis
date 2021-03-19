#include <Rdefines.h>

SEXP
R_typeof(SEXP x)
{
    return ScalarInteger(TYPEOF(x));
}
