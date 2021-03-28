#include <Rinternals.h>

char *classNames[] = {
    "A",
    "B",
    "C"
};

SEXP
foo(SEXP x, SEXP i)
{
    int ok = Rf_inherits(x, classNames[INTEGER(i)[0]]);
    return(ScalarLogical(ok));
}


SEXP
foo2(SEXP x)
{
    int ok = Rf_inherits(x, classNames[1]);
    return(ScalarLogical(ok));
}
