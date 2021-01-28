
#include <Rdefines.h>

SEXP
mk1(SEXP x)
{
    SEXP y = NEW_NUMERIC(Rf_length(x));
    PROTECT(y);
    Rf_setAttrib(y, R_ClassSymbol, ScalarString(mkChar("foo")));
    UNPROTECT(1);
    return(y);
}



SEXP
mk2(SEXP x)
{
    SEXP y = NEW_NUMERIC(Rf_length(x));
    PROTECT(y);
    SEXP k = NEW_CHARACTER(2);
    PROTECT(k);
    SET_STRING_ELT(k, 0, mkChar("foo"));
    SET_STRING_ELT(k, 1, mkChar("bar"));    
    Rf_setAttrib(y, R_ClassSymbol, k);
    UNPROTECT(2);
    return(y);
}


SEXP
doS4()
{
    SEXP k = R_do_MAKE_CLASS("Bob");
    SEXP obj = R_do_new_object(k);
    return(obj);
}
