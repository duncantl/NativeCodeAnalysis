#include <Rdefines.h>

SEXP
namedVec()
{
    SEXP ans, names;
    ans = NEW_INTEGER(2);
    PROTECT(ans);
    PROTECT(names = NEW_CHARACTER(2));

    SET_STRING_ELT(names, 0, mkChar("a"));
    SET_STRING_ELT(names, 1, mkChar("b"));    

    SET_NAMES(ans, names);
    UNPROTECT(2);
    return(ans);
}
