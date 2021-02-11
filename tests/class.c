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


SEXP
listEls(SEXP a)
{
    SEXP ans;
    PROTECT(ans = NEW_LIST(2));
    SET_VECTOR_ELT(ans, 0, doS4());
    SET_VECTOR_ELT(ans, 1, mk2(a));    
    UNPROTECT(1);
    return(ans);
}


SEXP
setElNames(SEXP ans)
{
    SEXP names;
    PROTECT(ans);
    PROTECT(names = NEW_CHARACTER(2));
    SET_STRING_ELT(names, 0, mkChar("A"));
    SET_STRING_ELT(names, 1, mkChar("B"));    
    SET_NAMES(ans, names);
    UNPROTECT(2);
    return(ans);
}


SEXP
listEls1(SEXP a)
{
    SEXP ans = listEls(a);
    return(setElNames(a));
}


SEXP
listEls2(SEXP a)
{
    SEXP ans;
    PROTECT(ans = NEW_LIST(2));
    SET_VECTOR_ELT(ans, 0, doS4());
    SET_VECTOR_ELT(ans, 1, mk2(a));
    Rf_setAttrib(ans, Rf_install("class"), ScalarString(mkChar("MyList")));
    UNPROTECT(1);
    return(ans);
}

/*
 This one calls listEls2 and then passes the result to setElNames
 which merely adds to the one parameter and returns it. 
 */
SEXP
listEls3(SEXP a)
{
    return(setElNames(listEls2(a)));
}


const char *mkClassName(SEXP x);

SEXP
mkUnknown(SEXP x)
{
    SEXP y = NEW_NUMERIC(Rf_length(x));
    PROTECT(y);
    Rf_setAttrib(y, R_ClassSymbol, ScalarString(mkChar(mkClassName(x))));
    UNPROTECT(1);
    return(y);
}
