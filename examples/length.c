#include <Rdefines.h>

SEXP
R_foo1(SEXP x)
{
    SEXP ans;
    int n = Rf_length(x);
    ans =  Rf_allocVector(REALSXP, n); // Rf_length(x)
    return(ans);
}

SEXP
R_foo2(SEXP x)
{
    SEXP ans;
    int n = INTEGER(x)[0];
    ans =  Rf_allocVector(REALSXP, n); 
    return(ans);
}


SEXP
R_foo3(SEXP x)
{
    SEXP ans;
    int n = INTEGER(x)[0];
    PROTECT(ans = NEW_LIST(2));
    SET_VECTOR_ELT(ans, 0, NEW_NUMERIC(n));
    SET_VECTOR_ELT(ans, 1, NEW_INTEGER(2*n));    
    UNPROTECT(1);
    return(ans);
}

SEXP
R_foo4(SEXP x)
{
    SEXP ans;
    int n = INTEGER(x)[0];
    int i = 0;
    PROTECT(ans = NEW_LIST(3));
    SET_VECTOR_ELT(ans, i++, NEW_NUMERIC(n));
    SET_VECTOR_ELT(ans, i++, NEW_INTEGER(4*n));
    SET_VECTOR_ELT(ans, i++, NEW_LOGICAL(3*n));        
    UNPROTECT(1);
    return(ans);
}

SEXP
R_foo5(SEXP x)
{
    SEXP ans;
    int r = INTEGER(x)[0];
    int c = INTEGER(x)[1];    

    ans = Rf_allocMatrix(REALSXP, r, c);
    return(ans);
}


SEXP
R_foo6(SEXP x,  SEXP y)
{
    SEXP ans;
    int r = INTEGER(x)[0];
    int c; 
    c = INTEGER(VECTOR_ELT(y, 3))[2];
    ans = Rf_allocMatrix(REALSXP, r, c);
    return(ans);
}


