#include <Rinternals.h>

SEXP
t1(SEXP x)
{
    if(!isReal(x))
	error("not a real");
    
    return(ScalarReal(REAL(x)[0]));
}


SEXP
t2(SEXP x, SEXP y)
{
    if(!isReal(x))
	error("x is not a real");

    if(!isReal(y))
	error("y is not a real");    
    
    return(ScalarReal(REAL(x)[0] + REAL(y)[0]));
}


SEXP
t2b(SEXP x, SEXP y)
{
    if(!isReal(x) || !isReal(y))
	error("both x and y have to be numeric vectors");
    
    return(ScalarReal(REAL(x)[0] + REAL(y)[0]));
}

SEXP
t2c(SEXP x, SEXP y)
{
    if(!isReal(x) || !isInteger(y))
	error("x has to be numeric and y an integer");
    
    return(ScalarReal(REAL(x)[0] + REAL(y)[0]));
}




SEXP
t3(SEXP x)
{
    if(!isReal(x) && !isInteger(x))
	error("x is not a real or integer vector");
    
    return(ScalarReal(asReal(x)));
}


SEXP
t4(SEXP x)
{
    return(ScalarReal(REAL(x)[0]));
}



SEXP
t5(SEXP x)
{
    if(!isReal(x) || LENGTH(x) < 1)
	error("not a real vector with at least one element");
    
    return(ScalarReal(REAL(x)[0]));
}




SEXP
t6(SEXP x)
{
    if(!( isReal(x) || isInteger(x) ))
	error("x is not a real or integer vector");
    
    return(ScalarReal(asReal(x)));
}
