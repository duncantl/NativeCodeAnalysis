#include <Rdefines.h>

SEXP
R_loop1(SEXP rx, SEXP ry)
{
    double *x = REAL(rx);
    int   *y = INTEGER(ry);
    int n = Rf_length(rx);
    SEXP ans = NEW_NUMERIC(n);
    for(int i = 0; i < n; i++) {
	switch(y[i]) {
 	case 0:
	       REAL(ans)[i] = x[i]*2;
	       break;
 	case 1:
	       REAL(ans)[i] = x[i]/2;
	       break;
	default:
	       REAL(ans)[i] = x[i]*x[i];
	       break;	       
	}
    }
    return(ans);
}



SEXP
R_loop2(SEXP rx, SEXP ry)
{
    double *x = REAL(rx);
    int n = Rf_length(rx);
    SEXP ans = NEW_NUMERIC(n);
    for(int i = 0; i < n; i++) {
	REAL(ans)[i] = x[i] + REAL(VECTOR_ELT(ry, i))[2];
    }
    return(ans);
}


SEXP
R_loop3(SEXP x)
{
    int i;
    for(i = 0; i < Rf_length(x); i++) {
	double val = REAL(x)[i];
	if(val < 0)
	    break;
	REAL(x)[i] = 2*val;
    }
    return(x);
}

double
twoLoops(int n, double *x)
{
    double max = x[0];
    int i;
    for(i = 1 ; i < n ; i++)
	max = x[i] > max ? x[i] : max;

    for(i = 1 ; i < n ; i++)
	x[i] = max - x[i];

    return(max);
}
