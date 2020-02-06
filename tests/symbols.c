#include <stdlib.h>
#include <R_ext/Rdynload.h>

/* .C calls */
extern void foo(double *x, int *n);

static const R_CMethodDef CEntries[] = {
    {"foo", (DL_FUNC) &foo, 2},
    {"bar", (DL_FUNC) &foo, 2},    
    {NULL, NULL, 0}
};

void R_init_overlap(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}


void foo(double *x, int *n)
{
    x[0] = 1.0;
}
	     
