#include <R.h>
#include <Rinternals.h>

SEXP return42()
{
  SEXP ans = PROTECT(allocVector(INTSXP, 1));
  INTEGER(ans)[0] = 42;
  UNPROTECT(1);
  return ans;
}
