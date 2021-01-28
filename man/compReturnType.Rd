\name{compReturnType}
\alias{compReturnType}
\title{Compute the type of the R return type from a routine}
\description{
  This function examines an native/IR Function that returns an R
  object (\code{SEXP}).
  It examines the native code to find the possible R types that
  are returned and returns a description for each of these possible types.
  These types are currently described as scalars, vectors, lists,
  matrices, NULL,
  and where possible, the length/dimensions of the vectors/matrices
  and also how these were computed from parameters to the function(s).
}
\usage{
compReturnType(fun, toc = NULL, blocks = getBlocks(fun))
}
\arguments{
  \item{fun}{the IR Function object whose return type is to be computed}
  \item{toc}{a table-of-contents of different modules so that
     we can find called functions that might be in other modules}
  \item{blocks}{the basic blocks for the \code{fun} object}
}
\value{
  A list with elements for each of the different return paths.
  The elements describe the R types. These can be scalars,
  vectors, matrices, NULL, ....
}
\author{
DTL
}
\details{
  What we are moving towards is being able to find native code that
  for example calls ScalarReal and identifying this as a scalar.
  This works, at least for some cases.
  When we find native code of the form
  \code{Rf_allocVector(REALSXP, n)},
  we capture how n is calculated.
  If this is Rf_nrows(x), we record that this
  is the number of rows of a data.frame/matrix computed from x.
  The function also detects lists and determines the types of the
  elements and also the names of the elements if they are set.
}


\seealso{
\code{\link{getRReturnTypes}} which will probably be removed.
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
