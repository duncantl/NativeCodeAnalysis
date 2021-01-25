usesRcpp =
function(d)
{
   dc = read.dcf(file.path(d, "DESCRIPTION"))
   v = c("Imports", "Depends", "LinkingTo")
   m = match(v, colnames(dc), 0)
   any(grepl("Rcpp", unlist(dc[, m])))
}

