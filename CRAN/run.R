src = system("find ~/CRAN2/Pkgs -maxdepth 2 -name src ", intern = TRUE)
csrc.files = lapply(src, list.files, pattern = "\\.(c|cc|cpp|C)$")
w = (sapply(csrc.files, length) > 0)
src = src[w]
npkgs = dirname(src)
source("funs.R")
rcpp = sapply(npkgs, usesRcpp)

bc = file.path(src[!rcpp], "all.bc")
bc2 = bc[file.exists(bc)]
zz = lapply(dirname(bc2), function(f) try(dotCallReturnTypes(f)))

