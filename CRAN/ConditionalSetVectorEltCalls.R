source("RCIndexSetVectorElt.R")

bcs = system("find ~/CRAN2 -name all.bc", intern = TRUE)
pkgSrc = dirname(bcs)

# This takes about 7 hours to run!
system.time({tmp2 = lapply(pkgSrc, function(d) { print(d); try(processDir(d))}) })
nn = sapply(tmp2, function(x) length(unlist(x)))
tmp3 = tmp2[nn > 0]
tmp4 = lapply(tmp3, function(files) {  z = lapply(files, function(f)  f[ sapply(f, length) > 0]); z[ sapply(z, length) > 0]})

saveRDS(tmp4, "IfSetVectorElt.rds")


routineNames = lapply(tmp4, function(x) unique(unlist(lapply(x, names))))
routineNames[!sapply(routineNames, is.null)]



####
# Find out which packages we had problems with
ll = readLines("~/GitWorkingArea/NativeCodeAnalysis/CRAN/RCIndexSession")
g = split(ll, cumsum(grepl("^\\[1\\]", ll)))
bad = sapply(g, length) > 1

badPkgs = gsub('(^\\[1\\] "|"$)', "", sapply(g[bad], `[`, 1))



system.time({tmp.bad = lapply(badPkgs, function(d) { print(d); try(processDir(d))}) })
