source("funs.R")

umPkgs = system("find ~/CRAN2/Pkgs -name '*.[Rr]' -type f -exec egrep -H 'UseMethod|[^l]NextMethod' {} \\;", intern = TRUE)
# Takes 853 seconds - 14.2 minutes
tt.s3 = table(getPkgName(umPkgs))  

saveRDS(umPkgs, "../useMethodCalls_grep.rds")

s4Calls = system("find ~/CRAN2/Pkgs -name '*.[Rr]' -type f -exec egrep -H 'setMethod|setGeneric|setClass|callNextMethod' {} \\;", intern = TRUE)
# 973 seconds.
tt.s4 = table(getPkgName(s4Calls))  

saveRDS(s4Calls, "../s4Calls_grep.rds")

# Does not include BioConductor which has many packages that use S4.

#  2844 packages that have UseMethod
#  1531 packages that have any S4

# Combined with Bioc   S3  2946
#                      S4  2444
#  So only 9% more use S3 of all the packages that use classes.
#  Or 20% more S3 packages than S4 packages.
#
# This doesn't include R itself and its recommended packages.
#
#
#
# Lines of S3 code  17880  (w/o NextMethod)
# Lines of S4 code  78586
#
#  Combined with Bioc  S3:  18424
#                      S4: 130247

# So lots more use of classes when using S4, as we might expect. Those who use it, use it a lot.
# Suggests perhaps more software engineering.
# Of course, 3 functions versus 1. And S4 is more verbose.  Not picking up the class() <-


#
# How many use both S3 and S4?
# How many import/depends on methods - DESCRIPTION
#
desc = system("find ~/CRAN2/Pkgs -maxdepth 2 -name DESCRIPTION", intern = TRUE)
um = sapply(dirname(desc), usesMethods)
table(um)
#  FALSE  TRUE 
#  13077  3083 
# So ~ 1450 packages don't use any S4 commands we searched for but still import methods
#
setdiff(names(um)[um], names(tt.s4))
#
# pxweb for example doesn't need methods.



bioc.s3 = system("find ~/Bioconductor/Code -name '*.[Rr]' -type f -exec egrep -H 'UseMethod|[^l]NextMethod' {} \\;", intern = TRUE)
bioc.s4 = system("find ~/Bioconductor/Code -name '*.[Rr]' -type f -exec egrep -H 'setMethod|setGeneric|setClass' {} \\;", intern = TRUE)

pkgs.s3 = getPkgName(bioc.s3, "~/Bioconductor/Code")
pkgs.s4 = getPkgName(bioc.s4, "~/Bioconductor/Code")


saveRDS(list(s3 = bioc.s3,  s4 = bioc.s4), "~/Bioconductor/Code/bioc_s3_s4.rds")

# BioC has 1925 packages.
# The length of - the number of lines with S4 actions - bioc.s4 is 51661, but for bioc.s3 is 544.
length(unique(pkgs.s3))
length(unique(pkgs.s4))

# 113 packages with S3 calls
# 913 packages with S4 calls.

# How many packages import/depend on the methods package
ff = list.files("~/Bioconductor/Code")
dirs = ff[ file.info(ff)$isdir ]
um = sapply(dirs, usesMethods)
#
# FALSE  TRUE 
#  670  1255 
#

ns3 = sapply(dirs, s3Methods)
table(ns3)
# 1626 off the 1925 have no S3method declaration.



# The packages that import/depend methods but don't actually have any setClass/setMethod/setGeneric
setdiff(names(um)[um], unique(pkgs.s4))
# 360 of them.

