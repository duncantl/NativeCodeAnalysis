#
# The goal is to analyze R packages (CRAN, BioC, ...) and find which
# routines they call from the R API.
#
#  We can count the number of calls across all routines in the package to these routines.
#
#
#
#
#
#
#
#

library(NativeCodeAnalysis)
library(Rllvm)
#source("../R/getCalledFunctions.R")


# Find the packages we have compliled modules
bcs = system("find ~/CRAN2 -name all.bc", intern = TRUE)
names(bcs) = basename(dirname(dirname(bcs)))

# 17 minutes.
cf = lapply(bcs, getCalledFunctions)

Rmain = parseIR("~/R-devel/build/src/main/all.bc")
rfuns = getDefinedRoutines(Rmain, names = FALSE)


## Calls to Routines in R

cf2 = lapply(cf, function(x) lapply(x, function(x) x[(x %in% names(rfuns)) ]))
cf3 = lapply(cf2, function(x) x[ sapply(x, length) > 0 ])

tt = sort(table(unlist(cf3)), decreasing = TRUE)

saveRDS(cf3, "RRoutinesCalledFromPkgs.rds")

#
# 238 routines used in 1645 packages. These packages are the ones not using Rcpp.
#


# Which of the routines return a SEXP
w = sapply(rfuns, function(x) sameType(getReturnType(x), SEXPType))
table(w)
# FALSE  TRUE 
#  1565   964 


# R
# Now find the more specific R type being returned for these routines.
rsexp = intersect(unique(unlist(cf3)) ,  names(rfuns)[w])
ftmp = rfuns[rsexp]
rty = lapply(ftmp[ names(ftmp) != "Rf_eval" ], function(x) { print(getName(x)); compReturnType(x)})




##########

# Return typee of Rf_installTrChar
unique(compReturnType(Rmain$Rf_installTrChar))


#########
#
# Table of contents of R's src/main *.c files.
#

ir = list.files("~/R-devel/build/src/main", pattern = "\\.ir$", full = TRUE)
rr = lapply(ir, getDefinedRoutines)
rtoc = data.frame(routine = unlist(rr), file = rep(basename(ir), sapply(rr, length)), stringsAsFactors = FALSE)

saveRDS(rtoc, "RmainTOC.rds")



#########
# Rf_setAttrib Symbol names
#
# We look at all the calls to Rf_setAttrib to find which
# 

mod = lapply(bcs, parseIR)
names(mod) = basename(dirname(dirname(bcs)))

source("rapiFuns.R")
setAttrW = lapply(mod, function(m) {print(getName(m)); getSetAttrCalls(m)})
setAttrW = setAttrW[ sapply(setAttrW, length)  > 0]

source("../R/getType.R")
zz = sapply(setAttrW, function(x) sapply(x, getSymbolName))

# Now no NAs 
#na = mapply(function(x, v) x[is.na(v)], setAttrW, zz)
#na = na[sapply(na, length) > 0]

tt = sort(table(unlist(zz)), decreasing = TRUE)
saveRDS(tt, "SetAttribSymbolNames.rds")




#####
# Sets S3 Class Attribute

hasS3 = sapply(zz, function(x) any(c("R_ClassSymbol", "class") %in% x))
pkgsSetS3Class = names(hasS3)[hasS3]
saveRDS(pkgsSetS3Class, "PkgsSetS3Class.rds")

#######################
# S4 classes in C code

S4RoutineNames = c("R_do_slot", "R_has_slot", "do_AT", "S4_extends", "R_S4_extends", "R_do_MAKE_CLASS", "R_do_slot_assign", "R_do_new_object")

# Takes about 20 minutes.
s4Calls = lapply(mod, getCallsTo, S4RoutineNames)
nn = sapply(s4Calls, length)
s4CPkgs = names(s4Calls)[nn > 0]

saveRDS(s4CPkgs, "S4CPkgs.rds")

