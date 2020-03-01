#source("~/GitWorkingArea/NativeCodeAnalysis/R/externalParamTypes.R")
#getCallName = NativeCodeAnalysis:::getCallName


m = parseIR("~/CRAN/Pkgs/cusp/src/cusp.nc.ir")
a = inferExternalParamTypes(m$cuspnc)

stopifnot(length(a) == 7)


