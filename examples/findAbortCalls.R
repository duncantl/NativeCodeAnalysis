library(Rllvm)
library(NativeCodeAnalysis)
source("../../NativeCodeAnalysis/R/findAbort.R")
# These are in Rllvm/src/
cfiles = c("Constants.ir", "IRBuilder.ir", "Intrinsics.ir", "metadata.ir", "types.ir")
ll.mods = lapply(file.path("../../Rllvm/src", cfiles), parseIR)
names(ll.mods) = cfiles
#  How is findFunUses related to getCallsTo
zz = lapply(ll.mods, function(mod) try(findFunUses("abort", mod)))
e = sapply(zz, is, 'try-error')
if(any(e))
  message("Failed for ", paste(cfiles[e], collapse = ", "))


ids = sapply(zz, function(x) demangle(sapply(x, getName)))
