library(Rllvm)
library(NativeCodeAnalysis)


m = parseIR("class.ir")


mk2 = compReturnType(m$mk2)
# doesn't get the names.

bob = compReturnType(m$doS4)
# gets class name.
