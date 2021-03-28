library(Rllvm)
m = parseIR("RcppEgO2.ir")
funs = getModuleFunctions(m)
f = funs[[grep("row_max", names(funs))]]

compReturnType(f)


