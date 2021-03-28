library(NativeCodeAnalysis)


m = parseIR("testType.ir")
funs = getDefinedRoutines(m, names = FALSE)
lapply(funs, testType)

if(FALSE) {
    m2 = readBitcode("~/CRAN2/Pkgs/fansi/src/all.bc")
    testType(m2$FANSI_state_at_pos_ext)
}


if(FALSE)
{
    errPkgs = readRDS("../CRAN/PackagesCallingErrorInC.rds")
    w = sapply(errPkgs, function(x){
                           m = readBitcode(file.path("~/CRAN2/Pkgs", x, "src/all.bc"))
                           try(sapply(getDefinedRoutines(m, names = FALSE) , function(f) !all(sapply(testType(f), is.null))))
                        })
}
