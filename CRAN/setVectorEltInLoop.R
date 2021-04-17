library(Rllvm)
library(NativeCodeAnalysis)

getSVEInLoop =
function(file, mod = readBitcode(file), ins = unlist(getInstructions(mod)) )
{
    w = sapply(ins, NativeCodeAnalysis:::isCallTo, "SET_VECTOR_ELT")
    if(!any(w))
        return(list())

    ins = ins[w]
    w2 = sapply(ins, isInLoop)
    ins[w2]
}

isInLoop =
function(ins, fun = as(ins, "Function"))
{
    la = loopAnalysis(fun)
    loops = getLoops(la)
    any(sapply(loops, contains, ins))
}

insInBlocks =
    # Not needed
function(blocks, ins)
{
  any(sapply(blocks, function(b) contains(getInstructions(b), ins)))
}

if(FALSE) {

bcs = system("find ~/CRAN2 -name all.bc", intern = TRUE)
names(bcs) = dirname(bcs)

mods = lapply(bcs, readBitcode)
system.time({tmp = lapply(mods, function(mod) { print(getName(mod)); try(getSVEInLoop(mod))}) })
tmp = tmp[sapply(tmp, length) > 0]
funs = lapply(tmp, function(x) table(sapply(x, function(x) getName(as(x, "Function")))))
saveRDS(funs, "RoutinesWithSetVectorEltInLoops.rds")
}



if(FALSE) {

m = readBitcode("~/CRAN2/Pkgs/corpus/src/all.bc")

runLoopPass(m)

r = getDefinedRoutines(m, names = FALSE)

la = loopAnalysis(r$text_count)
loops = getLoops(la)



las = lapply(r, loopAnalysis)
loops2 = lapply(las, getLoops)
n = sapply(loops2, length)
table(n)
}
