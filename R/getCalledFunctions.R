# Want a generic for this to apply to Module, Function, BasicBlock?
setGeneric("getCalledFunctions",
           function(x, external = TRUE, ...)
           standardGeneric("getCalledFunctions"))


setMethod("getCalledFunctions",
          "character",
          function(x,  external = FALSE, ...)
          {
              getCalledFunctions(parseIR(x), external, ...)
          })

setMethod("getCalledFunctions",
          "Module",
          function(x,  external = FALSE, ...)
{
    defs = getDefinedRoutines(getName(x), module = x, names = FALSE)
    lapply(defs, getCalledFunctions, external = external, module = x, defs = names(defs) )
})

setMethod("getCalledFunctions", "Function",
           function(x, external = FALSE, defs = character(), ...)
{
    ins = getInstructions(x)
    w = sapply(ins, is, "CallBase")
    ans = sapply(ins[w], function(x) getName(getCalledFunction(x)))
    ans = ans[!grepl("^llvm", ans)]

    if(external && length(defs))
        ans [ ! (ans %in% defs) ]
    else
        ans
})
   
