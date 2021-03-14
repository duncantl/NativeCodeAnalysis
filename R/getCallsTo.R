
setGeneric("getCallsTo", function(x, ids, ...) standardGeneric("getCallsTo"))

setMethod("getCallsTo", "Module",
          function(x, ids, ...) {
              ins = unlist(getInstructions(x))
              w = sapply(ins, function(x) is(x, "CallBase") && is(cf <- getCalledFunction(x), "Function") )
              cnames = sapply(ins[w], function(x) getName(getCalledFunction(x)))
              ins[w][cnames %in% ids]
          })

