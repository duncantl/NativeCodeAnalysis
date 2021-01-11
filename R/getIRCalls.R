
# Need to think this through more.
#  Call it getIRCalls ?
# Want to get the external calls. But can do this by getting all the calls
# and mkRoutinefileTOC() and computing the difference.

#
#
# See Coverage.xml

setGeneric("getCalls", function(x, ...) standardGeneric("getCalls"))

setMethod("getCalls", "Module",
          function(x, names = TRUE, ...) {
              r = getDefinedRoutines(module = x, names = FALSE)
              ins = unlist(lapply(r, getCalls, names, ...))
          })

setMethod("getCalls", "Function",
          function(x, names = TRUE, ...) {
              ins = getInstructions(x)
              getCalls(ins, names = names, ...)
          })

setMethod("getCalls", "list",
          function(x, names = TRUE, ...) {
              ins = x
              w = sapply(ins, is, "CallInst")
              if(!names)
                  return(ins[w])
              
              sapply(ins[w], function(x) getName(getCalledFunction(x)))              
          })
