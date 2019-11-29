
# Routine call graph via LLVM


setGeneric("getCalledRoutines",
           function(x, ...)
              standardGeneric("getCalledRoutines"))

setMethod("getCalledRoutines", "Function",
          function(x, ...) {
              getCalledRoutines(getBlocks(x), ...)
          })


setMethod("getCalledRoutines", "list",
          function(x, ...) {
             unname(unlist( lapply(x, getCalledRoutines, ...) ))
          })

setMethod("getCalledRoutines", "BasicBlock",
          function(x, ...) {
              lapply(x[], getCalledRoutines)
          })


setMethod("getCalledRoutines", "CallInst",
          function(x, ...) {
              getName(getCalledFunction(x))
          })

setMethod("getCalledRoutines", "Instruction",
          function(x, ...) {
              character()
          })
              

library(igraph)
setGeneric("callGraph",
           function(x, ...)
             standardGeneric("callGraph"))


setMethod("callGraph", "Module",
          function(x, ...) {
              funs = getDefinedRoutines(, x)
              calls = lapply(funs, function(x) getCalledRoutines(sh[[x]]))
              names(calls) = funs
              tmp = lapply(tmp, function(x) unique(x[!is.na(x)]) )
              d = data.frame(from = rep(names(tmp), sapply(tmp, length)), to = unlist(tmp), stringsAsFactors = FALSE)
              igraph::graph_from_data_frame(d)    
          })
