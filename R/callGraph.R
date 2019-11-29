
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



setMethod("callGraph", "character",
          function(x, pattern = "\\.(ll|ir)", ...) {
              # if is a file name, parse the module
              # if a directory, parse all the IR modules
              # if a mixture of file names and directories, merge the file names from the directories into the full list of file names.

              e = file.exists(x)
              if(any(!e))
                  stop("files don't exist", paste(x[!e], collapse = ", "))
              
              info = file.info(e)
              pdir = info$isdir
              if(any(pdir)) {
                  tmp = unlist(lapply(x[pdir], list.files, pattern = patter, full.names = TRUE))
                  x = c(x[!pdir], tmp)
              }
              
              mods = lapply(x, parseIR)
              callGraph(structure(mods, class = "ListOfModules"))
          })



setMethod("callGraph", "Module",
          function(x, within = TRUE, ...) {
              funs = getDefinedRoutines(, x)
              calls = lapply(funs, function(x) getCalledRoutines(sh[[x]]))
              names(calls) = funs
               # NAs are for calls via routine pointers.
              tmp = lapply(tmp, function(x) unique(x[!is.na(x)]) )
              d = data.frame(from = rep(names(tmp), sapply(tmp, length)), to = unlist(tmp), stringsAsFactors = FALSE)
              igraph::graph_from_data_frame(d)    
          })
