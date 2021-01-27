
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
              lapply(getBlockInstructions(x), getCalledRoutines)
          })


setMethod("getCalledRoutines", "CallInst",
          function(x, ...) {
              getName(getCalledFunction(x))
          })

setMethod("getCalledRoutines", "Instruction",
          function(x, ...) {
              character()
          })

setMethod("getCalledRoutines", "MemSetInst",
          function(x, ...) {
              character()
          })
              

library(igraph)

setGeneric("callGraph",
           function(x, adjacency = FALSE, ...)
              standardGeneric("callGraph"))



setMethod("callGraph", "character",
          function(x, adjacency = FALSE, pattern = "\\.(ll|ir)", ...) {
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
              callGraph(structure(mods, class = "ListOfModules"), adjacency)
          })



setMethod("callGraph", "Module",
          function(x, adjacency = FALSE, within = TRUE, ...) {
              funs = getDefinedRoutines(x, names = FALSE)
              calls = lapply(funs, function(f) getCalledRoutines(f))

              # NAs are for calls via routine pointers.
              
              tmp = if(within)
                       lapply(calls, function(x) unique(x[!is.na(x)]) )
                    else
                       lapply(calls, function(x) intersect(x[!is.na(x)], names(funs)))
              
              d = data.frame(from = rep(names(tmp), sapply(tmp, length)), to = unlist(tmp), stringsAsFactors = FALSE)
              if(adjacency)
                  d
              else
                  igraph::graph_from_data_frame(d)    
          })
