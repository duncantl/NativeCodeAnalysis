setGeneric("getRErrorMessages",
           function(x, ...)
              standardGeneric("getRErrorMessages"))

setMethod("getRErrorMessages", "Module",
          function(x, ...) {
              rr = getDefinedRoutines(x, names = FALSE)
              lapply(rr, getRErrorMessages)
          })

setMethod("getRErrorMessages", "Function",
          function(x, ...) {
              unlist( sapply(getInstructions(x), getRErrorMessages) )
          })


setMethod("getRErrorMessages", "Instruction",  
          function(x, ...) 
             character(0))

setMethod("getRErrorMessages", "CallBase",
          function(x, ...) {
              ok = getName(getCalledFunction(x)) %in% c("Rf_error", "Rf_errorcall")
              if(!ok)
                  return(character())

              getValue(x[[1]][[1]])
          })
