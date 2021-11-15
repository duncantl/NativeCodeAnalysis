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
              fn = getName(getCalledFunction(x)) 
              ok = fn %in% c("Rf_error", "Rf_errorcall")
              if(!ok)
                  return(character())

              idx = 1L
              if(fn == "Rf_errorcall")
                  idx = 2L
              
              getValue(x[[idx]][[1]])
          })
