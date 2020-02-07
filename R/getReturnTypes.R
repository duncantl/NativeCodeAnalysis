
getRReturnTypes =
function(fun, ret = getReturnValues(fun), module = as(fun, "Module"), stack = character())
{
    id = getName(fun)
    if(id %in% stack) {
        warning("recursively processing ", id)
        return(structure(c(routine = id), class = "Recursive"))
    }
        
    stack  = c(id, stack)
    
    rty = getReturnType(fun)
    if(identical(rty, VoidType))
        return(NULL)

    if(length(ret) == 0) {
        warning("routine ", getName(fun), " has no return. Maybe calls error().")
        return(NULL)
    }

    
    if(length(ret) > 1)
        warning("more than one return value!")

    ret = ret[[1]]
    if(is(ret, "PHINode"))
        lapply(ret[], pAllocVector, module = module, stack = stack)
    else
       pAllocVector(ret, module, stack = stack)
}


pAllocVector =
function(x, module, stack = character()) 
{
    if(is(x, "CallInst")) {
        fname = getName(getCalledFunction(x))
        if(fname %in% c("Rf_allocVector3", "Rf_allocVector"))
            return(structure(list(type = getRType(getValue(x[[1]])), length = x[[2]]), class = "RVectorType"))

        if(grepl('^Rf_Scalar', fname)) {
            ty = switch(fname,
                        Rf_ScalarReal = "numeric",
                        Rf_ScalarLogical = "logical",
                        Rf_ScalarInteger = "integer",
                        Rf_ScalarString = "character")
            return(structure(list(type = ty, length = 1), class = c('RScalarType', 'RVectorType')))
        }
        
        if(fname %in% names(module) && length(getBlocks(f2 <- module[[fname]]))) 
            return(getRReturnTypes(f2, module = module, stack = stack))
        else
            return(x)
    } else if(is(x, "LoadInst"))
        return(pAllocVector(x[[1]], module, stack))
    else if(is(x, "AllocaInst")) {
#        browser()
        u = getAllUsers(x)
        u = u[!sapply(u, isLoadReturn)]
        lapply(u, pAllocVector, module)
    } else if(is(x, "StoreInst"))
        pAllocVector(x[[1]], module)
    else if(is(x, "CastInst"))
        x[[1]] # pAllocVector(x[[1]], module)
    else if(is(x, "GlobalVariable")) {
        id = getName(x)
        return(switch(id,
                      "R_NilValue" = structure("R_NilValue", class = "NULLType"),
                      R_NaString = structure(list(type = "character", length = 1, value = as.character(NA)), class = c("RScalarType", "RVectorType"))))
    } else if(is(x, "Constant"))
        getValue(x)
    else
        x
}                


isLoadReturn =
function(x)
{
   is(x, "LoadInst") &&  getName(x[[1]]) == "retval"
}
