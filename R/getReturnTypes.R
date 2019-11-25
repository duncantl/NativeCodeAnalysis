
getRReturnTypes =
function(fun, ret = getReturnValues(fun), module = as(fun, "Module"))
{
    if(length(ret) > 1)
        warning("more than one return value!")

    ret = ret[[1]]
browser()
    if(is(ret, "PHINode"))
        lapply(ret[], pAllocVector, module = module)
    else
       pAllocVector(ret, module)
}


pAllocVector =
function(x, module) 
{
    if(is(x, "CallInst")) {
        fname = getName(getCalledFunction(x))
        if(fname == "Rf_allocVector3")
            return(structure(list(type = getRType(x[[1]]), length = x[[2]]), class = "RVectorType"))

        f2 = module[[fname]]
        return(getRReturnTypes(f2, module = module))
    } else if(is(x, "LoadInst"))
       return(x[[1]])

    x
}                
