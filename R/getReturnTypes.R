
getRReturnTypes =
function(fun, ret = getReturnValues(fun), module = as(fun, "Module"), stack = character())
{
    if(is.null(fun))
        stop("passed NULL for routine")
    
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
    pAllocVector(ret, module, stack = stack)    
}


pAllocVector =
function(x, module, stack = character()) 
{
    if(is(x, "PHINode"))
        return(lapply(x[], pAllocVector, module = module, stack = stack))

    
    if(is(x, "CallInst")) {
        fname = getName(getCalledFunction(x))
        if(fname %in% c("Rf_allocVector3", "Rf_allocVector")) {
            ans = structure(list(type = getRType(getValue(x[[1]])), length = mkLength(x[[2]])), class = "RVectorType")
            if(ans$type == "list") 
                ans$elTypes = getElementTypes(x, module)

            return(ans)
        }

        if(fname %in% c("Rf_allocMatrix")) {
            ans = structure(list(type = "Matrix",
                                 elType = getRType(getValue(x[[1]])),
                                 dim = lapply(x[2:3], mkLength)),
                            class = "RMatrixType")
            return(ans)
        }
        
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



getElementTypes =
    # x is the call to Rf_allocVector() or similar
    # So we want to find the expressions that insert elements
    # and what types those values being inserted are.
function(x, module, uses = getAllUsers(x))
{
    w = sapply(uses, function(x) is(x, "CallInst") && getCallName(x) == "SET_VECTOR_ELT")
    sets = uses[w]
    ans = lapply(sets, function(x) pAllocVector(x[[3]], module))
    browser()    
    idx = sapply(sets, function(x) asIndex(x[[2]]))
    if(is(idx, "numeric"))
       ans = ans[order(idx)]
    
    ans    
}

asIndex =
function(x)    
{
    x = unravel(x)
    if(is(x, "Constant"))
        return(getValue(x))

    x
}

unravel =
function(x)
{
    if(is(x, "CastInst"))
        x = x[[1]]

    if( is(x, "LoadInst"))
        x = x[[1]]
    
    x
}


mkLength =
    #
    #
    #
function(x)
{
    x = unravel(x)

    if(is(x, "CallInst")) {
        rtn = getCallName(x)
        if(rtn == "Rf_length")
            return(LengthOf(x[[1]]))
        else {
            if(rtn %in% c("INTEGER", "LOGICAL", "REAL")) {
               return(getElementOf(x))

            } else
               x
        }
    } else if(is(x, "Constant"))
         # we may want to just return x as it has the precise type e.g. int64.
        structure(getValue(x), names = class(x), type = getType(x))
    else if(is(x, "BinaryOperator")) {
        lens = lapply(x[], mkLength)
        names(lens) = c("a", "b")
          switch(names(getOpcode(x)),
                 shl = substitute(2^a*b, lens),
                 x
               )
    } else if(is(x, "GetElementPtrInst")) {
        getElementOf(x)
   
    } else
        x
}


getElementOf =
function(x)
{
    val = x[[1]]
    idx = 0
 browser()    

    if(is(val, "CallInst")) {
        if(getCallName(val) %in% c("INTEGER", "LOGICAL", "REAL"))
            val = val[[1]]
        else if(getCallName(val) == "VECTOR_ELT") {
            idx = mkLength(x[[2]])
        }
    }

    if(is(x, "CallInst") && getCallName(x) %in% c("VECTOR_ELT")) 
        idx = mkLength(x[[2]])
        
    if(is(val, "CallInst") && getCallName(val) %in% c("VECTOR_ELT")) 
        val = getElementOf(val)
    
    if(is(x, "GetElementPtrInst")) 
        idx = mkLength(x[[2]])

    structure(list(obj = val, index = idx),  class = "ElementOf")
}
