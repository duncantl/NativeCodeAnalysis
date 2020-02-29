#
# Find the assignment of a class to an object in C code.
# For now, not S4. 
# See R_foo7 in  length.{c,ir}

getReturnClass =
function(retVal)
{
    u = getAllUsers(retVal)
    w = sapply(u, isSetClass, retVal)
    if(!any(w))
        return(character())

    unlist(lapply(u[w], function(x) getSetClass(x[[3]])), recursive = FALSE)
}


getSetClass =
    #
    # For now,  assums Rf_setAttrib(x, R_ClassSymbol, val)
    # and we want val.
    #
function(ins)
{
    ans = ins
    
    if(is(ans, "LoadInst"))
        ans = ans[[1]]

    if(is(ans, "CallInst")) {
        ans = switch(getCallName(ans),
            Rf_mkChar = get_mkCharValue(ans[[1]]),
            Rf_ScalarString = getSetClass(ans[[1]]),
            Rf_getAttrib = {
                if(isClassSymbol(ans[[2]]))
                    return(structure(list(obj = ans[[1]]), class = "ClassOf"))
                else
                    ans
            },
                     {browser(); ans})
    } else if(is(ans, "ConstantExpr")) {
        browser()
    } else if(is(ins, "PHINode")) {
        ans = lapply(ins[], getSetClass)
    } else
       ans
}


isClassSymbol =
function(x)
    isBuiltinRSymbol(x, "R_ClassSymbol")

isBuiltinRSymbol =
function(x, symName)    
{
    if(is(x, "LoadInst"))
        x = x[[1]]

    is(x, "GlobalVariable") && getName(x) == symName

}
