#
# The goal here is to find the names of elements of a returned list (for now, may do vectors also)
# when they are "fixed" rather than computed.
# E.g. cusp/src/cusp.nc.c and its cuspnc routine.
# It returns a list with 4 elements named
#  vallue, abs.error, subdivisions, ierr.
# We'll use a simple approach until we find more complex cases
#
#
#  m = parseIR("~/CRAN/Pkgs/cusp/src/cusp.nc.ir"); 
#  getReturnValueNames(m$cuspnc)
#
#


#getCallName = NativeCodeAnalysis:::getCallName

getReturnValueNames =
function(fun)
{
    rv = getReturnValues(fun)
    
    setn = lapply(rv, findSetNames)
    lapply(setn, getNameVectors)
}

findSetNames =
function(rexp)
{
    uses = getAllUsers(rexp)
    uses[sapply(uses, isSetNames, rexp)]
}

isSetNames =
function(e, on)
  isSetAttrib(e, on, "R_NamesSymbol")    

isSetClass =
function(e, on)
    isSetAttrib(e, on, "R_ClassSymbol")

isSetAttrib =    
function(e, on, sym)
{
    is(e, "CallInst") && getCallName(e) == "Rf_setAttrib" &&
        is(e[[2]], "LoadInst") && is(e[[2]][[1]], "GlobalVariable") &&
            getName(e[[2]][[1]]) == sym &&
               identical( e[[1]],  on)
}


    



getNameVectors =
function(x)
{
   unlist(lapply(x, getNameVector))
}

getNameVector =
function(x)
{
    x = x[[3]] # 3rd arg. in Rf_setAttrib()
    u = getAllUsers(x)
    lapply(u, getSetStringEltValue)
}

getSetStringEltValues =
function(ins)
{
    w = sapply(ins, function(x) is(x, "CallInst") && getCallName(x) == "SET_STRING_ELT")
    sapplly(ins[w], getSetStringEltValue)
}

getSetStringEltValue =
function(ins)    
{
    if(is(ins, "CallInst") && getCallName(ins) %in% c("Rf_protect", "Rf_allocVector" ))
        return(list())
    
    val = ins[[3]]
    if(is(val, "CallInst")) {
        switch(getCallName(val),
               "Rf_mkChar" =  get_mkCharValue(val[[1]]),
               "Rf_allocVector" = list(),
                {browser(); NA})
    } else
        list()
}

get_mkCharValue =
function(x)
{
    if(is(x, "ConstantExpr"))
        getValue(x[[1]])
    else if(is(x, "SelectInst")) {
       sapply( x[-1], get_mkCharValue)
    } else {
#        browser()
        NA
    }
}
