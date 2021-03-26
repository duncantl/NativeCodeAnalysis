#
# This is code that aims to find routines which test the type of a SEXP
# and raise an error if the test is not true.  These are essentially assertions
# that tell us what the expected type is. These provide slightly more information about the 
# type than how it is used (e.g. inferParamType)


#
# We are looking for code in the form
# foo(SEXP x)
# {
#     if(!isReal(x)) {
#         PROBLEM ""
#          ERROR;
#     }
# }
#

testType =
function(fun, params = getParameters(fun))
{
   sapply(params, testParamType)
}


testParamType =
function(p, uses = getAllUsers(p))
{
browser()
    w = sapply(uses, function(x) is(x, "CallBase") && is(cf <- getCalledFunction(x), "Function") && ( grepl("^Rf_is", getName(cf)) || getName(cf) %in% c("TYPEOF")))

    if(!any(w))
        return(NULL)

    w2 = sapply(uses[w], isUsedInErrorTest)

    if(any(w2)) {

        tmp = uses[w][w2]
        return(sapply(tmp, function(x) getName(getCalledFunction(x))))
    }

    return(NULL)        
}


isUsedInErrorTest =
function(call)
{
    uses = getAllUsers(call)

    is.comp = sapply(uses, function(x) is(x, "CmpInst") && any(sapply(x[], function(v) is(v, "ConstantInt") && getValue(v) == 0L)))
    if(!any(is.comp))
        return(FALSE)

    
    sapply(uses[is.comp], branchesToError)
}

branchesToError =
function(cmp)
{
    u = getAllUsers(cmp)
    if(length(u) > 1)
        warning("more than one use of the comparison predicate")
    u = u[[1]]
    if(!is(u, "BranchInst"))
        return(FALSE)

    block = u[[3]]
    hasCallToError(block)
}

hasCallToError =
function(b, ins = getInstructions(b))
{
   any(sapply(ins, function(i) is(i, "CallBase") && is(cf <- getCalledFunction(i), "Function") && grepl("^Rf_error", getName(cf))))
}

