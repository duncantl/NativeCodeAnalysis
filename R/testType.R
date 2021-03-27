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
    #
    # given a function, determine if there are any tests on the parameters that identify the expected type(s)
    # using is* routines and TYPEOF().
    #
function(fun, params = getParameters(fun))
{
   sapply(params, testParamType)
}


testParamType =
    # p is a Argument, i.e. a parameter of an LLVM Function
function(p, uses = getAllUsers(p))
{
    w = sapply(uses, function(x) is(x, "CallBase") && is(cf <- getCalledFunction(x), "Function") && ( grepl("^Rf_is", getName(cf)) || getName(cf) %in% c("TYPEOF")))

    if(!any(w))
        return(NULL)

    w2 = sapply(uses[w], isUsedInErrorTest)
    w2 = sapply(w2, any)

    if(any(w2)) {
        tmp = uses[w][w2]
        fnNames = lapply(tmp, function(x) getName(getCalledFunction(x)))
        w = fnNames == "TYPEOF"
        if(any(w))
            fnNames[w] = sapply(tmp[w], getTYPEOFComparison)

        return(fnNames)
    }

    return(NULL)        
}


isUsedInErrorTest =
function(call)
{
    isTypeof = getName(getCalledFunction(call)) == "TYPEOF"
    
    uses = getAllUsers(call)

    if(isTypeof) {
        is.comp = sapply(uses, function(x) is(x, "CmpInst") && any(sapply(x[], function(v) is(v, "ConstantInt"))))
    } else
        is.comp = sapply(uses, function(x) is(x, "CmpInst") && any(sapply(x[], function(v) is(v, "ConstantInt") && getValue(v) == 0L)))
    
    if(!any(is.comp))
        return(FALSE)


    sapply(uses[is.comp], branchesToError, if(isTypeof) 2L else 3L)
}

# for routine ALIKEC_compare_attributes_internal_simple in package vetr, we get multiple users
#  an or command (BinaryOperator) and a SelectInst.
#  So two and neither is a BranchInst.

branchesToError =
function(cmp, index = 3L)
{
    u = getAllUsers(cmp)
    if(length(u) > 1)
        warning("more than one use of the comparison predicate")
    u = u[[1]]
    if(!is(u, "BranchInst"))
        return(FALSE)

    block = u[[ index ]]
    hasCallToError(block) || leadsToErrorBlock(block)
}

hasCallToError =
function(b, ins = getInstructions(b))
{
   any(sapply(ins, function(i) is(i, "CallBase") && is(cf <- getCalledFunction(i), "Function") && grepl("^Rf_error", getName(cf))))
}


leadsToErrorBlock =
function(b, seen = list())
{
    if(any(sapply(seen, identical, b)))
        return(FALSE)
    
    trm = getTerminator(b)

    if(is(trm, "InvokeInst")) {
        # any(sapply(trm[3:4], hasCallToError))
        any(sapply(trm[][ sapply(trm[], is, "BasicBlock") ], hasCallToError))
   } else if(is(trm, "SwitchInst")) {
       any(sapply(trm[] [ sapply(trm[], is, "BasicBlock")] , hasCallToError))
   } else
       any(sapply(trm[-1], hasCallToError))
}



getTYPEOFComparison =
function(call)
{
    uses = getAllUsers(call)
    is.comp = sapply(uses, function(x) is(x, "CmpInst") && any(sapply(x[], function(v) is(v, "ConstantInt"))))
    sapply(uses[is.comp], function(cmp) { w = sapply(cmp[], is, "ConstantInt")
                                          NativeCodeAnalysis:::getRType(getValue(cmp[][w][[1]]))
                                      })
}
