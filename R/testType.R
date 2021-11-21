#
# This is code that aims to find routines which test the type of a SEXP
# and then raise an error if the test is not true.  These are essentially assertions
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


#
#
# Look at all the packages that contain calls to Rf_error{,call}

# w = sapply(errPkgs, function(x){ m = readBitcode(file.path("~/CRAN2/Pkgs", x, "src/all.bc"));
#            try(sapply(getDefinedRoutines(m, names = FALSE) , function(f) !all(sapply(testType(f), is.null))))})
# table(sapply(w, any))
#
#  179 seem to have code that tests one or more parameters. 
#
#
#  DSL package and _collector2 isn't quite right.  Get's Rf_isNull() but the error is not directly related to this.
#
#   mm = readBitcode("~/CRAN2/Pkgs/DSL/src/all.bc")
#   testType(mm$"_collector2")
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





# R 4.2.0 development.   Built: 10/20/2021  SVN Rev: 81081
# R Under development (unstable) (2021-10-20 r81081)
Rf_IsRoutines =
c("Rf_isArray", "Rf_isBasicClass", "Rf_isBlankString", "Rf_isComplex", 
"Rf_isEnvironment", "Rf_isExpression", "Rf_isFactor", "Rf_isFrame", 
"Rf_isFree", "Rf_isFunction", "Rf_isInteger", "Rf_isLanguage", 
"Rf_isList", "Rf_isLogical", "Rf_isMatrix", "Rf_isNewList", "Rf_isNull", 
"Rf_isNumber", "Rf_isNumeric", "Rf_isObject", "Rf_isOrdered", 
"Rf_isPairList", "Rf_isPrimitive", "Rf_isProtected", "Rf_isReal", 
"Rf_isS4", "Rf_isString", "Rf_isSymbol", "Rf_isTs", "Rf_isUnmodifiedSpecSym", 
"Rf_isUnordered", "Rf_isUnsorted", "Rf_isUserBinop", "Rf_isValidName", 
"Rf_isValidString", "Rf_isValidStringF", "Rf_isVector", "Rf_isVectorAtomic", 
"Rf_isVectorList", "Rf_isVectorizable")

testParamType =
    # p is a Argument, i.e. a parameter of an LLVM Function
    #
    # To find all the Rf_is* routines, we can use 
    #  nm -gj ~/R/R-new/build/bin/exec/R | grep Rf_is 
    #
    # sub("^_", "", system("nm -gj ~/R/R-new/build/bin/exec/R | grep Rf_is  ", intern = TRUE))
    #
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

        return(unique(unlist(fnNames)))
    }

    NULL
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
    if(length(u) == 0)
        return(FALSE)
    
    if(length(u) > 1) {
        # Not certain how robust this is but analyzes all the users, not just the first.
        return(any(sapply(u, function(i)  if(is(i, "BranchInst")) leadsToError(i[[index]]) else branchesToError(i))))
       #  warning("more than one use of the comparison predicate")
    }
    
    u = u[[1]]
    if(!is(u, "BranchInst"))
        return(FALSE)

    block = u[[ index ]]
    leadsToError(block)
}

leadsToError =
function(block)               
    hasCallToError(block) || leadsToErrorBlock(block)

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
