#!!!!  These should move to Rllvm, but we need to check that they fit the the
# idea of getValue.  The function is more for converting LLVM values to R, i.e. literal values.
# These methods extend it to more symbolic information, i.e., Arguments, 

setMethod("getValue", "StoreInst",
          function(x, ...)
             x[[1]]
          )

getValue_NoLoadInst =
function(x, ...) {
             u = getAllUsers(x)
             u = u[!sapply(u, is, "LoadInst")]
             u = lapply(u, getValue)
             if(length(u) == 1)
                u = u[[1]]
             u
         }

setMethod("getValue", "LoadInst", function(x, ...) getValue_NoLoadInst(x[[1]], ...))
setMethod("getValue", "AllocaInst", getValue_NoLoadInst)

setMethod("getValue", "SelectInst",
          function(x, ...)
             sapply(x[-1], getValue)
          )

setMethod("getValue", "CallInst",
          function(x, ...) {
              id = getCallName(x)
              switch(id,
                     Rf_str2type = structure(NA, class = "SEXPTYPE"),
                     NA)
          })

setMethod("getValue", "CastInst",
          function(x, ...) 
              getValue(x[[1]])
          )
# -----------

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
        warning("more than one return value. Processing first one!")

    ret = ret[[1]]
    # What about getCallType(), compReturnType()
    tmp = pAllocVector(ret, module, stack = stack)
    .trim(tmp)
}

.trim =
function(tmp)
{
    # If we have a list with a class other than list, leave that alone.
    if(length(class(tmp)) > 1 || class(tmp) != "list")
        return(tmp)
    
    tmp = tmp[ ! sapply(tmp, is.null) ]
    if(length(tmp) == 1)
        tmp[[1]]
    else
        tmp
}

followAllPhis =
    #
    # given a PHI node, typically the return value of a routine,
    # recursively/iteratively follow all the values  and unravel the phi nodes.
    #
function(x)
{
#    browser()
    tmp = ans = x[]
    while(any(  w <- sapply(tmp, is, "PHINode") )) {
        tmp = unlist(lapply(ans[w],`[`))
        w = !(tmp %in% ans)
        tmp = tmp[w]
        ans = c(ans, tmp)
#        ans = ans[ - which(w) ]
        ans = unique(ans)
    }

    ans
}

pAllocVector =
    #
    # How does this relate to getCallType() and compReturnType()
    #
function(x, module = as(x, "Module"), stack = character(), prev = list()) 
{
    if(any(sapply(prev, identical, x)))
        return(NULL)

    prev = c(prev, x)
    if(is(x, "LoadInst"))
        return(pAllocVector(x[[1]], module, stack, prev))

#    if(is(x, "CallInst") &&  getName(getCalledFunction(x)) == "allocMatrixNA") browser()

    
    if(is(x, "PHINode")) {
        # "unravel" this phi and all the nodes it points to get rid of the PHI nodes
        # and end up with the unique Value objects that are not PHI nodes.
        v = followAllPhis(x)
        ans = lapply(v, pAllocVector, module, stack)
        return(.trim(ans))
#        return(lapply(unique(x[]), pAllocVector, module = module, stack = stack, prev = prev))
    }

    
    if(is(x, "CallInst")) {
        fname = getName(getCalledFunction(x))
        if(is.na(fname))
            return(structure(list(NULL), class = "IndirectRoutineCall"))
        
        if(fname %in% c("Rf_protect", "Rf_duplicate"))
            return(.trim(pAllocVector(x[[1]], module, stack, prev)))

        if(fname == "Rf_coerceVector") {
            sexpty = x[[2]]
            if(!is(sexpty, "Argument"))
                sexpty = getRType(getValue(sexpty))
            ans = structure(list(type = sexpty, length = NA, class = "RVectorType"))
            return(ans)
        } else if(fname %in% c("Rf_allocVector3", "Rf_allocVector", "Rf_allocSExp")) {
            sexpty = x[[1]]  
            # If the type is an argument, then we cannot determine the R type
            # except for via calls to this routine.
            #?? Should we have getRType() handle Argument  and PHINode objects
            if(is(sexpty, "PHINode"))
                 # !!! would like to keep the types with the lengths.
                sexpty = sapply(sexpty[], getValue)
            else if(!is(sexpty, "Argument"))
                sexpty = getRType(getValue(sexpty))

            # if the first argument is TYPEOF(arg), want to capture that symbollically.
            ans = structure(list(type = sexpty, length = mkLength(x[[2]])), class = "RVectorType")
            if(is.character(ans$type) && ans$type == "list") 
                ans$elTypes = getElementTypes(x, module)

            names = getReturnValueNames(rv = list(x))
            if(length(names)) 
                ans$names = names[[1]]
            
            return(ans)
        } else if(fname == "RfallocS4Object") {
            return(list(), class = "GenericS4Object")
        }

        if(fname %in% c("Rf_allocMatrix")) {
            #[check] need to make this smarter. If it is not a literal value
            # we need to figure out what the value might be, e.g., a parameter
            # a computation such as TYPEOF(otherObj)
            # e.g. do_readDCF in dcf.c
            elTy = getRType(getValue(x[[1]]))
            ans = structure(list(type = "Matrix",
                                 elType = elTy,
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
            ans = structure(list(type = ty, length = 1), class = c('RScalarType', 'RVectorType'))
            return(ans)
        }

        if(fname == "Rf_mkString") {
            ans = structure(list(type = "character", length = 1), class = c('RScalarType', 'RVectorType'))
            return(ans)
        }
        
        if(fname %in% names(module) && length(getBlocks(f2 <- module[[fname]]))) {
            # analyze the function, but then see if the call provides additional information.
            ans = getRReturnTypes(f2, module = module, stack = stack)
#            browser()
            ans = mergeCallWithReturnTypes(ans, x)
            return(ans)
        } else {
            print(x)
 #           browser()
            return(x)
        }
    } else if(is(x, "LoadInst"))
        return(.trim(pAllocVector(x[[1]], module, stack, prev)))
    else if(is(x, "AllocaInst")) {
        u = getAllUsers(x)
        u = u[!sapply(u, isLoadReturn)]
        .trim(lapply(u, pAllocVector, module, stack, prev))
    } else if(is(x, "StoreInst"))
        pAllocVector(x[[1]], module, stack, prev)
    else if(is(x, "CastInst"))
        x[[1]] # pAllocVector(x[[1]], module)
    else if(is(x, "GlobalVariable")) {
        id = getName(x)
        return(switch(id,
                      "R_NilValue" = structure("R_NilValue", class = "NULLType"),
                      "R_LogicalNAValue" = structure("R_LogicalNAValue", class = "logical"),
                      "R_FalseValue" = structure("R_FalseValue", class = "logical"),
                      "R_TrueValue" = structure("R_TrueValue", class = "logical"),
                      R_NaString = structure(list(type = "character", length = 1, value = as.character(NA)), class = c("RScalarType", "RVectorType"))))
    } else if(is(x, "Constant"))
        getValue(x)
    else if(is(x, "SelectInst")) {
        #XXXX Fix up and the code above for LogicalNAValue, FalseValue and TrueValue.
        # Get the type descriptions right.
        .trim(lapply(x[-1], pAllocVector, module, stack, prev))
    }  else
        x
}                

#trace(pAllocVector, exit = quote(if(!is.null(returnValue())) { print(x); print(class(returnValue()))}), print = FALSE)

mergeCallWithReturnTypes =
    #
    # Needs to handle case when arguments in call are not simple values
    # passed by the caller to the routine, but computations that are passed
    #
    # ans is the list of possible return value types from the routine being called in call
    # call is the specific call to the routine, so provides information.
    #
    # example that needs to be handled:
    # m2 = parseIR("NativeCodeAnalysis/examples/argType.ir")
    # getRReturnTypes(m2$bar)
    #
    # Works
    # m = parseIR("~/R-devel/build/src/library/stats/src/random.ir")
    # z = getRReturnTypes(m$do_rbinom)
    #
function(ans, call)
{

    if(is(ans, "Argument")) {
        i = paramIndex(ans)
        return(pAllocVector(call[[i]]))
    }

    
    test = function(x) (is(x, 'RVectorType') && is(x$type, "Argument")) ||
                         (is(x, 'RMatrixType') && is(x$elType, "Argument"))
    
    if(unl <- test(ans)) {
        ans = list(ans)
        w = TRUE
    } else
           # could still be an Argument, not a list of types.
        w = sapply(ans, test)
    
    if(any(w)) 
        ans[w] = lapply(ans[w], substituteArgInType, call)

    if(unl)
        ans = ans[[1]]
    ans
}

substituteArgInType =
function(type, call)
{
    el = if(is(type, "RVectorType")) "type" else "elType"
    i = paramIndex(type[[el]])
    v = call[[i]]
    # Could be a literal value or a computation, e.g.,
    # TYPEOF(x).
    # If TYPEOF(x), then want to represent this symbolically and determine what x
    # is a local variable or a parameter.
    tmp = findValue(v)

    if(!is.null(tmp))
       type[[el]] = getRType(tmp)
    type
}

paramIndex =
function(p, fun = getParent(p), params = getParameters(fun))
{
   which( sapply(params, identical, p) )
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
    w = sapply(uses, function(u)
                         is(u, "CallInst") && getCallName(u) == "SET_VECTOR_ELT" &&
                          # ignore where x is used as a value not the object into which a
                          # value is being inserted, e.g. SET_VECTOR_ELT(other, , x)       
                           identical(u[[1]], x))
    sets = uses[w]

    ans = lapply(sets, function(x) pAllocVector(x[[3]], module))

    idx = sapply(sets, function(x) asIndex(x[[2]]))  # Handle symbolic values SET_VECTOR_ELT(x, i++, value).  if() statements.
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



mkLength =
    #
    #
    #
function(x)
{
    #browser()
    x = unravel(x)

    if(is(x, "CallInst")) {
        rtn = getCallName(x)
        if(rtn %in% c("Rf_length", "LENGTH", "XLENGTH"))
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
                 shl = substitute(2^a*b, lens),  # How does this map to typesys.
                 x
               )
    } else if(is(x, "GetElementPtrInst")) {
        getElementOf(x)
    } else if(is(x, "SelectInst")) {
        structure(lapply(x[-1], mkLength), class = c("SelectLengthOf", "LengthOf"))
    } else if(is(x, "PHINode")) {
        structure(lapply(x[], mkLength), class = c("SelectLengthOf", "LengthOf"))
    } else 
        getValue(x)

    # Also handle BinaryOperator - but here or in getValue() or what? How to make
    # this symbolic
    
}

#trace(mkLength, exit = browser, print = FALSE)


getElementOf =
function(x)
{
    val = x[[1]]
    idx = 0
# browser()    

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
