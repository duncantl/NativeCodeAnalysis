inferLength =
    #
    # p is an Argument/Parameter of a routine.
    #
function(p)
{
   u = getAllUsers(p)
   w = sapply(u, isElementAccessor)
   if(!any(w)) {
       if(all(sapply(u, is, "LoadInst")) && !isSEXPType(getType(p)))
           return(structure(1L, class = "RScalarType"))
       else
           return(NA)
   }

   ans = lapply(u[w], findElAccessLength)
   names(ans) = sapply(u[w], as, "character")
   
   if(sum(w) == 1)
       return(ans[[1]])
   
   ans
}


isElementAccessor =
    #
    # Determines whether this instruction access elements of an R object
    # This is not for .C() calls, but .Call() and .External() routines working
    # on SEXP objects.
function(ins)
{
    if( is(ins, "CallInst") ) {
        fn = getCallName(ins)
        fn %in% c("REAL", "INTEGER", "LOGICAL", "STRING_ELT", "VECTOR_ELT")
    } else if(is(ins, "GetElementPtrInst") )
        TRUE
    else {
        # browser()
        # Could be a return statement.
        FALSE
    }
}


findElAccessLength =
    #
    # Determines the symbolic length of the object being accessed in ins.
    # Again, this is for SEXP objects, not primitive C types in .C() routines.
    # 
function(ins)
{
    idx = NULL
    if(is(ins, "CallInst")) {
        fn = getCallName(ins)
        if(fn %in% c("STRING_ELT", "VECTOR_ELT")) {
            idx = ins[[2]]
        } else 
            return(lapply(getAllUsers(ins), findElAccessLength))
        
    } else if(is(ins, "GetElementPtrInst")) {
        idx = ins[[2]]
    }

    if(is.null(idx))
        return(NA)

    if(is(idx, "CastInst"))
        idx = idx[[1]]
    
    var = getName(idx)
    if(is(idx, "PHINode"))
        # order may not be always correct.
        # Just putting names here to make it easier to understand what we are looking at in the output.
       return(structure(lapply(idx[], findIndexRange, var), names = c("start", "end")) ) 

    else if(is(idx, "Constant"))
        return(getValue(idx))

    browser()
}


findIndexRange =
    #
    # varName not currently used.
    #
function(ins, varName)
{
   
     # This sends inferLength(p$population) for bamp in bamp/src/bamp.ir into infinite loop.
    if(is(ins, "CastInst"))
        ins = ins[[1]]
    
    if(is(ins, "GetElementPtrInst"))
        ins = ins[[2]]
   
    if(is(ins, "PHINode"))
        return(structure(lapply(ins[], findIndexRange, varName), names = c("start", "end")))
    
    if(is(ins, "Constant"))
        return( getValue(ins) )

    if(is(ins, "BinaryOperator")) {
        # assuming i + 1
        return(findLoopRange(ins))
    }

    browser()
    NA
}

findLoopRange =
function(ins)
{
    t = getTerminator(getParent(ins))
    # t is a BranchInst
    cond = t[[1]]
    # cond is an ICmpInst and identical(t[[1]][[1]], ins)
    w = !sapply(cond[], identical, ins)
    o = cond[[ which(w) ]]

    o = NativeCodeAnalysis:::unravel(o)

#   if(is(o, "LoadInst"))
#      o = o[[1]]
#
#   if(is(o, "CastInst"))
#       o = o[[1]]

    if(is(o, "CallInst")) {
        fn = getCallName(o)
        if(fn %in% c("Rf_length")) {
            arg = o[[1]]
            #if(is(arg, "Argument"))
            return(LengthOf(arg))
        }
    }

    if(is(o, "Argument"))
        return(structure(list(variable = getName(o)), class = "LengthFromArgument"))
    
    browser()
}

LengthOf =
function(ins)            
   structure(list(expr = ins), class = "LengthOf")
        
