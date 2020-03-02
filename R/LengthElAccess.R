inferLength =
function(p)
{
   u = getAllUsers(p)
   w = sapply(u, isElementAccessor)
   if(!any(w))
       return(NA)

   ans = lapply(u[w], findElAccessLength)
   names(ans) = sapply(u[w], as, "character")
   
   if(sum(w) == 1)
       return(ans[[1]])
   
   ans
}


isElementAccessor =
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

    var = getName(idx)
    if(is(idx, "PHINode"))
        structure(lapply(idx[], findIndexRange, var), names = c("start", "end")) # order may not be always correct. Just putting names here to make it easier to understand what we are looking at in the output.
}


findIndexRange =
function(ins, varName)
{
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

    if(is(o, "CastInst"))
        o = o[[1]]

    if(is(o, "CallInst")) {
        fn = getCallName(o)
        if(fn %in% c("Rf_length")) {
            arg = o[[1]]
            #if(is(arg, "Argument"))
            return(LengthOf(arg))
        }
    }
    browser()
}

LengthOf =
function(ins)            
   structure(list(expr = ins), class = "LengthOf")
        
