
getReturnValues =
  # This returns the LLVM Value objects     
function(fun, blocks = getBlocks(fun))
{
    if(sameType(getReturnType(fun), VoidType))
        return(NULL)
    
    terms = lapply(blocks, getTerminator, FALSE)
    isRet = sapply(terms, is, 'ReturnInst')
    rets = terms[isRet]
    lapply(rets, `[[`, 1L)
}

