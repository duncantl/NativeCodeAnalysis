
getReturnValues =
  # This returns the LLVM Value objects     
function(fun, blocks = getBlocks(fun))
{
    terms = lapply(blocks, getTerminator, FALSE)
    isRet = sapply(terms, is, 'ReturnInst')
    rets = terms[isRet]
    lapply(rets, `[[`, 1L)
}

