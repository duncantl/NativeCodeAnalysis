xgetValue = function(x, seen = list()) {
#    print(x)
#    browser()
    if(any(sapply(seen, identical, x)))
        return(NULL)

    if(is(x, "CallInst") || is(x, "GetElementPtrInst") || is(x, "UndefValue") || is(x, "AllocaInst") || is(x, "Argument"))
        return(NA)
    else if(is(x, "BinaryOperator")) 
        return(NA)


    els = NULL
    if(is(x, "SelectInst"))
        els = x[-1]
    else if(is(x, "PHINode"))
        els = x[]
    else if(is(x, "LoadInst") || is(x, "CastInst"))
        els = list(x[[1]])

    if(is.null(els))
        Rllvm::getValue(x)
    else {
        seen = c(seen, x)
        ans = vector("list", length(els))
        for(i in seq(along = els)) {
            e = els[[i]]
            ans[[i]] = xgetValue(e, seen)
            seen = c(seen, e)
        }
        ans
    }
}
