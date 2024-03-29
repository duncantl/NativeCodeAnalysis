getSymbolName =
function(i, seen = list())    
{
    if(any(sapply(seen, identical, i)))
        return(NA)

    seen = c(i, seen)
    
    if(is(i, "CallInst")) {
        # should handle calls to Rf_allocVector()
        # and chase down their uses to see what their contents are
        fn = getName(getCalledFunction(i))
        if(!is.na(fn) && fn == "Rf_allocVector") {
#browser()
            return( getCharVectorEls(i) )
        }
        getSymbolName(i[[1]], seen)  
    } else if(is(i, "LoadInst"))
        getSymbolName(i[[1]], seen)
    else if(is(i, "GlobalVariable"))
        getName(i)
    else if(is(i, "GetElementPtrInst"))
        getSymbolName(i[[1]], seen)
    else if(is(i, "ConstantExpr"))
         getValue(i[[1]])    
    else if(is(i, "PHINode"))
        sapply(i[], getSymbolName, seen = seen)
    else if(is(i, "SelectInst"))
        sapply(i[-1], getSymbolName)
    else if(is(i, "Argument"))
        "<Argument>"
    else
        NA # browser()
}



getSetAttrCalls =
function(m, funName = "Rf_setAttrib", index = if(funName == "Rf_setAttrib") 3L else 2L)
{
    ins = unlist(getInstructions(m), recursive = FALSE)
    w = sapply(ins, function(i) is(i, "CallInst") && is(cf <- getCalledFunction(i), "Function") && getName(cf) == funName)
    if(length(index) && !is.na(index))
        lapply(ins[w], function(x) { x = x[[index]];  if(is(x, "LoadInst")) x[[1]] else x})
    else
        ins[w]
}
