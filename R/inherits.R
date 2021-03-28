# Find the calls to Rf_inherits and get the class being checked if it is a literal value.


getInheritsClass =
function(x, ...)
  lapply(getCallsTo(x, "Rf_inherits"), function(x) computeInheritsClass(x[[2]]))

         

computeInheritsClass =
function(ins)
{
    if(is(ins, "GetElementPtrInst")) {

        ans = getGEPValue(ins)
        if(is.null(ans))
            return(NA)
        
        ans
    } else if(is(ins, "ConstantExpr")) {
        if(.Call("R_ConstantExpr_isGEPWithNoNotionalOverIndexing", ins)) {
            getGEPValue(ins)
        } else
            getValue(ins)
    }
    else if(is(ins, "SelectInst"))
        sapply(ins[2:3], getInheritsClass)
    else if(is(ins, "LoadInst"))
        getInheritsClass(ins[[1]])
    else if(is(ins, "CallInst"))
         "<from Call>"
    else if(is(ins, "Argument"))
         "<Argument>"
    else
        NA
}


getGEPValue =
function(ins)
{
    x = ins[[1]]
    if(is(x, "GlobalVariable")) {
        gvarType = getElementType(getType(x))
        if(is(gvarType, "ArrayType")) {
            ans = getValue(getInitializer(x))
#            browser()
                # First value is x, second is 0 to access the global variable. Then 
            idx = sapply(ins[-(1:2)], function(x) if(is(x, "ConstantInt")) getValue(x) else NA)
            w = !is.na(idx)
            if(any(w))
                ans = ans[idx[w] + 1L]
            return(ans)
        }
    }

    NULL
}
