getCallName =
function(ins)
{
    if(!is(ins, "CallInst"))
        return(NA)

    getName(getCalledFunction(ins))
}

