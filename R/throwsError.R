throwsError =
    #
    # Doesn't handle calls to errorFun via function pointers
    # Assumes dead code elimination so that if there are any calls, they are reachable.
    #
function(fun, errorFun = "Rf_error")
{
    ins = getInstructions(fun)
    calls = ins[sapply(ins, is, "CallBase")]
    calls = sapply(calls, function(i) getName(getCalledFunction(i)))
    errorFun %in% calls
}
