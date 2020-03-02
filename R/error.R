isErrorBlock =
    #
    #  Expects a BasicBlock
    #
function(b)
   any(sapply(b[], isRErrorCall))


isRErrorCall =
function(x)
    is(x, "CallInst") && getCallName(x) == "Rf_error"
