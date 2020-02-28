# Infer the types of arguments in a .External() call.

inferExternalParamTypes =
function(irfun, params = getParameters(irfun))
{
    p = params[[1]]
    
    u = getAllCARUses(p)
    
    lapply(u, inferExternalParamType)
}

inferExternalParamType =
function(u)
{
    a = inferParamType(u)
    if(!is.null(a))
        return(a)
    # keep going from here.
}


getAllCARUses =
function(p)
{
    uses = list()
    
    u = rev(getAllUsers(p))

    while(length(u) > 0) {
        k = sapply(u, function(i) if(is(i, "CallInst")) getCallName(i) else NA)
        w = k == "CAR"
        uses = append(uses, u[w])
        u = unlist(lapply(u[!w], getAllUsers))
        
    }
    uses
}
