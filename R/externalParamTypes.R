# Infer the types of arguments in a .External() call.

inferExternalParamTypes =
function(irfun, params = getParameters(irfun))
{
    u = rev(getAllUsers(params[[1]]))
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
