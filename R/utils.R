getCallName =
function(ins)
{
    if(!is(ins, "CallInst"))
        return(NA)

    getName(getCalledFunction(ins))
}


unravel =
function(x)
{
    if(is.list(x))
        return(lapply(x, unravel))
    
    if(is(x, "CastInst"))
        x = x[[1]]

    if( is(x, "LoadInst"))
        x = x[[1]]
    
    x
}


isSEXPType =
    #
    # Given a type, determine if it is the general SEXP type.
    #
function(ty)
{
    if(! is(ty, "PointerType") )
        return(FALSE)

    pty = getElementType(ty)
    if( (!is(pty, "StructType")) || getName(pty) != "struct.SEXPREC")
        return(FALSE)

    TRUE
}


getAllUsers =
function(x, direct = FALSE)
{
    if(is.list(x))
        return(unlist(lapply(x, getAllUsers, direct = direct)))

    ans = Rllvm::getAllUsers(x)
    if(direct) {

       ans = unlist( lapply(ans, function(x) if(is(x, "LoadInst")) getAllUsers(x, direct = TRUE) else x) )
    }

    ans
}
