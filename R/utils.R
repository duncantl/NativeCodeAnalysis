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
