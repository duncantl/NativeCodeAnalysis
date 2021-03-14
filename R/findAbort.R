
findFunUses =
    #
    # Given a Function object, or a name and module,
    # find the 
    #
    #
    #
function(fun, mod, recursive = TRUE, .done = list())
{
    fname = deparse(substitute(fun))
    if(is.character(fun))
        fun = mod[[fun]]

    if(is.null(fun))
        stop("Not a function")
    
    if(length(.done) && any(sapply(.done, identical, fun)))
        return(NULL)


    
    u = getAllUsers(fun)

    if(length(u) == 0)
        return(list())
    
    ans = lapply(u, as, "Function")
    if(recursive) {
    #    .done = c(ans, .done)
        for(f in ans) {
            tmp = findFunUses(f, mod, .done = .done)
            ans = c(ans, tmp)
            .done = c(.done, tmp)
        }
    }

    unique(ans)
}
