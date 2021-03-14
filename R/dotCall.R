findDotCallRoutines =
function(m, demangledOnly = TRUE)
{
   r = getDefinedRoutines(m, names = FALSE)
   ans = r[ sapply(r, isDotCall) ]
   if(demangledOnly) {
       w = names(ans) == demangle(names(ans))
       ans = ans[w]
   }

   ans
}

isDotCall =
function(r)
{
    if(!sameType(getReturnType(r), SEXPType))
        return(FALSE)
    
    ptypes = lapply(getParameters(r), getType)
    w = sapply(ptypes, sameType, SEXPType)
    all(w)
}
