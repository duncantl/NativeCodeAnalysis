inferParamTypes =
function(fun, params = getParameters(fun))
{
   lapply(params, inferParamType)
}

inferParamType =
    #
    # Todo 
    #   When we discover this is a vector (character, numeric, etc.), determine if it is 
    #   only used as a scalar. E.g. STRING_ELT(p, 0) or STRING_ELT(p, i).
    #   The C code could use asChar, asReal, etc. but may not.
    #
    #
function(p)
{
    users = getAllUsers(p)
    k = sapply(users, getCallName)
    ku = unique(k)
    w = ku %in% c("REAL", "INTEGER", "LOGICAL", "STRING_ELT", "SET_STRING_ELT", "VECTOR_ELT", "SET_VECTOR_ELT")
    ans = if(any(w)) {
        i = match(tolower(ku[w]), RSEXPTypeValues.df$rtypeName)
        if(!is.na(i))
            return(getRType(RSEXPTypeValues.df[i, 1]))
        list(type = switch(ku[w],
               REAL = getRType(14),
               "STRING_ELT"=,
               "SET_STRING_ELT" = getRType(16),
               "VECTOR_ELT" = ,
               "SET_VECTOR_ELT" = getRType(2)
                 ),
             length = NA) #XXX FIX!
    }

    if(!is.null(ans))
        return(ans)
    

    if("R_ExternalPtrAddr" %in% ku)
        return(structure(list(type = "ExternalPtr"), class = "RExternalPtrType"))

    
    if(!is.na(k) && "TYPEOF" %in% k) {
            tyu = getAllUsers(users[ k == "TYPEOF" ])
            w = sapply(tyu, function(x) if(is(x, "ICmpInst")) lapply(x[], function(x) if(is(x, "ConstantInt")) getValue(x)))
            ans = tyu[w]
        }

    RAsRoutines = c("Rf_asChar" = 16L, "Rf_asReal" = 14L, "Rf_asInteger" = 13L, "Rf_asLogical" = 10L)
    if(any(w <- (ku %in% names(RAsRoutines)))) {
        if(!all(w))
            warning("not all routine call names in Rf_as*")
        kalls = intersect(names(RAsRoutines), ku)
        return(structure(list(type = RAsRoutines[kalls], length = 1), class = c("RScalarType", "RVectorType")))
    }

    if(is.null(ans)) {
         #
         #  Look at where the argument was stored to a local variable
         #  and then subsequently loaded and used.
         #
        users = users[ sapply(users, is, "StoreInst") ]
         # next get which variable each was stored.
        vars = lapply(users, `[[`, 2)
        u2 = unlist(lapply(vars, getAllUsers))
         # avoid any circularity of finding the initial store.
        w = u2 %in% users
        u2 = u2[!w]
         # Now find where those variables were load and then where they were
         # used.
        w = sapply(u2, is, "LoadInst")
        u3 = c(u2[!w], unlist(lapply(u2[w], getAllUsers)))

        # Now, in u2 elements, where are the elements of users actually
        # used and what does this tell us about their types.
        isCall = sapply(u3, is, "CallInst")
        ans = lapply(u3[isCall], function(k) {
                                  m = match(u2, getOperands(k))
                                  if(is.na(m))
                                      return(NULL)
                                  getArgType(m, k)
                              })
        if(length(ans) == 1)
            ans = ans[[1]]
        
    }

    # 
    # We want ANY rather than NULL when we know it can be anything.

    ans
}



KnownRoutines = 
list(Rf_setAttrib = list("SEXP" = "ANY", "SYMSXP" = "symbol", "SEXP" = "ANY"))

getArgType =
function(argNum, call)
{
    fun = getCallName(call)
    if(fun %in% names(KnownRoutines)) {
      list(type = KnownRoutines[[fun]][[argNum]], length = NA)
    } else {
        stop("Need to find ", fun, " and determine the types of its parameters")
    }
    
}
