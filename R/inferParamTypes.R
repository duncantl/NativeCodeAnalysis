inferParamTypes =
function(fun, params = getParameters(fun))
{
   lapply(params, inferParamType)
}

inferParamType =
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
        switch(ku[w],
               REAL = getRType(14),
               "STRING_ELT"=,
               "SET_STRING_ELT" = getRType(16),
               "VECTOR_ELT" = ,
               "SET_VECTOR_ELT" = getRType(2)
               )
    }

    if(!is.null(ans))
        return(ans)
    

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
        users = users[ sapply(users, is, "StoreInst") ]
        vars = lapply(users, `[[`, 2)
        u2 = unlist(lapply(vars, getAllUsers))
        w = u2 %in% users
        u2 = u2[!w]
        w = sapply(u2, is, "LoadInst")
        u2 = c(u2[!w], unlist(lapply(u2[w], getAllUsers)))
        #!!! Need to finish this off!
    }
        

    ans
}


getCallName =
function(ins)
{
    if(!is(ins, "CallInst"))
        return(NA)

    getName(getCalledFunction(ins))
}
