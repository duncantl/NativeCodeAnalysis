inferParamTypes =
    #
    #
    #
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
    # TODO:  Integrate the testType() and findSEXPTypesFromSwitch() to determine 
    # the types of the parameters with these additional guides.
    #
    # Check for use in isFunction(), etc. and see if within a condition that branches to
    #  an error.  See testType.R.  e.g. testParamType(p) returns a list of function names
    #  which identify the test predicate
    #
    # Break function into smaller units
    #
    #
    #
function(p, users = getAllUsers(p)) # , direct = FALSE))
{
    k = sapply(users, getCallName)
    k = k[!is.na(k)]  # will get NA from getCallName for a ReturnInst, i.e., when returning the parameter.
    ku = unique(k)
    w1 = ku %in% c("VECTOR_ELT", "SET_VECTOR_ELT")

    # Can match more than one, e.g., third parameter in influence() which is a REAL and also inserted into a list as an element via SET_VECTOR_ELT()    
    if(any(w1)) {
        # See in what capacity
    }
    
    w = ku %in% c("REAL", "INTEGER", "LOGICAL", "STRING_ELT", "SET_STRING_ELT")
        
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

    if(length(ku) && all(ku %in% c("EXTPTR_PTR", "R_ExternalPtrAddr")))
        return(structure(list(type = "ExternalPtr"), class = "RExternalPtrType"))


    if(any(ku %in% "Rf_coerceVector")) {
        w = k == "Rf_coerceVector"
        ty = sapply(users[w], function(x) getValue(x[[2]]))
        i = match(ty, RSEXPTypeValues.df$typeNum)
        names(ty) = RSEXPTypeValues.df$rtypeName[i]
        return(structure(list(type = ty, length = NA), class = c("RVectorType")))  # Capture coercion.
                    # capture that the use of this Value has an implicit length.
    }
    
    RAsRoutines = c("Rf_asChar" = 16L, "Rf_asReal" = 14L, "Rf_asInteger" = 13L, "Rf_asLogical" = 10L)
                    
    if(any(w <- (ku %in% names(RAsRoutines)))) {
        if(!all(w))
            warning("not all routine call names in Rf_as*")
        kalls = intersect(names(RAsRoutines), ku)
        ty = RAsRoutines[kalls]
        return(structure(list(type = ty, length = 1), class = c("RScalarType", "RVectorType")))
    }

     # Do we ever access this as an external ptr.
    ext =  c("EXTPTR_PTR", "R_ExternalPtrAddr") %in% ku    

    typeof = getTypeOf(p, users, k)
    if(any(ext) && all(typeof == "externalptr"))
        return(structure(list(type = "externalptr"), class = "RExternalPtr"))



    isEnv = isUsedAsEnv(p) # do we pass  users, k)
    if(all(isEnv) && (length(typeof) == 0 || "environment" %in% typeof))
        return(structure(list(type = "environment"), class = "REnvironment"))

    
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

        if(length(u2) == 0) {
            # so it appears it is never used.
            # So we think that it may be stored somewhere which is passed to another routine
            # and this R object is used there.
            w = sapply(users, function(u) is(u, "StoreInst") && identical(p, u[[1]]))
            if(any(w)) 
               return(unlist(lapply(users[w], function(u) findUseOfInOtherRoutines( u[[2]] )), recursive = FALSE))
        }

        
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


    tst = testParamType(p)
    if(!is.null(tst)) {
        # Need to check if raises an error if not one of these types.
        # Need to package the information as types not names of routines.
        return(tst)
    }

    if(length(typeof))
        return(structure(list(type = typeof)))
    
    # 
    # We want ANY rather than NULL when we know it can be anything.

    if(length(ans) == 0)
       ans = structure(list("ANY"), class = "ANY")

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





getTypeOf =
    # Find the uses of TYPEOF(param)  and see what these values are compared to determine
    # what types are expected.
    #
    #  want to find both positive comparisons and negative comparisons that lead to an error
    #  e.g.
    #   if(TYPEOF(param) != EXTPTRSXP)  error()    (see Cairo::raw_to_ptr, nseval::_dots_to_env)
    # and
    #    switch(TYPEOF(param)) { case INTSXP  ;  case REALSXP ; .....}
    #
    #
function(p, users = getAllUsers(p), #direct = TRUE),
             k = sapply(users, getCallName))
{
    if(!all(is.na(k)) && "TYPEOF" %in% k) {
           # Was getAllUsers(users[ k == "TYPEOF" ] ) but that doesn't make sense as it is a list, not a single Value.
        tyu = unlist(lapply(users[ !is.na(k) & k == "TYPEOF" ], getAllUsers)) # , direct = TRUE)) 
               # Looks flaky!
        w = lapply(tyu, function(x)
                            if(is(x, "ICmpInst")) {
                                 sapply(x[], function(x)
                                         if(is(x, "ConstantInt")) {
                                             getValue(x)
                                         } else
                                            -1L
                                        )
                             } else
                                -1L)


        ans = unlist(w)
        getRType(ans[ans > -1])
    } else
        integer()
}



isUsedAsEnv =
    # Should this be isUsedOnlyAsEnv
    # Need the direct = TRUE.
function(p, users = getAllUsers(p, direct = TRUE),
         k = sapply(users, getCallName))
{
    # R routines that have a parameter that is an environment.  Name -> parameter number
    envirParamMap = c("Rf_defineVar" = 3L, Rf_findVar = 2L, Rf_findVarInFrame3 = 1L, findVarLocInFrame = 1L,
        findVarInFrame = 1L,
        "Rf_eval" = 2L, R_tryEval = 2L, R_tryEvalSilent = 2L, R_ParseEvalString = 2L, R_forceAndCall = 3L)
    
    m = match(k, names(envirParamMap))

    any(mapply( function(call, idx) {
                     v = call[[idx]]
                     if(is(v, "LoadInst"))
                         v = v[[1]]
                     identical(v, p)
                 }, users[!is.na(m)], envirParamMap[m[!is.na(m)]]))
}


