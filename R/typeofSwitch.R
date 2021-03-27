findSEXPTypesFromSwitch =
    #
    #  given a parameter (class Argument) p, find where it is used and if any of these are TYPEOF() calls.
    #  For these TYPEOF() calls, find any switch statements that use this value and see what cases they handle.
    #
    #
    #  Connect to testType() in testType.R
    #
function(p)
{
    u = getAllUsers(p)

    isTypeof = sapply(u, function(x) is(x, "CallBase") && is(cf <- getCalledFunction(x), "Function") && getName(cf) == "TYPEOF")
    if(!any(isTypeof))
        return(integer())

    u2 = unlist(lapply(u[isTypeof], getAllUsers))
    
    w = sapply(u2, is, "SwitchInst")
    if(!any(w))
        return(integer())


    ans = lapply(u2[w], getSwitchValues)
    # Check to see if the default branch for each switch leads to an error block
    # If not, then implicitly handles other types of R objects but in the same way.
    #    sapply(u2[w], function(x) leadsToError(x[[2]]))
    # 
    ans
}

getSwitchValues =
function(sw)
{
    sapply(sw[seq(3, length(sw) - 1L, by = 2) ], getValue)
}
