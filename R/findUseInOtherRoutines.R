#
# The idea here is motivated by code in the mco package.
# Specifically, in the do_nsga2 routine, three parameters - an environment (s_env) and 2 functions
# and stored in a struct and the struct is passed to other routines (by reference)
# One of these routines accesses the fields in the struct. to which these R objects were assignd
# and uses them in R API calls.
# The environment is the one object which has no test on the type in the do_nsga2 routine and is not
# used there at all directly other than in the store command into the structure.
# The only way we can determine its class/type is by seeing how it is used in these other routines.
# This is only evaluate_pop.
# It is possible in other cases that the routine to which it is passed might be in another module in the package
# (or elsewhere but that should be either another package or the R engine via its API)
#
#
#
#
#

if(FALSE) {
    m = parseIR("~/CRAN2/Pkgs3/mco/src/nsga2.ir")
    f = m$do_nsga2
    params = getParameters(f)
    u = getAllUsers(params$s_env)
    findUseOfInOtherRoutines(u[[1]][[2]])

    u = getAllUsers(params$s_function)
    fnp = getAllUsers(u[[2]])[[1]][[2]]
    findUseOfInOtherRoutines(fnp, infer = FALSE)

    u = getAllUsers(params$s_constraint)
    fnp = getAllUsers(u[[2]])[[1]][[2]]
    findUseOfInOtherRoutines(fnp, infer = FALSE)    
}


findUseOfInOtherRoutines =
    #
    # This takes the GetElementPtrInst that is the reference to the struct and the operands to get the field.
    # So this would be
    #   u = getAllUsers(params$s_env)[[1]][[2]]
    #
    #
    #
    #
    #
    #
    #
function(val, ...)
{
    #   if(is(val, "Argument")) {
    # See inferParamType when there appears to be no uses of the parameter.
    #      u = getAllUsers(val)
    #  }
    
    struct = val[[1]]
    indices = sapply(getOperands(val)[-1], as, "integer")
    calls = findCallsPassingAsArg(struct)
    # Do we need to examine each call or just the unique routines.
    # It is possible that struct could be passed in different arguments and used differently,
    # e.g.  foo(struct *x, struct *y) and foo(ctx, NULL) and foo(NULL, ctx)  (where ctx is the instance of struct)
    ans = lapply(calls, findUseInRoutine, struct, indices, ...)
    ans[sapply(ans, length) > 0]
}




findUseInRoutine =
    #
    #  arg is the address of the struct
    #  indices are the operands to the field in arg
    #
function(call, arg, indices, fun = getCalledFunction(call), ...)
{
    argNum = which(sapply(getOperands(call), identical, arg))
    fun = getCalledFunction(call)    
    param = getParameters(fun)[[argNum]]
    findUseOfField(param, indices, ...)
}

findUseOfField =
function(val, indices, users = getAllUsers(val), infer = TRUE)
{
    w = sapply(users, function(x) is(x, "GetElementPtrInst") && identical(x[[1]], val) && length(ops <- getOperands(x)) == length(indices) + 1L && all(sapply(ops[-1], as, "integer") == indices))
    if(!any(w))
        return(list())

    if(infer)
        lapply(users[w], inferParamType)
    else
        getAllUsers(users[w], direct = TRUE)
}


findCallsPassingAsArg =
    #
    # for now, only matches val but could be passing by value or reference.
    # clang should switch the by value to by reference.
    #
function(val, users = getAllUsers(val))
{
    if(!length(users))
        return(list())

    passesArg = sapply(users, function(k) is(k, "CallInst") && any(sapply(getOperands(k), identical, ctx)))
    users[passesArg]
}


