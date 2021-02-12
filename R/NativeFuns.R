# Find the calls to native routines in a package.

library(rstatic)

getCCalls =
    #
    # Can probably use code in the tools package to do this.
    #
function(dir, withFunName = TRUE)
{
    rfiles = list.files(file.path(dir, "R"), full.names = TRUE, pattern = "\\.[RrSsQq]$")
    ans = lapply(rfiles, getCCallsInFile, withFunName = withFunName)
    do.call(rbind, ans)
#    names(ans) = rfiles
#    ans[sapply(ans, length) > 0]
}

removeIfFalse =
    # remove any if(FALSE)    
function(ast, in_place = TRUE)   
{
     # Just top-level if(FALSE) and not dealing with the else
    #    w = sapply(ast$contents, function(x) is(x, "If") && is(x$condition, "Logical") && !x$condition$value)
    #    ast$contents = ast$contents[!w]
    
    ast = replace_nodes(ast, function(x)
                  if(is(x, "If") && is(x$condition, "Logical") && !x$condition$value)
                      if(!is.null(x$false)) x$false else NULL
                  else
                    x
        , in_place = in_place)
}
getCCallsInFile =
function(file, ast = to_ast(parse(file)), withFunName = FALSE)
{
    ast = removeIfFalse(ast)
  
    nodes = find_nodes(ast, isNativeCall)

    #nodes = unlist(nodes, recursive = FALSE)
    mapNativeCallNodes(nodes, file, withFunName = withFunName)
}


mapNativeCallNodes =
function(nodes, file, withFunName = FALSE)
{
    routines = sapply(nodes, getRoutine)
    ans = data.frame(callType = sapply(nodes, rgetCallType),
                     routine = unlist(routines),
                     routineType = names(routines),
                     package = sapply(nodes, getPackage),
                    file = rep(file, length(nodes)),
        stringsAsFactors = FALSE)

    if(withFunName)
        ans$rfunction = sapply(nodes, getFunctionName)
    
    ans
}

getFunctionName =
function(node)
{
    fun = getFunctionAncestor(node)
    
}

getFunctionAncestor =
function(node)    
{
    tmp = node

    while(!is.null(node)) {
        if(is(node, "Function"))
            break
        node = node$parent
    }

    if(is.null(node))
      return(NA)

    def = node$parent
    if(is(def, "Assignment")) {
        if(is(def$write, "Symbol") || is(def$write, "Character"))
            def$write$value
        else
            stop("jane")
    } else if(is(def, "ArgumentList") && is(def$parent, "Call")) {
        def = def$parent
        if(is(def$fn, "Symbol"))
            return(def$fn$value)
        else if(TRUE || is(def$fn, "Namespace"))
            return(deparse(as_language(def$fn)))
        else
            stop("Call but to what")
    } else if(is(def, "Brace") || is(def, "ArgumentList"))
        "<anonymous>"
    else if(is(def, "Return"))
        "<nested>"
    else
        stop("bob")
}

isNativeCall =
function(node)
{
    is(node, "Call") && is(node$fn, "Symbol") && node$fn$value %in% c(".External", ".External2", ".Call", ".C", ".Fortran")
}


rgetCallType =
function(call)    
{
   call$fn$value
}

getRoutine =
function(call)
{
    r = call$args$contents[[1]]
    if(is(r, "Character") || is(r, "Symbol"))
        structure(r$value, names = class(r)[1])
    else if(is(r, "Call")) {
        structure(paste(deparse(as_language(r)), collapse = ""), names = "Call")
    } else if(is(r, "If"))
        c(If = NA) # paste(getRoutine(r$true), getRoutine(r$false))
    else        
        stop("fix")
}

getPackage =
function(call, pkg = NA)
{
    p = names(call$args)
    i = match("PACKAGE", p)
    if(is.na(i))
        pkg
    else {
        p = call$args$contents[[i]]
        if(is(p, "Character"))
            p$value
        else if(is(p, "Call") && length(p$args) == 1)
            p$args$contents[[1]]$value
        else
            NA
    } 
} 
