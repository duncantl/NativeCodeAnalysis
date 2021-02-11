library(rstatic)

getClassAssigns =
    #
    # from an entire file of code or collection of top-level expressions
    #
function(file, expr = parse(file), ast = to_ast(expr))
{
   find_nodes(ast, isClassAssign)
}

getFunClassAssign =
function(f)
{
   b = to_ast(body(f))
   find_nodes(b, isClassAssign)
}

isClassAssign =
function(node)
{
   is(node, "Replacement") && is_symbol(node$read$fn, "class<-")
}


if(FALSE) {
ff = list.files("~/CRAN2/Pkgs/lava/R", full.names = TRUE)
}
