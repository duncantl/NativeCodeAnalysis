if(FALSE) {
  native = getNativeCalls("~/GitWorkingArea/Rllvm/R")
  native = native[sapply(native, length) > 0]
}


getNativeCalls =
function(dir, rfiles = list.files(dir, full.names = TRUE, pattern = "\\.[RrSsq]$"))
{
    ans = lapply(rfiles, getNativeCallsInFile)
    names(ans) = rfiles
    ans
}

getNativeCallsInFile =
function(f, doc = to_ast(parse(f)))
{
    k = find_nodes(doc, isNativeCall)
    sapply(k, getRoutine)
}

isNativeCall =
function(node, dotFuns = c(".C", ".Call", ".External"))
{
   is(node, "Call") && is(node$fn, "Symbol") && node$fn$value %in% dotFuns
}

getRoutine =
function(node)
{
    ans = node$args$contents[[1]]
    if(is(ans, "Character"))
        ans$value
    else if(is(ans, "Symbol")) {
        ans = ans$value
    }
}
