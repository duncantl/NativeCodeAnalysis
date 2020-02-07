findTerminators =
function(x, test, ...)
   # Find all ops that satisfy a condition.
{
  if (is(x, "Function"))
    x = getBlocks(x)

  found = lapply(x, function(block) {
    terminator = getTerminator(block)
    if (test(terminator, ...))
      terminator
    else
      NULL
  })
  unlist(found, recursive = FALSE, use.names = FALSE)
}


findInstructions =
function(x, test, ...)
# Find all ops that satisfy a condition.
{
  if (is(x, "Function"))
    x = getBlocks(x)

  found = lapply(x, function(block) {
    # QQ: Might be nice if getBlockInstructions was an as.list method so that
    # getBlockInstructions isn't necessary here.
    instrs = getBlockInstructions(block)
    is_match = vapply(instrs, test, NA, ...)
    instrs[is_match]
  })
  unlist(found, recursive = FALSE, use.names = FALSE)
}
