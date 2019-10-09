# 2019-10-07
#
# The goal of this example is to infer the return type.
#
library(Rllvm)

m = parseIR("return42.ll")
return42 = m$return42

blocks = getBlocks(return42)
lapply(blocks, getTerminator)

findTerminator =
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


isRet =
  # Helper to check whether op is ret.
  #
  # QQ: Is there a better way to do this? Is there a constant/enumeration for
  # opcodes?
function(x) {
  getOpcode(x) == 1 # 1 is ret
}


# Get all of the ret instructions.
rets = findTerminator(return42, isRet)

# The ret instruction returns the %call1 object. So now the goal is to
# determine the SEXPTYPE for %call1. We can do this by checking how %call1 is
# defined.
#
# First let's get the operand.
op = getOperand(rets[[1]], 1)

# This gives us the defintion for the operand. But the definition is a call to
# Rf_protect, which doesn't tell us anything about the SEXPTYPE.
#
# So let's get the defintion for the object passed to Rf_protect.
#
# The arguments come before the routine in a call op.
op = getOperand(op, 1)

# Now we're at a call to Rf_allocVector(). The arguments are constants, so all
# we need to do is look up SEXPTYPE 13.
#
# QQ: Does Rllvm already have the SEXPTYPE enumeration? Or should I add this to
# NativeCodeAnalysis?
