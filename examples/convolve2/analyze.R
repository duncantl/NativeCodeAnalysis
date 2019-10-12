# 2019-10-09
#
# Let's start by trying to infer the return type for the convolve2 function.
#
library(Rllvm)

mod = parseIR("convolve2.ll")
con = mod$convolve2

# We can take the same approach as in the return42 example: work backwards from
# the return value(s) of the routine.
#
# So first let's get the return values.

# FIXME:
# library(NativeCodeAnalysis)
source("../../R/find.R")

rets = findTerminators(con, is, "ReturnInst")

# Now work backward to find the assignment that actually sets up the SEXP.
#
# For working backward:
#   ret             => get operand 1
#   call Rf_protect => get operand 1
insts = lapply(rets, getOperand, 1)

inst = getOperand(insts[[1]], 1)

findSEXPDef =
function(x)
{
  UseMethod("findSEXPDef")
}

findSEXPDef.CallInst =
function(x)
{
  name = getName(getCalledFunction(x))
  if (name == "Rf_protect")
    findSEXPDef(getOperand(x, 1))
  else
    x
}


findSEXPDef.ReturnInst =
function(x)
{
  findSEXPDef(getOperand(x, 1))
}

defs = lapply(rets, findSEXPDef)

# Now we've got the call that created the SEXP, so let's try to get information
# from the call.

guessType =
function(x)
{
  name = getName(getCalledFunction(x))
  switch(name,
    "Rf_allocVector" = {
      sexptype = getOperand(x, 1)
      sexptype = if (is(sexptype, "ConstantInt"))
        # TODO: Lookup in SEXPTYPE enum.
        sexptype
      else
        # TODO: Try checking for aliases.
        NA

      len = getOperand(x, 2)
      len = if (is(len, "ConstantInt"))
        len
      else
        # TODO: Try checking for aliases and arithmetic.
        # Ultimately we need to try to link this symbolically to the
        # parameters. Here we end up reconstructing the high-level operations
        # in the C code, so it may end up being better to do this with RCIndex.
        NA
      browser()
    },
    NULL
  )
}

# Let's try to work out a strategy to get the length of the return value
# symbolically. In this example the length is:
#
#   length(a) + length(b) - 1
#
# We can work backwards from the call to Rf_allocVector to try to determine the
# length.

len = getOperand(defs[[1]], 2)

# The previous instruction is `sext`, which extends the bitsize of a type. We
# can safely ignore `sext` instructions if we just want to know the symbolic
# value.
#
val = getOperand(len, 1)

# Now we have an `add` instruction We need to continue working backward
# symbolically for the operands.
add_ops = getOperands(val)

# The first operand is defined by another `add` instruction.
#
# The second is defined by a call to Rf_length. So we know the second operand
# is the length of some SEXP. We still need to work backward to find out which
# SEXP.

findSEXPDef(getOperand(add_ops[[2]], 1))

# What should `findSEXPDef` do with a `coerceVector` call? The `coerceVector`
# routine coerces a SEXP to a specific SEXPTYPE.
#
# For SEXPTYPE inference, we want `findSEXPDef` to stop at the first
# instruction that identifies a SEXPTYPE. So here we would want to stop at the
# call to `coerceVector`.
#
# On the other hand, for dimension inference, we want `findSEXPDef` to stop at
# the first instruction that identifies a dimension. Since `coerceVector`
# doesn't affect the dimensions, we need to continue backtracking.
#
# So maybe we need a separate function for each case, or we need to generate
# the whole chain, and then inspect the chain for types and dimensions.


# If present, we can use calls to `coerceVector` to determine the expected
# SEXPTYPEs of the arguments.
