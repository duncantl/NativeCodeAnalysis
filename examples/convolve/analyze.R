# 2019-10-01
#
# The goal of this example is to extract the argument types from the signature
# for a routine called with `.C()`. Further on in the example, we will also try
# to find heuristics to decide whether each argument is an input/output/both
# and to determine the dimensions of each argument.
#
# We can generate unlinked, unoptimized LLVM IR from the C source file with the
# command:
#
#     clang -S -emit-llvm convolve.c
#
# Since Clang v7.0.0 we can add the `-fno-discard-value-names` flag to keep the
# variable names in the generated IR:
#
#     clang -fno-discard-value-names -S -emit-llvm convolve.c
#
# DTL: Nice - thanks for that.
#
# It is also possible to keep variable names in older versions of Clang by
# using the `-###` flag to get the raw compiler command and then removing the
# `-discard-value-names` flag.

library(Rllvm)

# Load the LLVM IR into R.
m = parseIR("convolve.ll")

# List the names of the routines in the module.
names(m)

# Get the convolve routine.
con = m$convolve

getType(con)

# Get the basic blocks in the routine.
getBlocks(con)

# Get the parameters on the routine.
#
# Can also access parameters directly on the Function object with `[[`.
#
# This comes back as a list of Argument objects.
params = getParameters(con)

# So now we need to get the type of of the Argument.
#
# But getType() just tells us that the param is a pointer.
# And does it do this even if the param type is not a pointer?
# DTL:  Why would it tell you it was a pointer if it wasn't?
#  f = Function("bob", VoidType, list(Int32Type), m)
#  getType(getParameters(f)[[1]])  # Returns IntegerTyId
ty = getType(params[[1]])

# Get the element type of the pointer.
elt_ty = getElementType(ty)

# QQ: Does Rllvm provide a function to check equality of two LLVM Types?
#  DTL:  Yes, sameType.
#
# According to the LLVM docs, Types are immutable and so can be compared by
# comparing addresses. But identical() returns FALSE when these two types are
# the same:
identical(elt_ty, DoubleType)
# DTL: sameType(elt_ty, DoubleType)  returns TRUE.

# So now we can get the element type for each parameter.
get_elt_type =
function(parameter)
{
  type = getType(parameter)

  if (isPointerType(type))
    getElementType(type)
  else
    type
}

# Get the types for all of the parameters.
#
# QQ: It would be convenient if Rllvm printed the element type for pointer
# types.
#
# We can translate these types into typesys types if we want to use them in
# type inference.
lapply(params, get_elt_type)

# Now that we have a way to get the argument names and types, what rules can we
# use to determine whether or not an argument is an input/output type?
#
# The basic idea for input types is to check whether the argument is ever read
# somehow, and for output types to check whether the argument is ever written
# somehow. Aliases may complicate this (but maybe LLVM will help to deal with
# aliases).

# <DTL>  If you you compile with any level of optimization, e.g. -O1 or O2 or O3,
#  the resulting .ll file will have additional attributes on the parameters, including
#  readonly.
#  So
#   sapply(params, onlyReadsMemory)
#  will return
#     a    na     b    nb    ab 
#  TRUE  TRUE  TRUE  TRUE FALSE 
# So a, b, na, nb are readonly and  ab is not and that is where the results are returned.
#
# It is also possible for us to create the ll file unoptimized and then to Optimize() a clone of it
# via Rllvm after having read it so that we can have both versions.
# The Optimize() function may not do that quite correctly now, but if this is needed, I can fix this.
# </DTL>

#
#
# Let's start with checking whether each argument is read.
blocks = getBlocks(con)

# For reference, we know that:
#
# a  | input
# na | input
# b  | input
# nb | input
# ab | output

# Relevant: getAllUsers(), getAllUses()
#
# Get users of parameter `ab`.
users = getAllUsers(con[["ab"]])

# The parameters are all stored at %NN.addr addresses.
#
# The store operations might get optimized out at higher levels of
# optimization.
#
# The chain in the LLVM IR is:
#
#     %ab.addr = alloca double*, align 8
#     %21 = load double*, double** %ab.addr, align 8
#     %arrayidx14 = getelementptr inbounds double, double* %21, i64 %idxprom13
#     store double %add15, double* %arrayidx14, align 8
#
# So we need to watch out for getelementptr commands that affect the elements
# of an array.
#
# QQ: Is there any routine in LLVM that can automatically return the whole
# use-def chain (including through stores/loads), or do we have to construct it
# manually?
#
# DTL: Is this question still relevant given the onlyReadsMemory solution above.
#
#
# One way to do it manually:
#   * Extract address from store.
#   * Find uses of address and check whether they are stores.
#   * If uses of address are getelementptr, repeat for element address.
#
# The drawback of this approach is that it completely ignores control flow. But
# it's not clear how much control flow matters for this problem.
#
# Let's try it.

use = users[[1]]
addr = getOperands(use)[[2]]
users = getAllUsers(addr)

# Essentially need to follow chain of users and see if there are any stores in
# the chain. We can do this recursively (although that may be inefficient).

# NOT WORKING:
check_for_store =
function(inst)
{
  users = getAllUsers(inst)
  if (is(users[[1]], "StoreInst")) {
    ops = getOperands(users[[1]])
    # Check whether inst is the value (1st op) or address (2nd op).
    if (getName(ops[[1]]) == getName(inst)) {
      # Get the address; check for stores at the address.
      check_for_store(ops[[2]])
    } else {
      return (TRUE)
    }
  }
}

# The recursive function needs to dispatch on the class of the instruction.
#
# TODO: Fix it tomorrow.


# References:
#   * Rllvm/AnalyzeCCode/eg.R
