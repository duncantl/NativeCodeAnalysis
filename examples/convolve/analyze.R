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

# ------------------------------------------------------------
# 2019-10-02
#
# Per DTL above, if we compile with some level of optimization:
#
#     clang -fno-discard-value-names -O1 -S -emit-llvm convolve.c
#
# Then the IR is annotated with `readonly` on parameters that are read only.
# The Rllvm function `onlyReadsMemory()` checks for this annotation.
m = parseIR("convolve.ll")

params = getParameters(m$convolve)
is_input_only = lapply(params, onlyReadsMemory)

# TODO: So now we can build a function to get the parameter types and whether
# they are inputs/outputs.
#
# The `onlyReadsMemory()` function does not tell us whether the outputs are
# read, but that information is less important from an R-user perspective.
#
# The next thing we'd like to do is determine the (symbolic) dimensions of the
# parameters.
#
# We can determine whether the parameters are vectors or scalars based on how
# they are accessed. In other words, we'll assume that a parameter is a scalar
# if only its first element is accessed; all other parameters are vectors.
# 
# In LLVM IR, element access is a two step process. The first step is to get
# the address of the element with `getelementptr`. For scalars, this step is
# not necessary because pointers point to the first element. The second step is
# to load the element into a register with `load`.
#
# Clang does not generate a `getelementptr` for a dereference operation `*x`,
# and let's assume it won't for the equivalent `x[0]` either. So
# `getelementptr` will only be generated to read/write vectors.
#
# We can traverse all of the blocks to find all of the `getelementptr`
# operations, and then check whether they affect the parameters. Alternatively,
# we can follow each parameter through the code to see if it is ever subject to
# `getelementptr`.
#
# In order to make this robust, we need to deal with aliases created by
# load/store. In other words, we need the use-def chain.
#
# However, we can still try the strategy without the complete use-def chains to
# get an approximation.

blocks = getBlocks(m$convolve)

findOps =
function(x, test, ...)
# Find all ops that satisfy a condition.
{
  if (is(x, "Function"))
    x = getBlocks(x)

  found = lapply(x, function(block) {
    # QQ: Might be nice if getBlockInstructions was an as.list method so that
    # getBlockInstructions isn't necessary here.
    ops = getBlockInstructions(block)
    is_match = vapply(ops, test, NA, ...)
    ops[is_match]
  })
  unlist(found, recursive = FALSE, use.names = FALSE)
}

geps = findOps(m$convolve, is, "GetElementPtrInst")
vecs = sapply(geps, function(op) {
  getName(getOperand(op, 1))
})

# So now we can conclude that a, b, ab are vectors.
#
# We can also conclude that the remaining parameters
setdiff(params@names, vecs)
# na, nb are scalars, since there are only 3 gep instructions in the code. If
# there were more gep instructions, we would need to check for aliasing to be
# sure.
#
# TODO: We've demonstrated everything necessary to build a function that
# approximates which parameters are vectors and which are scalars.
#
# The next step is to try to recognize that `*na` and `*nb` are the lengths of
# `a` and `b`, respectively. We'd also like to recognize that the length of
# `ab` is `*na + *nb - 1`.
#
# Let's start by looking at how to figure this out manually from the original C
# code. The key is the loops.
#
# The loop `for(int i = 0; i < nab; i++)` iterates from 0 to `nab - 1`. The loop
# variable `i` is used to access elements of `ab`. So we can conclude that `ab`
# has length `nab`. By working backwards through the code, we can see that
# `nab` is equal to `*na + *nb - 1`.
#
# The loop `for(int i = 0; i < *na; i++)` iterates from 0 to `*na - 1`. The
# loop variable `i` is used to access elements of `a`. So `a` has length at
# least `*na`.
#
# The loop `for(int j = 0; j < *nb; j++)` iterates from 0 to `*nb - 1`. The
# loop variable `j` is used to access elements of `b`. So `b` has length at
# least `*nb`.
#
# The loop variables `i` and `j` from the latter two loops are also used in the
# sum `i + j` to access elements of `ab`. Since `i` and `j` are non-negative,
# we know that `ab` must have length at least `*na + *nb - 1` (the `-1` is
# because the first element is 0).
#
# All of this assumes that the loop variables are never reassigned in the
# middle of the loop. Reassignment is possible, but unlikely unless the user is
# a relatively sophisticated C programmer (most R users are not). So it is
# probably safe to assume that the loop variables are not reassigned.
#
# Similarly, this assumes that `na` and `nb` are not reassigned. We can check
# this with the `readonly` attribute discussed above.
#
# With -O1 optimization, the first loop is completely replaced in the code by a
# call to the `llvm.memset.*` intrinsic routine. The `llvm.memset.*` intrinsic
# fills a block of memory with a particular byte value. The third argument to
# the intrinsic is the length (number of bytes) to fill.
#
# Since the number of bytes is needed, the LLVM IR must multiply the length by
# 8. The IR does this with a `shl` instruction:
#
#     %2 = add i32 %1, %0           ; %2 = na + nb
#     %3 = add i32 %2, -2           ; %3 = na + nb - 2
#     %4 = zext i32 %3 to i64       ; zero extend to i64
#     %5 = shl nuw nsw i64 %4, 3    ; %5 = (na + nb - 2) * 2^3
#     %6 = add nuw nsw i64 %5, 8    ; %6 = (na + nb - 2) * 8 + 8
#                                   ;    = (na + nb - 1) * 8
#
#     call void @llvm.memset.p0i8.i64(i8* align 8 %ab55, i8 0, i64 %6, i1 false)
#
# It's not clear why the Clang subtracts 2 only to add back 8 bytes later.
#
# Since `ab` is a double, shouldn't there be more bytes to fill?


# References:
#   * Rllvm/AnalyzeCCode/eg.R
