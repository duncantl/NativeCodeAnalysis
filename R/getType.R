# CRAN2/matrixStats/src/all.bc - dotCallReturnTypes infinite loop.
# getAttribValue method for PHINode  - rootSolve  and call_stsparse routine.
#
#
# √ NA for at if(at == "class")  -   rootSolve  call_stsparse, & rphase rph_bgc_hmm
#    Partially solved so now get multiple values, but get warnninng as we have multiple values in at. How to resolve.
#
#    This seems like an -O2 optimization issue. In the Rf_install() in setAttrib(, Rf_install(), )
#     the generated IR has 3 possible values of the argument to Rf_install.  These correspond
#     to three (maybe 2 as two of the values are the same (jan)) separate calls to setAttrib()
#    If we use -O1, this multiplicity doesn't happen. However, we don't seem to get the full answer.
#     missing the attributes - steady, ian, jan.

#
#  infinite loop in
#   √ matrixStats::rowMads
   # XML:::R_findXIncludeStartNodes
   #   mongolite::R_mongo_collection_command_simple 
#

# √ matrixStats::allocVector2 -  (!(inherits(x, "RVector") && !is.na(x$type) && x$type == "VECSXP")) { :   NA for x$type probably
#     $type is character(0).  It should be the type of the third argument.
#

# [ok but need to do more] if (fn == "Rf_coerceVector") { :   missing value where TRUE/FALSE needed   svd::ematmul_unchecked
#           structure(NULL, class = )   warp::get_month_offset
#
# complete resolveFunction.  When we have a function pointer, see if we can resolve that to one or more Function objects.
#
#

#
# @DTL check this case unix::R_eval_fork   lassoshooting::ccd 

# inferParamType  - not all routine call names in Rf_as*   curl::R_curl_fetch_disk
#   This is a warning.
#   in the curl example, the other routines are Rf_isString and Rf_length
#    These are used in tests on the input and throw an error if the condition is not true.

# XXX  getOperand() too large.   CorrBin::ReprodISDM.
#   looks like when computing the types of the elements and specifically the second (1) which is just Q
#   calling a routine UpdateQ  which returns void. But are we passing the SEXP by reference?

# check  inferParamType when returning an argument and "Stopping here"  rbamtools::gap_site_list_get_df ,  Cairo::raw_to_ptr
#    nseval$"_dots_to_env"


# √ infinite loop  - wrassp::getDObj

# √ namespace nsreg? - global variable

# √  NA for id in if(id == "Rf_protect") return(getCallType0(kall[[1]], ........)
#    promisish_to_closxp in nseval
#
#       Getting a ConstantExpr from getCalledFunction().  So then have to use getValue() to get the underlying Function
#       Arises in getName(getCalledFunction()) and getBlocks(getCalledFunction())

# √ Runuran  - error Runuran_tag    -   no method or default for coercing “GlobalVariable” to “Instruction”. Also in rstream  RngStreams_tag, PBSMapping::importGSHHS, RDieHader::dieharder
#        Seems to be fixed when fixing other issues.

###################

# This needs some cleaning up but is a reasonable start.
# The classes are up for tweaking to make more useful in how we use them.
# This is a proof-of-concept and has come a reasonable way to be useful.


# Losing the class on R_makebin in arulesSequences


# see tests/classes.c   tests/byRef.c  tests/Rexample.c
# For tests/classes.c
# Need to
#   + listEls3 doesn't give back much information - just VECSXP 2, no class, el-types and no names on the elements.
#     I think we are just analyzing listEls1 and not taking advantage of the fact that we know what we are passing into it.
#     This is a case where listEls1() is returning what we pass into it and just annotating it.
#     Different from the case where the output is based on the *value* in the inputs, but unrelated to their structure.
# 
#   + √ mkUnknown.  Have it return that there is a class being set and make it NA, or have it be the call to mkClassName - parameter is x.
#       currently create a list with the instruction and the parameters that it uses and put a SymbolicComputation class on it.
#  
#
#
#  + get names on vectors - mk2
#  + class attribute - mk1
#  + get literal values when available, e.g. for class names, names of vector/list.
#
#  Works for -O1 and -O2 level compiled code, but giving back less information on -O0. For listEls2, just VECSXP 2, no class, etc.
#  and for -O0, compReturnType(m$mk2) (and mk1) doesn't get the name of the parameter for the length and has for the class attribute
# [1] "NULL"           "SymbolicLength"
#
#

#  connect arguments in a call to another routine and the dimension we get back from that second routine  - connect that to the argument
#  as it is in terms of the parameter to the second routine, and we need to tie that to the actual parameter in the first/calling routine
#
#  tests/classes.c
#
# √ listEls2 -
# √ listEls -  returns VECSXP length 2 with the RClassLabels
#
# √ mk1 - basically working. merge the class with the elements field.
#
# √ mk2 -  gets the STRSXP for the class type. Now has the literal names of the class labels.  Returns two different types for the same thing, one complete.
#
# √ doS4 - returns the class name of the S4 class that the C code creates. Class is S4Instance.

if(FALSE) {
    library(Rllvm)
    library(NativeCodeAnalysis)
    source("getType.R")
    m = parseIR("influence.ir")
    rty = compReturnType(m$influence)

    class(r)
    names(r)
    r$length
    r$elNames
    sapply(r$els, class)    
}

compReturnType =
    #
    # AFAIR, this is intended for analyzing C routines in R source code or R packages to find the return
    # type of the SEXPs they return. It doesn't make sense to do this for arbitrary routines as we can just call
    # getReturnType().  It is only the case where we have essentially a union.  So this could be generalized for
    # more than just SEXPs, but not relevant now.
    #
    #
    #' @toc data.frame providing a table of contens of defined routines in different files.
    # toc = mkRoutineFileTOC("~/R-4.1/build3/src/main/")
    #
function(fun, unique = TRUE, toc = NULL, blocks = getBlocks(fun))
{
    if(length(blocks) == 0) {
        # The routine has no body so must be defined somewhere else.
        # We look in the table of contents (toc) to see if we can find it)
        fn = getName(fun)
        if(length(toc)) {
            m = match(getName(fun), toc$routine)
            if(!is.na(m)) {
                f2 = parseIR(toc$file[m])[[fn]]
                return(compReturnType(f2, toc = toc, unique = unique))
            }
        }

        if(!grepl("^llvm\\.lifetime\\.", fn))
            warning(fn, " has no BasicBlocks; probably implemented in another module")
        return(NULL)
    }
    
    rets = getReturnInstructions(blocks = blocks)

    ans0 = lapply(rets, getCallType0)

    ans = mapply(compListTypes, ans0, rets, SIMPLIFY = FALSE, MoreArgs = list(toc = toc))

    # collapse the result down.
    # ans = lapply(ans, unlist, recursive = FALSE)# , recursive = FALSE)
    
    ans = if(length(rets) == 1) {
        tmp = ans[[1]]
         # Drop any null values
        # tmp[sapply(tmp, length) > 0]
    } else
        ans


    if(FALSE && unique)
       ans = unique(ans)

    ans
}


getReturnInstructions =
    #
    # for a routine, get the return instruction(s). Should only ever be one, but we won't insist on that yet.
    #
function(fun, blocks = getBlocks(fun))
{
    terms = lapply(blocks, getTerminator, FALSE)
    isRet = sapply(terms, is, 'ReturnInst')

    terms[isRet]
        # Can we ever have multiple returns???
}

compListTypes =
    # Determine the types of the elements in an R list
function(x, ret, ...)
{
    
  if(!(inherits(x, "RVector") && !is.na(x$type) && x$type == "VECSXP")) {

      # Need to process these.
      k = sapply(x, is, "CallInst")
      # XXX Have to handle the case where the actual return entity is passed by reference to a function
      # and is not just the assignment

      x[k] = lapply(x[k], function(x) compReturnType(getCalledFunction(x), ...))

      return(x)
  }

  usrs = getAllUsers(ret[[1]])
  w = sapply(usrs, function(x) is(x, "CallInst") && getName(getCalledFunction(x)) == "SET_VECTOR_ELT")

  els = lapply(usrs[w], function(x) getCallType0(x[[3]]))

  # Should check the order of the elements by looking at x[[2]] of each users[w]

  x$els = els
  x$elNames = getListNames(usrs, ret)
  x
}


getListNames =
function(usrs, ret)
{
    w = sapply(usrs, function(x) is(x, "CallInst") && getName(getCalledFunction(x)) == "Rf_setAttrib" &&
                                        getAttribName(x[[2]]) == "names") 
    if(!any(w))
        return(character())
    
    tmp = sapply(usrs[w], function(x) x[[3]]) # get the 3rd arg for Rf_setAttrib()
    nu = lapply(tmp, getAllUsers) # get the second arg of Rf_allocVector() (??)

    lapply(nu, function(nu) {
                    w = sapply(nu, function(x) is(x, "CallInst") && getName(getCalledFunction(x)) == "SET_STRING_ELT")
                    sapply(nu[w], function(x) findValue0(x[[3]]))
               })
}

getCallType0 =
    #
    # setGeneric() apparently strips code 
    #
    #
function(x, var = NULL, .done = list(), ...) {
               if(any(sapply(.done, identical, x))) {
                  cat("already processed\n")
                  return(NULL)
                }
               .done = unique(c(x, .done))
               
               tmp = getCallType(x, var, .done, ...)
               
               if(length(tmp) == 1)
                   tmp[[1]]
               else
                   tmp
           }


setGeneric("getCallType",
           function(x, var = NULL, .done = list(), ...) 
              standardGeneric("getCallType")  # previously had call to unique()
           )

setMethod("getCallType", "ANY",
          function(x, var = NULL, .done = list(), ...) {
              message("getCallType ANY for ", class(x))
              return(NULL)
          })

setMethod("getCallType", "ReturnInst",
          function(x, var = NULL, .done = list(), ...)
          {
              val = x[[1]]


              # so if returning a parameter, we want to know its type.
              # This should figure out any changes to it.
              if(is(val, "Argument"))
                  return(inferParamType(val))
              
              # If the return value is a call to a function that calls a function
              # that returns one of its SEXP parameters, then we want to get the type
              # of that
              # e.g. if we have return(f(g(x))) and f returns its only parameter having updated it
              #  we want to get the return type of g() and then update that
              # Similalrly, if we have f(g(h(x))) and g updates its parameter then we need the return type of
              # h and update it with the info. from g and then update that with the input from f

              if(is(val, "CallInst")) {
                  cf = getCalledFunction(val)
                  if(!is(cf, "Function")) {
                      # so a function pointer (see exint_do_ expint() in expint/ package.)
                      # We can try to find the set of all possible functions and then examine those.
                      # See expint.
                      # However, if it is an arbitrary routine, we only know it returns a generic SEXP.
                      
                      return( structure(list("ANY"), class = "ANY") )
                  }
                  ra = returnsArg(cf)
                  #XXX use this.
                  ans = getCallType0(val, val, .done = .done, ...)
#browser()                  
                  compListTypes(ans, x)
                  
                  #XXX Need all the arguments, not just the first one.
                  # Need to call compReturnType() here, not getCallType()

              } else
                  getCallType0(val, .done = c(x, .done))
})

setMethod("getCallType", "ConstantExpr",
          function(x, var = NULL, .done = list(), ...) {
              # XXX probably need to do more.
              getCallType0(x[[1]], .done = .done)
          })


#XXX Change this.
# 1. infer the parameter type or indicate that it is the same type as the argument
# 2. see where it is passed by reference and the value set
# 3. see where its structure/characteristics is modified via setAttrib or set length  (not its contents changed)
#     either in this routine or in routines it is passed to.


#
setMethod("getCallType", "Argument",
          function(x, var = NULL, .done = list(), seen = list(), ...) {
              u = getAllUsers(x)
              w = !sapply(u, is, "ReturnInst")
#browser()              
              ans = lapply(u[w], getCallType0, x, seen = c(seen, u[w]), .done = c(.done, x), ...)
              unlist(ans[!sapply(ans, is.null)], recursive = FALSE)
           })

setMethod("getCallType", "PHINode",
          function(x, var = NULL, .done = list(), ...) {
              #XXX implement -    lapply(x[], getCallType)
              unlist(lapply(seq(length = length(x)), function(i) getCallType0(x[[i]], .done = c(x, .done), ...)),  recursive = FALSE)
          })

setMethod("getCallType", "LoadInst",
          function(x, var = NULL, .done = list(), ...) {
              getCallType0(x[[1]], var, c(x, .done), ...)
          })


setMethod("getCallType", "StoreInst",
          function(x, var = NULL, .done = list(), ...) {
              getCallType0(x[[1]], var, c(x, .done), ...)
          })

setMethod("getCallType", "AllocaInst",
          function(x, var = NULL, .done = list(), ...) {
              u = getAllUsers(x)
              classes = sapply(u, class)
              # The loads shouldn't change this so ignore those for now. May need to put them back in e.g., for
              # being used in a function call as a pointer. DispatchGroup in do_Math2
              unlist(lapply(u[!(classes %in% c("LoadInst", "xxx.BitCastInst"))], getCallType0, var = x, .done = c(x, .done), ...), recursive = FALSE)
          })



setMethod("getCallType", "CastInst", # was "BitCastInst",    expanded to CastInst to get SExtInst
function(x, var = NULL, .done = list(), ...)
{
    ans = lapply(getAllUsers(x), getCallType0, var = x, .done = c(x, .done), ...)

    w = sapply(ans, function(x) is(x, "CallInst") && grepl("llvm.lifetime.(start|end)", getName(getCalledFunction(x))))
    if(all(w))
        NULL
    else
        unlist(ans[!w], recursive = FALSE)
})

setMethod("getCallType", "GlobalVariable",
          function(x, var = NULL, .done = list(), ...) {
              id = getName(x)
              if(id == "R_NilValue")
                  return(structure(list("NULL"), class = "RNULL"))

              list(list(name = id), class = "RGlobalVariable")
#              x
          })


isPointerToSEXP =
function(x)
{
    ty = getType(x)
    isPointerType(ty) && sameType(getElementType(ty), SEXPType)
}

tmp <- function(x, var = NULL, .done = list(), ...)
{
    if(!is.null(var) && any(w <- sapply(x[-length(x)], identical, var)) && isPointerToSEXP(x[[which(w)]])) { # could be passed in two different arguments!
        # so the return variable is being passed by reference to a routine.
        return(findTypeByReference(x, var, which(w)))
    }

    
    kall = x
    cf = getCalledFunction(kall)
    if(is(cf, "ConstantExpr"))
        cf = getValue(cf)
    id = getName(cf)

    if(is.na(id))
        return(NULL)  # XXXX
    
    if(id == "Rf_protect")
        return(getCallType0(kall[[1]], .done = c(x, .done)))

    ans = NULL

#if(id == "logicalSubscript") { cat("getCallType for logicalSubscript\n"); browser()    }

    #browser()
    if(id %in% c("Rf_mkString", "Rf_mkString2")) {
        ans = structure(list(type = "STRSXP", length = 1), class = "RScalar")
        if(is(kall[[1]], "Constant")) {
            ans$value = getValue(kall[[1]])
            class(ans) = c("Constant", class(ans))
        }
        
    } else  if(id %in% c("Rf_allocVector", "Rf_allocVector3", "Rf_allocSExp")) {
        ty = kall[[1]]
        len = kall[[2]]

        #??? Change the class to RList if we know the type is VECSXP.
        rv = findValue0(ty)
        if(is.null(rv)) {
            type = compRType(ty)
        } else
            type = mapRType(rv)
        
        ans = structure(list(type = type, length = findValue0(len)), class = "RVector")
    } else if(id == "Rf_allocMatrix") {

        ans = structure(list(type = mapRType(findValue0(kall[[1]])),
                             dims = list(nrow = findValue0(kall[[2]]),
                                         ncol = findValue0(kall[[3]]))),
                           class = "RMatrix")
    } else if(grepl("^Rf_Scalar", id)) {

      ans = structure(list(type = switch(id,
                                     Rf_ScalarLogical = "LGLSXP",
                                     Rf_ScalarInteger = "INTSXP",
                                     Rf_ScalarReal = "REALSXP",
                                     Rf_ScalarString = "STRSXP",                                              
                                     NA)
                      ), class = "RScalar")

      if(is(kall[[1]], "Constant")) {
          ans$value = getValue(kall[[1]])
          class(ans) = c("Constant", class(ans))
      }
      #XXXX we are losing the class later/up in the computations.

    } else if(grepl("^Rf_as", id)) {

      ans = structure(list(type = switch(id,
                                     Rf_asLogical = "LGLSXP",
                                     Rf_asIntger = "INTSXP",
                                     Rf_asReal = "REALSXP",
                                     Rf_asChar = "CHARSXP",                                              
                                     NA)
                      ), class = "RScalar")
            

    }  else if(id == "R_MakeExternalPtr") {
        # We'll just assume the tag and prot are of the form Rf_install("literal").
        # We'll extend this later.  Could be a global variable that was initialized
        # to a symbol at some point.

        # We want the type of the object and whether it was allocated.
        tmp = getExtPtrObj(kall[[1]])
        ans = structure(c(tmp, tag = findValue0(kall[[2]][[1]])), class = "RExternalPtr")
#browser()
    } else if(id == "R_do_new_object") {
#browser()        
        nm = kall[[1]]
        if(is(nm, "CallInst") && getName(getCalledFunction(nm)) == "R_do_MAKE_CLASS") 
            nm = nm[[1]]   # Now have a ConstantExpr for eg. rklass.  But need the string.

        val = findValue0(nm)
        ans = structure(list(className = val), class = "S4Instance")
    }  else if(id == "Rf_coerceVector") {
#        browser()
        ans = NULL # Fix.
    }  else if(id == "INTEGER") {
        ans = "INTSXP"
    }  else if(id == "LOGICAL") {
        ans = "LGLSXP"
    }  else if(id == "REAL") {
        ans = "REALSXP"
    }  else if(id == "VECTOR_ELT") {
         # could be in the second argument of VECTOR_ELT
        ans = "VECSXP"
    }  else if(id == "STRING_ELT") {
        ans = "STRSXP"
    } else if(id == "SET_VECTOR_ELT") {
#        browser()
        if(!is.null(var)) {
            w = sapply(kall[], identical, var)
            if(any(w) && which(w) == 3)
                return(NULL)
        }
        ans = kall
    } else  if(id %in% c("Rf_length", "Rf_getAttrib")) {
        ans = NULL
    } else if(id %in% c("Rf_cons", "listAppend", "list2", "list3", "list4", "list5", "list6")) {
        ans = "LISTSXP"  
    } else if(id %in% c("Rf_mkChar", "Rf_mkCharCE")) {
        ans = "CHARSXP"
    } else if(id %in% c("Rf_install", "Rf_installTrChar", "Rf_installChar")) {
        ans = "SYMSXP"
    } else {
        # ans = kall
        # browser()
        fun = cf  # getCalledFunction(kall)
        if(length(getBlocks(fun))) 
            ans = compReturnType(fun) # , .done = list(x, .done))
        else
            ans = structure(list(NULL), class = "Unknown")
    }


    # Now find any other code that manipulates the result in a way that gives us more information about
    # the type of the result, e.g.,  class attribute, names attribute, ...
    # Is this just the calls to Rf_setAttrib ?
    # When processing, for example, mk2 we have calls to Rf_protect() and return()
    # but these just create duplicate - almost, i.e. some with less information about the class.
    # XXX Find out what other instructions and calls to routines we need to capture the details
    # describing the R object.

    users = getAllUsers(x)
    w = sapply(users, isCallTo, "Rf_setAttrib")

    if(any(w))
        for(u in users[w])
            ans = findSetAttributes(u, ans, x)

    ans
}


setMethod("getCallType", "CallInst",   tmp)        
setMethod("getCallType", "InvokeInst", tmp)

###############

getCharVectorEls =
    #
    # Given an instruction that corresponds to something on which the code calls SET_STRING_ELT
    # we see if the values of the elements are literal characters that we can now at compile time
    # and query them.
    #  m = parseIR("tests/classes.ir")
    #  z = getBlocks(m$mk2)[[1]][[5]]
    #  getCharVectorEls(z)
    #
    #
function(x)    
{
    u = getAllUsers(x)
    unlist(lapply(u, function(x)
                       if(isCallTo(x, "SET_STRING_ELT")) {
                          getCharEl(x[[3]])  #??? Do we also need the 2nd argument giving the element number in case they are not in order.
                      } else if(is(x, "CallInst"))
                          getCharVectorEls(x)
                      else
                         character()))
}

getCharEl =
    #
    # Anticipate passing the 3rd argument in a call to SET_STRING_ELT to this routine.
    #
    #
function(x)
{
    if(isCallTo(x, "Rf_mkChar"))
        x = x[[1]]

    if(is(x, "ConstantExpr"))
        x = x[[1]]

    if(is(x, "GlobalVariable"))
       getValue(x)
    else
       NA
}

isCallTo =
    #
    # Checks if this instruction is a CallInst to any of the routines named in the fun character vector.
    #
function(x, fun)
  is(x, "CallInst") && getName(getCalledFunction(x)) %in% fun


##########

# Identify where an object is subsequently manipulated to change its nature,
# e.g., set a class, names, length, dim, row/column names.
#
#  ins is the instruction that uses the return value
#  irvalue is the return value instruction
#  to is the R object currently describing the return type as an R object, i.e. from getCallType().
#
setGeneric("findSetAttributes", function(ins, to, irvalue, ...) standardGeneric("findSetAttributes"))

setMethod("findSetAttributes", "ANY",
function(ins, to, irvalue, ...) {
    to  
})

setMethod("findSetAttributes", "CallInst",
function(ins, to, irvalue, ...)
{
      id = getName(getCalledFunction(ins))


      if(id %in% "Rf_setAttrib" && identical(irvalue, ins[[1]])) {

          at = getAttribName(ins[[2]])
          val = getAttribValue(ins[[3]])

          to = if(at == "class") {
                    # separate attribute or put the literal values into val ?

              
              #XXXX fix  smooth the two cases when we have Rf_setAttrib(x, class, ScalarString()) and Rf_setAttrib(x, class, classVector we populate elsewhere)
              
              # For mk2 in classes.c val 
              # val is a list with the RVector describinng a STRSXP with 2 elements.
              # We could just return the names of the classes if we have literal values
              # But if these are not known at compile time, we may want to return
              # a structure describing what we do know.
                  if((is.list(val) && is(val[[1]], "RVector")) || is(val, "RVector") )
                     val = getCharVectorEls(ins[[3]])
              #              else if(is.character(val))
              # Do we want to structure the simple name e.g. foo from Rf_setAttrib(ans, class, "foo")
              # into a RVector of type STRSXP with length 1 and elements = "foo"
                  attr(to, "RClassLabels") = val
                  to
           } else
                  to

      }

      to
})


setGeneric("getAttribName", function(x, ...) standardGeneric("getAttribName"))
setGeneric("getAttribValue", function(x, ...) standardGeneric("getAttribValue"))

setMethod("getAttribName", "ANY", function(x, ...) {
    message("getAttribName default method for ", class(x))
    NA
})

setMethod("getAttribValue", "ANY", function(x, ...) {
    message("getAttribValue default method for ", class(x))
    NULL
})


setMethod("getAttribName", "LoadInst",
function(x, ...)
    getAttribName(x[[1]]))

setMethod("getAttribName", "GlobalVariable",
function(x, ...)
{
    val = getName(x)
    if(grepl("^R_.*Symbol$", val))
        val = tolower(gsub("^R_(.*)Symbol$", "\\1", val))

    val
})


setMethod("getAttribName", "CallInst",
function(x, ...)
{
    # See the call to Rf_install("class") in listEls2
    fun = getName(getCalledFunction(x))

    if(fun %in% "Rf_install") 
        return( getAttribName( x[[1]] ) )

    NA
})


setMethod("getAttribName", "PHINode",
function(x, ...)
{
    lapply(x[], getAttribName)
})

setMethod("getAttribName", "ConstantExpr",
function(x, ...)
{
    getValue(x[[1]])
})


setMethod("getAttribValue", "CallInst",
function(x, ...)
{
    fun = getName(getCalledFunction(x))
    if(fun %in% c("Rf_allocVector", "Rf_allocVector3")) {
        getCallType0(x)
    } else if(fun %in% c("Rf_ScalarString", "Rf_mkChar"))
        getAttribValue(x[[1]])
    else if(any(x[-length(x)] %in% (params <- getParameters(as(x, "Function"))))) {
        i = match(x[-length(x)], params)
        structure(list(instruction = x), class = "SymbolicComputation", usesParameters = structure(i, names = names(params[i])))
    } else
        NA

})

setMethod("getAttribValue", "ConstantExpr",
function(x, ...)
{
    getAttribValue(x[[1]])
})

setMethod("getAttribValue", "GlobalVariable",
function(x, ...)
{
    getValue(x)
})


###################

findValue0 =
function(val, rtype = FALSE, .done = list(), ...) {
    if(any(sapply(.done, identical, val)))
        return(NULL)

     findValue(val, rtype, c(.done, val), ...)
}

setGeneric("findValue",
function(val, rtype = FALSE, .done = list(), ...) {
               ans = standardGeneric("findValue")
               if(rtype) {
                   mapRType(ans)
               } else
                   ans
           })

setMethod("findValue", "ANY",
          function(val, rtype = FALSE, .done = list(), ...) {
              message("findValue default ", class(val), llvmDump(val), "\n")
#print(length(.done))              
#browser()
          })

tmp = function(val, rtype = FALSE, .done = list(), ...)  findValue0(val[[1]], rtype, .done = .done, ...)
setMethod("findValue", "CastInst", tmp)
setMethod("findValue", "GetElementPtrInst", tmp)
# Uncommenting this next method causes infinite recursion for some IR code
# specifically due to PHI nodes whose elements refer back to the same PHInode.
#setMethod("findValue", "OverflowingBinaryOperator", tmp)


setMethod("findValue", "AllocaInst",
function(val, rtype = FALSE, .done = list(), ...)  {

              u = getAllUsers(val)
              w = sapply(u, is, "StoreInst")
              if(any(w)) {
                  ans = lapply(u[w], function(x) findValue0(x[[1]], rtype, .done, ...))
                  if(sum(w) > 1) warning("@DTL: check this case")
                  ans[[1]]
              } else
                  NULL
          })


setMethod("findValue", "ConstantInt",
          function(val, rtype = FALSE, .done = list(), ...) 
               getValue(val)
          )

setMethod("findValue", "SelectInst",
           function(val, rtype = FALSE, .done = list(), ...) 
               sapply(val[2:3], findValue0, rtype, .done, ...)
          )

setMethod("findValue", "LoadInst",
function(val, rtype = FALSE, .done = list(), ...)  {

               findValue0(val[[1]], rtype, .done, ...)
           })


setMethod("findValue", "CallInst",
function(val, rtype = FALSE, .done = list(), ...) {
              cf = getCalledFunction(val)
              if(!is(cf, "Function")) {
                  tmp = resolveFunction(cf)
                  if(is.null(tmp))
                      return(NULL)
              }

              fn = getName(cf)
          if(!is.na(fn)) {
              if(fn %in% c("Rf_asInteger", "Rf_asLogical", "Rf_asReal")) {
                  return(findValue0(val[[1]], rtype, .done, ...))
              }

              if(grepl("^Rf_.*length$", fn)) {
                  ans = findValue0(val[[1]], rtype, .done, ...)
                  if(is.null(ans)) ans = NA
                  return(structure(ans, class = c(class(ans), "SymbolicLength" )))                  
              }

             if(fn == "Rf_coerceVector") {
                 ans = findValue0(val[[1]], .rtype, .done, ...)
                 if(is.null(ans)) ans = list()                 
                 return(structure(list(obj = ans, to = getRType(findValue0(val[[2]], .done = .done))), class = c(ans, "Coerce" )))
             }

             if(fn == "getListElement") {
                 tmp = findValue0(val[[1]], rtype, .done, ...)
                 return(structure(list(obj = tmp, elName = findValue0(val[[2]], .done = .done, ...)), class = "ListElement"))
             }

              if(fn %in% c("Rf_nrows", "Rf_ncols")) {
                  ans = findValue0(val[[1]], rtype, .done, ...)
                  #XXX Probably want ans to be a list(ans) rather than merging/concatenating classes here.
                  return(structure(ans, class = c(class(ans), if(fn == "Rf_nrows") "SymbolicNrows" else "SymbolicNcols", "SymbolicDim")))
              }

              if(fn == "Rf_mkChar")
                  return(findValue0(val[[1]], rtype, .done, ...))

              if(fn == "R_do_MAKE_CLASS")
                  return(findValue0(val[[1]], rtype, .done, ...))              

          }
              
              if(is.na(fn) || fn != "getListElement") {
                  if(is(val, "CallInst") && getName(val[[length(val)]]) == "INTEGER")
                      return(findValue0(val[[1]], rtype, .done, ...))
              }
              
              NULL
          })


setMethod("findValue", "ConstantExpr",
          function(val, rtype = FALSE, .done = list(), ...) {
              findValue0(as(val, "Instruction"), rtype, .done, ...)
          })

setMethod("findValue", "GlobalVariable",
           function(val, rtype = FALSE, .done = list(), ...) {
               id = getName(val)
               if(id %in% c("R_NilValue"))
                   return(structure(list("NULL"), class = "RNull"))
               
               findValue0(getInitializer(val), rtype, .done, ...)
          })

setMethod("findValue", "ConstantDataSequential",
          function(val, rtype = FALSE, .done = list(), ...) {
              .Call("R_ConstantDataSequential_getAsCString", val)              
          })


setMethod("findValue", "Argument",
          function(val, rtype = FALSE, .done = list(), ...) {
              name = getName(val)
              if(is.na(name))
                  name = gsub(".*%", "%", as(val, "character"))
              structure(name, class = "Parameter")
          })

setMethod("findValue", "PHINode",
function(val, rtype = FALSE, .done = list(), seen = list(), ...) {

             if(any(sapply(seen, identical, val)))
                 return(NULL)
    
              ans = lapply(val[], findValue0, rtype, .done, seen = c(seen, val), ...)
              names(ans) = sapply(val[], as, 'character')
              ans
          })

mapRType =
function(val, map = RSEXPTypeMap)
{
    i = match(val, map)
    names(map)[i]
}

RSEXPTypeMap =
    c(NILSXP = 0, SYMSPX = 1, LISTSXP =2, CLOSXP = 3, ENVSXP = 4, PROMSXP = 5, LANGSXP = 6,
      CHARSXP = 9, LGLSXP = 10, INTSXP = 13, REALSXP = 14, CPLXSXP = 15, STRSXP = 16,
      VECSXP = 19, RAWSXP = 24)



getExtPtrObj =
function(call)
{
    isAlloc = FALSE
    if(is(call, "CallInst")) {
        fun = getName(getCalledFunction(call))
        isAlloc = fun %in% c("malloc", "calloc")
    }
    
    # How do we find the type from the IR. We just have the size.
    # and the code deals with offsets, not field names.

    ans = list(allocated = isAlloc)
        
}




compParamTypes =
    #
    # Want to get the types and lengths, etc. for each parameter/input based on
    # how it is used.
    # Also want to get the relationships between them, e.g. same length.
    # This works on all of the parameters for a routine
    #
    # Todo:
    #  find out the length and if scalar for a vector. i.e. don't just stop at INTEGER() and say vector.
    #     Determine which other vectors this is parallel to in length.
    #     Get the users of a parameter, and their users, etc.
    #  Map e.g. strings or integers to enumerated values.
    #
    #
function(fun, params = getParameters(fun), ...)
{
   lapply(params, compParamType, ...)
}


compParamType =
    #
    # This works on a single parameter to identify the specific R type
    # based on how it is used.
    #
    # The idea is simple. We find all uses of the parameter within the routine
    # and then look at those to identify if they indicate a specific R type.
    #
    # We get back information from all uses.  We want to combine these
    # (like a constraint in Nick U's world) to a specific type.
    # e.g. z3 = compParamTypes(m3$rpart)
    #  z3$xmat2 returns REALSXP and two RMatrix values.
    #  So this is a numeric matrix, i.e. a matrix with numeric cells/mode.
    #
function(p, users = getAllUsers(p), ...)
{
    ans = lapply(users, compInputType, ...)
    names(ans) = sapply(users, as, 'character')
    ans
}

# compInputType aims at determining the specific R type based of a parameter/input
# (not the result) for a routine based on how it is used.
# We can do this in cases such as INTEGER(p1), REAL(p2), asInteger(p3),
# length(p4) (gives us some information), VECTOR_ELT(), STRING_ELT()
# Rf_coerceVector().
# This can be extended a great deal but is just here as proof of concept.
setGeneric("compInputType", function(x, ...) standardGeneric("compInputType"))

setMethod("compInputType", "CallInst",
          function(x, ...) {

              fn = getName(getCalledFunction(x))
              ans = getRTypeFromFunName(fn)

              if(length(ans) > 0)
                  return(list(type = ans))

              if(fn == "Rf_coerceVector") {
                 return(list(type = mapRType(x[[2]])))
              }

              if(fn %in% c("Rf_nrows", "Rf_ncols"))
                  return(list(type = "RMatrix"))
              
#              browser()
              "???"
          })

setMethod("compInputType", "FPMathOperator",
          function(x, ...) {
              compInputType(x[[1]])
          })

setMethod("compInputType", "Function",
          function(x, ...) {
              getRTypeFromFunName(getName(x))
          })



getRTypeFromFunName =
    #
    # Maps the name of C routine to an R type
    # when we know the C routine is specific to that type.
    # This only takes the function name. It doesn't handle cases
    # such as Rf_allocMatrix() or Rf_coerceVector() which require
    # additional information, i.e., the R type in the first and second arguments
    # respectively.
function(fn)
{
   ans = switch(fn,
                INTEGER = "INTSXP",
                Rf_asInteger = c("INTSXP", "Scalar"),
                REAL = "REALSXP",
                Rf_asReal = c("REALSXP", "Scalar"),
                LOGICAL = "LGLSXP",
                STRING_ELT = "STRSXP",
                CHAR = "CHARSXP",
                VECTOR_ELT = "VECSXP",
                LENGTH = c("SEXP", "Vector"),
                character())
   ans
}




findTypeByReference =
function(call, var, argNum, ...)
{
    fun = getCalledFunction(call)
    funName = getName(fun)
    m = as(call, "Module")
    fun = m[[funName]]
    p = getParameters(fun)
    rtypeSetTo(p[[argNum]])
}

rtypeSetTo =
function(p)
{
    u = getAllUsers(p)
    getCallType0(p)
}


# Remove as now in Rllvm.
setAs("Instruction", "Module",
      function(from) {
          as(getParent(from), "Module")
      })




returnsArg =
    #
    # Note that the IR has returned as an attribute on the parameter.
    #
    #
function(f, params = getParameters(f))
{
    ret = getReturnInstructions(f)
    if(length(ret) == 0)
        return(FALSE)

    w = sapply(params, identical, ret[[1]])
    if(any(w))
        which(w)
    else
        FALSE
}


returnsArg =
    # 2nd version that uses hasReturned attribute on the paramters
function(f, params = getParameters(f))
{
    w = sapply(params, hasReturned)
    if(!any(w))
        FALSE
    else
        which(w)
}


#######################

setGeneric("compRType",
    # for calls to routines such as Rf_allocVector() where we 
    # get the type of the desired/allocated R object but when it is not
    # a simple value (either a literal or a variable) but a computation.
    # For example in matrixStats::allocVector2, the type is the TYPEOF(arg1)
function(val, ...)
{
  standardGeneric("compRType")
})

setMethod("compRType", "CallInst",
function(val, ...)
{
    cf = getCalledFunction(val)
    # could be a function pointer
    if(getName(cf) == "TYPEOF") {
        ty = compRType(val[[1]])
        return(structure(list(of = ty), class = "TypeOf"))
    }
})

setMethod("compRType", "Argument",
function(val, ...)
{
    ty = inferParamType(val)
    if(is(ty, "ANY"))
        return(structure(list(as = val), class = "SameRTypeAs"))

    ty
})




#################################

# For arguments that are part of the return value
# we want to find where
#  1. it is passed by reference to another routine and so could be completely changed
#  2. its characteristics are changed by calls to setAttrib(), set length


modifiesSEXP =
function(obj)
{
browser()
}




############
setGeneric("resolveFunction",
           function(x, ...)
           standardGeneric("resolveFunction"))

setMethod("resolveFunction", "LoadInst",
           function(x, ...)
             resolveFunction(x[[1]], ...))

setMethod("resolveFunction", "CastInst",
           function(x, ...)
              resolveFunction(x[[1]], ...))

setMethod("resolveFunction", "GetElementPtrInst",
           function(x, ...) {
               obj = x[[1]]
               #XXX if obj refers to a global variable and we can determine
               # its possible values, then we can follow that and get the elements
               # Otherwise, it is truly run-time and unknowable.
               NULL
           })

