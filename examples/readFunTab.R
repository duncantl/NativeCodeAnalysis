if(FALSE) {
    # List the .ir files in R/src/main
    # Parse them, some may fail.
ir = list.files(file.path(R.home(), "src/main"), pattern = "\\.ir$", full = TRUE)
mods = lapply(ir, function(f) try(parseIR(f)))
names(mods) = basename(ir)
err = sapply(mods, is, 'try-error')

    # Find the names of the defined routines in each of the modules (successfully parsed)
tmp = lapply(mods[!err], getDefinedRoutines)
    # Make a vector of file names whose names elements are the routines.
tmp2 = structure( rep(basename(ir[!err]), sapply(tmp, length)), names = unlist(tmp))

    # Read the R_FunTab from names.c mapping the .Internal/.Primitive functions to routine.
funTab = readFunTab()    
funTab$file = NA
m = match(funTab$routine, names(tmp2))
funTab$file[ !is.na(m) ] = tmp2[m[!is.na(m)]]

w = !is.na(funTab$file)

#XXX May need to pass the offset to analyze the code more accurately.
# We will be repeating the analysis for the same routine that maps to multiple R functions.
# Extra work unless we take into account the offset.
rtypes = mapply(function(id, file) {
                  print(c(id, file))
                  if(id == "do_enc2") return(structure(NULL, class = "try-error"))
                  try(getRReturnTypes(mods[[file]][[id]]))
                },
    funTab$routine[w], funTab$file[w])
names(rtypes) = funTab$rFun[w]
err2 = sapply(rtypes, is, 'try-error')
# 70 had errors.
names(err2)[err2]

#
errorInfo = funTab[names(err2)[err2], c("routine", "file")]
# 29 unique routines in 14 files that gave rise to errors.
#
# Some of the errors are because a CallInst have a non Function object being called, but
# an instruction, e.g.,
#   mods$internet$do_curlVersion
#

etypes = mapply(function(id, file) {
                  print(c(id, file))
                  if(id == "do_enc2") return(structure(NULL, class = "try-error"))
                  try(getRReturnTypes(mods[[file]][[id]]))
                },
    errorInfo$routine, errorInfo$file)
names(etypes) = rownames(errorInfo)

ee = sapply(etypes, is, 'try-error')
etypes[ee]
errorInfo[ee,]



saveRDS(rtypes, "RFunTabReturnTypes.rds")

rtypes2 = unlist(rtypes, recursive = FALSE)
#rtypes2 = unlist(lapply(rtypes, function(x) lapply(x, unique)), recursive = FALSE)
}

readFunTab =
function(rmain = file.path(R.home(), "src/main"),
         m = parseIR(file.path(rmain, "names.ir")))
{
    fntab = getValue(m[["R_FunTab"]])
    if(is.null(fntab[[length(fntab)]]))
        fntab = fntab[ - length(fntab) ]

    ans = data.frame(rFun = sapply(fntab, `[[`, 1),
                     routine = sapply(fntab, function(x) getName(x[[2]])),
                     offset = sapply(fntab, `[[`, 3),
                     eval = sapply(fntab, `[[`, 4),
                     arity = sapply(fntab, `[[`, 5))

    ids = ans$rFun
    w = duplicated(ids)
    ids[w] = paste0(ids[w], "2")
    rownames(ans) = ids

    z = t(sapply(fntab, function(x) unlist(x[[6]])))
    colnames(z) = c("ppkind", "precedence", "rightassoc")
    cbind(ans, z)
}
