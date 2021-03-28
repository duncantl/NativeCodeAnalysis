# Consider the PKI package.
# It sets a class attribute in C code with the value symmetric.cipher.
# In other C code in the package, it checks Rf_inherits(sKey, "symmeric.cipher")
# Note that the inherits is missing the 't'. This is a simple error that can be difficult to find.
# However, code analysis tools can catch these.
#
# In fact in PKI, the code that sets the class name symmetric.cipher is #ifdef'ed out
# so doesn't actually ever set the class. However, it is still a reasonable example.
#
#
#
#
#

source("rapiFuns.R")

getS3CClassDefs =
function(m)
{
    classSet = getSetAttrCalls(m)
    defs = unique(unlist(sapply(classSet, getSymbolName)))
    defs = defs[!is.na(defs)]
}



if(FALSE) {
    mod = readBitcode("~/CRAN2/Pkgs/PKI/src/all.bc")
    mod = readBitcode("~/CRAN2/Pkgs/iotools/src/all.bc")

    inh = unlist(getInheritsClass(mod))

    defs = getS3CClassDefs(mod)

# Now find the class names in defs that is closest to those in m1
    m1 = setdiff(inh, defs)
    d = adist(m1, defs)
    dimnames(d) = list(m1, defs)
    sapply(1:nrow(d), function(i) {
                       w = d[i, ] < round(nchar(m1[i])*.3)
                       d[i, w][ order(d[i,w]) ]
                   })
}
