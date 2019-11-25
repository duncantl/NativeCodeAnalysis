
#
# This is for cases where we don't get a literal string as the
# name of the symbol to call
#

mapRoutineName =
    # sym is what is in the .C/.Call/.Fortran
function(sym, package)
{

    routines = getNamespace("overlap")$.__NAMESPACE__.$nativeRoutines
    rr = routines[[1]]
    if(sym %in%names(rr))
        rr[[sym]]
    else
        sym
}

if(FALSE) {
      # 
    mapRoutineName("ccnSetSeed", "catnet")
      # uses .fixes to prepend C_
    mapRoutineName("C_densRad", "overlap")
}


