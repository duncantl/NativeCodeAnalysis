pkgDotCallTypes = 
function(irFile)
{
    m = readBitcode(irFile)
    .calls = findDotCallRoutines(m)
    rts = lapply(.calls, compReturnType) #XXX getRReturnTypes,
    # param types
}
