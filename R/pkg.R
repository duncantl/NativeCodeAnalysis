pkgDotCallTypes = 
function(irFile)
{
    m = readBitcode(irFile)
    .calls = findDotCallRoutines(m)
    rts = lapply(.calls, compReturnType)
    # param types
}
