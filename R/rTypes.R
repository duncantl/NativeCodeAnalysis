RSEXPTypeValues.df = data.frame(typeNum = c(NILSXP = 0L, SYMSXP = 1L, LISTSXP = 2L, CLOSXP = 3L, ENVSXP = 4L, 
PROMSXP = 5L, LANGSXP = 6L, SPECIALSXP = 7L, BUILTINSXP = 8L, 
CHARSXP = 9L, LGLSXP = 10L, INTSXP = 13L, REALSXP = 14L, CPLXSXP = 15L, 
STRSXP = 16L, DOTSXP = 17L, ANYSXP = 18L, VECSXP = 19L, EXPRSXP = 20L, 
BCODESXP = 21L, EXTPTRSXP = 22L, WEAKREFSXP = 23L, RAWSXP = 24L, 
S4SXP = 25L, NEWSXP = 30L, FREESXP = 31L, FUNSXP = 99L),
rtypeName = c(NILSXP = "NULL", SYMSXP = "symbol", LISTSXP = "VECSXP", CLOSXP = "CLOSURE", ENVSXP = "environment", 
PROMSXP = "PROMISE", LANGSXP = "language", SPECIALSXP = "SPECIAL", BUILTINSXP = "builtin", 
CHARSXP = "CHAR", LGLSXP = "logical", INTSXP = "integer", REALSXP = "numeric", CPLXSXP = "complex", 
STRSXP = "character", DOTSXP = "...", ANYSXP = "ANY", VECSXP = "list", EXPRSXP = "expression", 
BCODESXP = "bytecode", EXTPTRSXP = "externalptr", WEAKREFSXP = "WEAKREF", RAWSXP = "raw", 
S4SXP = "S4", NEWSXP = "NEW", FREESXP = "FREE", FUNSXP = "FUN"), stringsAsFactors = FALSE)


getRType =
function(val, sexpTypes = RSEXPTypeValues.df)
{
    if(!is(val, "numeric")) {
        return(switch(class(val),
                      Argument = val,
                      val))
    }
    
    i = match(val, sexpTypes[[1]])
    structure(sexpTypes$rtypeName[i], names = rownames(sexpTypes)[i])
}
