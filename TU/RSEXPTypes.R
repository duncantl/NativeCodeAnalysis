library(RCIndex)

rh = Sys.getenv("R_HOME")
inc = sprintf("%s/include", rh)
tu = createTU("sexpTypes.c", includes = inc, options = CXTranslationUnit_DetailedPreprocessingRecord)
defs = getDefines(tu, "Rinternals.h")


# grep("INTSXP", names(defs))

i = grep("SXP$", names(defs), value = TRUE)
vals = as.integer(sapply(defs[i], function(x) getCursorTokens(x)["Literal"]))
names(vals) = i

RSEXPTypeValues = vals
save(RSEXPTypeValues, file = "../data/RSEXPTypeValues.rda")
