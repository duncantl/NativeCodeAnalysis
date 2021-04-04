library(Rllvm)

mods = lapply(bcs, readBitcode)
ins = lapply(mods, function(x) unlist(getInstructions(x)))
alloc = lapply(ins, function(i) i[ sapply(i, function(x) is(x, "CallInst") && is((cf <- getCalledFunction(x)), "Function") && grepl("^Rf_allocVector", Rllvm::getName(cf)) ) ])


ualloc = unlist(alloc)

k = sapply(ualloc, function(x) class(x[[1]]))
table(k)

#      Argument BinaryOperator       CallInst    ConstantInt       LoadInst        PHINode     SelectInst 
#             8             25             53          11958             10             17             15 

klen = sapply(ualloc, function(x) class(x[[2]]))

#      Argument BinaryOperator       CallInst    ConstantInt     FPToSIInst     FPToUIInst       LoadInst        PHINode     SelectInst       SExtInst       ZExtInst 
#            15            374            178           4973              5              8            183            214             62           5313            761 

isList = sapply(ualloc, function(x) is(x[[1]], "ConstantInt") && getValue(x[[1]]) == 19L)

klist = sapply(ualloc[isList], function(x) class(x[[2]]))

lapply(ualloc[isList] [ klist == "SelectInst" ], `[[`, 2)



# There are  11 calls where the length is a SelectInst, so  a variable number.
#vv = lapply(ualloc[isList] [ klist == "SelectInst" ], function(x) sapply(x[[2]][-1], getValue))

a = lapply(ualloc[isList], function(x) unique(unlist(xgetValue(x[[2]]))))
vv = a[sapply(a, length) > 1 & sapply(a, function(x) !any(is.na(x)))]
saveRDS(vv, "ListAllocWithVariableLength.rds")
cat(sapply(strsplit(unique(names(vv)), "\\."), function(x) paste(x[1:2], collapse = "::")), sep = "\n")



#vv = lapply(ualloc[isList] [ klist == "PHINode" ], function(x) sapply(x[[2]], getValue))
#which(sapply(ualloc[isList] [ klist == "ZExtInst" ], function(x) class(x[[2]][[1]])) == "SelectInst")




################

# Where we have different type of R object

a = lapply(ualloc, function(x) unique(unlist(xgetValue(x[[2]]))))
vv = a[sapply(a, length) > 1 & sapply(a, function(x) !any(is.na(x)))]

cat(sapply(strsplit(unique(names(vv)), "\\."), function(x) paste(x[1:2], collapse = "::")), sep = "\n")

# The ones where we don't have a known compile time type.
w = sapply(a, function(x) !any(is.na(x)))
table(sapply(ualloc[w], function(x) class(x[[1]])))
#      Argument BinaryOperator       CallInst       LoadInst        PHINode     SelectInst 
#             8             25             53             10             13              1 

# Of the 53 calls, 46 are to TYPEOF and the other 7 to routines that map to a SEXPTYPE
table(sapply(ualloc[w][sapply(ualloc[w], function(x) is(x[[1]], "CallInst"))], function(x) Rllvm::getName(getCalledFunction(x[[1]]))))

# Which packages do these come from?
table(sapply(strsplit(names(ualloc[w]), "\\."), `[`,1))

