
# tu = createTU("~/ifSetVectorElt.c")
#tu = createTU("~/ifSetVectorElt.c", c("~/R-devel/build/include", "~/local/lib/clang/11.0.0/include"))

genGetIf = function()
{
    ifs = list()
    function(cur, parent) {
             if(cur$kind == CXCursor_IfStmt)
                 ifs[[length(ifs) + 1L]]  <<- cur
             2L
    }
}

if(FALSE) {
f2 = genGetIf()
visitTU(tu, f2, TRUE)
ifs = environment(f2)$ifs
ifs = ifs[ grepl("ifSetVectorElt.c", sapply(ifs, getFileName)) ]
}

getSetVectorElt =
function(cmd)
{
    ans = list()
    f = function(cur, parent) {
          if(cur$kind == CXCursor_CallExpr && length(cur) > 0 && getName(cur[[1]]) == "SET_VECTOR_ELT") {
              ans[[length(ans) + 1L]] <<- cur
          }
          2L
    }
    visitChildren(cmd, f, TRUE)
    ans
}

getIfs =
function(r)
{
    f2 = genGetIf()
    visitChildren(r, f2, TRUE)
    environment(f2)$ifs    
}

processRoutine =
function(r, ifs = getIfs(r))
{
    ans = lapply(ifs, getSetVectorElt)
    w = sapply(ans, length) > 0
    mapply(c, ifs[w], ans[w], SIMPLIFY = FALSE)
}


BaseIncludes = c("~/R-devel/build/include",
    "~/local/lib/clang/11.0.0/include",
    "~/Rpackages4/Rcpp/include",
    "~/LLVM/llvm-project-11.0.0/libcxx/include/",
    "/usr/local/include")

processFile =
function(f, includes = BaseIncludes, ...)
{
    tu = createTU(f, includes = includes, ...)
    rr = getRoutines(tu, basename(f))
    lapply(rr, processRoutine)
}

processDir =
function(d, flags = getCFlags(d))
{
    includes = BaseIncludes

    w = grepl("^-I", flags)
    args = flags[!w]
    if(any(w))
      includes = c(includes, gsub("^-I", "", flags[w]))
    
    cur = getwd()
    on.exit(setwd(cur))
    setwd(d)
    
    cf = list.files(d, full = FALSE, pattern = "\\.(c|cpp|cc)$")    
    tmp = lapply(cf, processFile, includes = includes, args = flags)

    names(tmp) = basename(cf)
    tmp
}


getCFlags =
function(d)
{
    mk = file.path(d, "Makevars")
    if(!file.exists(mk))
        return(character())

if(FALSE) {
    ll = readLines(mk)
    tmp = grep("PKG_C(PP)?FLAGS", ll, value = TRUE)
    if(length(tmp) == 0)
        return(character(0))
    
    flags = gsub("PKG_C(PP)?FLAGS *=", "", tmp)

} else {
    #XXXX Not getting the flags from R itself
    # for the laGP package, we need the -DNDEBUG that R provides to avoid the error from gp_sep.c 
    # about gab not being a field in a struct 
    flags = system(sprintf("R CMD make -f $HOME/MyMake flags PKG_DIR=%s", d), intern = TRUE)
}

    flags = gsub("(^|[[:space:]])-I ", " -I", flags) # deformula package has a space between -I and include
    
    vals = unlist(strsplit(flags, "[[:space:]]+"))

    
    i = grep("^#", vals)
    if(length(i))
        vals = vals[ - (i:length(vals)) ]

    vals = vals[ vals != ""]

    w = grepl("^\\$", vals)
    if(any(w)) {
        warning("Found make variables in flags: ", paste(vals[w], collapse = ", "))
        vals = vals[!w]
    }
    vals
}

if(FALSE) {
tu = createTU("~/CRAN2/Pkgs/corpus/src/term_stats.c", c("~/R-devel/build/include", "~/local/lib/clang/11.0.0/include"))
rr = getRoutines(tu, "term_stats.c")

lapply(rr, processRoutine)


cf = list.files("~/CRAN2/Pkgs/corpus/src", full = TRUE, pattern = "\\.c$")
tmp = lapply(cf, processFile)



bcs = system("find ~/CRAN2 -name all.bc", intern = TRUE)
names(bcs) = basename(dirname(dirname(bcs)))
pkgSrc = dirname(bcs)

tmp2 = lapply(pkgSrc, function(d) try(processDir(d)))
}
