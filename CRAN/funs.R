usesRcpp =
function(d)
{
   dc = read.dcf(file.path(d, "DESCRIPTION"))
   v = c("Imports", "Depends", "LinkingTo")
   m = match(v, colnames(dc), 0)
   any(grepl("Rcpp", unlist(dc[, m])))
}



getPkgName =
function(path, base = "./",  strip = TRUE)
{
    if(strip)
        path = gsub("^([^:]+):.*", "\\1", path)

    base = path.expand(base)
    if(!grepl("/$", base))
        base = paste0(base, "/")
    
    tmp = gsub(base, "", path, fixed = TRUE)
    sapply(strsplit(tmp, "/"), `[`, 1L)
}



usesMethods =
function(pkg, desc = read.dcf(file.path(pkg, "DESCRIPTION")))
{
    m = match(c("Imports", "Depends"), colnames(desc), 0)
    any(grepl("methods", unlist(desc[1, m])))
}

s3Methods =
function(pkg, ns = readLines(file.path(pkg, "NAMESPACE"), warn = FALSE))
{
   length(grep("S3method", ns))
}





dotCallReturnTypes =
function(pkg)
{
    f = file.path(pkg, "all.bc")

    if(!file.exists(f)) {
        warning("no file ", f)
        return(NULL)
    }

    cat("Package", basename(dirname(pkg)), "\n")
    
    m = readBitcode(f)
    funs = getDefinedRoutines(m, names = FALSE)
    funs = funs[sapply(funs, isDotCall)]

    lapply(funs, function(f) {cat("routine:", getName(f), "\n"); try(compReturnType(f)) })
}
