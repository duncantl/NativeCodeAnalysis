# Native Code in CRAN Packages

We have a mirror of CRAN. It is a little out of date at this time (Feb 10)
having been updated in Dec. 2019. However, the approach and the general
data are still representative.

We want to find all packages that have native code.
While we could analyze all of the R code in each package,
we'll start by finding each package that has a `useDynLib()`
directive in its NAMESPACE file.
We use the shell command
```
find . -name NAMESPACE -maxdepth 2 -exec grep -l 'useDynLib(' {} \; > useDynLib.txt
```
3494 of the 14,995 packages have this directive as we see from
```
wc -l useDynLib.txt 
```
```
3494 useDynLib.txt
```

We will then examine only these:
```
dirs2 = readLines("useDynLib.txt")
pkgs = dirname(dirs2)
```
We use `getCCalls()` from the file [NativeFuns.R](NativeFuns.R)
```
ans2 = lapply(pkgs, function(p){print(p); try(getCCalls(p))})
e = sapply(ans2, is, 'try-error')
allCalls = do.call(rbind, ans2[!e])
allCalls$pkg = dirname(dirname(gsub("^\\./", "" , allCalls$file)))
saveRDS(allCalls, "AllNativeCalls.rds")
```
(See /Users/duncan/CRAN/Pkgs/AllNativeCalls.rds)


`getCCalls` loops over the files in the R/ directory of the specified package.
It parses the code in each file and finds all calls to .C, .Call, .External, .External2, .Fortran.
(We should probably look at .External.graphics and .Call.graphics.)
For each call to one of these functions, 
it finds the 
+ type of call
+ name of the routine being called either as a character string or a symbol that needs to be
  resolved via the registration mechanism  
+ the class/type of the routine identifier
+ the PACKAGE argument
+ the name of the file in which the call was found.

We should probably add the name of the function.



Excluding the 100 packages for which we got an error from rstatic
(due to expressions of the form `foo::fun(call) = value`)
we have 38,608 calls to native routines
```
table(allCalls$callType)

        .C      .Call  .External .External2   .Fortran 
      7943      27620        277          4       2764 
```
(The .External2 calls are from the rlang package.)

The .External calls come from various packages
```
sort(table(allCalls[allCalls$callType == '.External', "pkg"]))
                 coxsei                  devEMF 
                      1                       1 
              dotCall64               fastmatch 
                      1                       1 
                 Matrix                  OpenCL 
                      1                       1 
      RandomFieldsUtils                   RGtk2 
                      1                       1 
                   slam              tikzDevice 
                      1                       1 
                 bibtex expint/inst/example_API 
                      2                       2 
                foreign                   IHSEP 
                      2                       2 
          lassoshooting             RPostgreSQL 
                      2                       2 
     spatialsegregation              treethresh 
                      2                       2 
                   cusp                 iotools 
                      3                       3 
                 pbdMPI                     xts 
                      4                       4 
                 expint                    Rcpp 
                      6                      13 
                   SGCS                  wrassp 
                     13                      14 
                 actuar 
                    191 
```
So 191/277 from one package actuar and in this package,
there are only 6 routines called and 154 of the 191 calls are to C_actuar_do_dpq.
This is in actuar/src/dqp.c.



# Rcpp

About 1/3 (12953) of the 36608 calls we found are in a file named RcppExports.R,
indicating Rcpp. There may be others using Rcpp not in files with other names.

```
lto = sapply(names(ans2), function(p) { d = read.dcf(file.path(p, "DESCRIPTION")); i =match("LinkingTo", colnames(d)); d[,i] })
table(is.na(lto)
FALSE  TRUE 
 1720  1800 
```
1681 of these contain some Rcpp package, e.g., Rcpp, RcppEigen

The other packages that are `LinkedTo` are
```
unique(trimws(unlist(strsplit(gsub("\\([^)]+\\)", "", names(table(grep("Rcpp", lto, value = TRUE, invert = TRUE)))), ", *"))))
 [1] "bdsmatrix"         "BH"                "Boom"             
 [4] "bigmemory"         "cinterpolate"      "cubature"         
 [7] "ergm"              "expint"            "libcoin"          
[10] "Matrix"            "mvtnorm"           "network"          
[13] "qtbase"            "RandomFieldsUtils" "ring"             
[16] "rlang"             "sp"                "xts"              
[19] "zoo"  
```

# PACKAGE argument

More calls do not have a PACKAGE argument than do, but it is very close:
```
table(is.na(allCalls$package))
FALSE  TRUE 
18900 19708 
```


```
PACKAGEnaByPkg = with(allCalls, tapply(package, pkg, function(x) table(is.na(x))))
table(sapply(PACKAGEnaByPkg, length))
   1    2 
3043  238 
```
So the overwhelming majority of packages are consistent in all calls within that package
in either using a PACKAGE argument or not in all their calls.

```
plot(do.call(rbind, PACKAGEnaByPkg[w]))
```

Consider entropy.


# Size of the R Code
We can determine how much R "code" there is in each of the packages with native code via
```
for d in `dirname \`cat useDynLib.txt\` `; do du -sk $d/R ; done > RpkgRCodeSize
```
This helps us to determine how long it will take to process the code,
but also a proxy for the proportion of R code that has calls to native code.




#

Using the Rllvm approach, we need to have dependent libraries installed
to generate the IR files.
This is also true for the RCIndex approach, but RCIndex coontinues to parse if it 
cannot find a header file.
