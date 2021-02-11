# Native Code in CRAN Packages

!!  See the parallel directory CRAN.

We have a mirror of CRAN. It is a little out of date at this time (Feb 10)
having been updated in Dec. 2019. However, the approach and the general
data are still representative.

```
find Pkgs -maxdepth 1 -type d | wc -l
```

We want to find all packages that have native code.
While we could analyze all of the R code in each package,
we'll start by finding each package that has a `useDynLib()`
directive in its NAMESPACE file.
We use the shell command
```
find Pkgs -name NAMESPACE -maxdepth 2 -exec egrep -l '^[^#]*useDynLib\(' {} \; > useDynLib.txt
```
3,782 of the 16,161 packages have this directive as we see from
```
wc -l useDynLib.txt 
```
```
3834 useDynLib.txt
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

We should probably add the name of the function in which the call is made.



Excluding the 100 packages for which we got an error from rstatic
(due to expressions of the form `foo::fun(call) = value`)
we have 38,608 calls to native routines
```
table(allCalls$callType)
        .C      .Call  .External .External2   .Fortran 
      8133      35888        278         11       2693 
```
(The .External2 calls are from the rlang and vctrs package.)

The .External calls come from various packages
```
sort(table(allCalls[allCalls$callType == '.External', "pkg"]))
            Pkgs/coxsei             Pkgs/devEMF 
                      1                       1 
         Pkgs/dotCall64          Pkgs/fastmatch 
                      1                       1 
             Pkgs/mable           Pkgs/maptools 
                      1                       1 
            Pkgs/Matrix             Pkgs/OpenCL 
                      1                       1 
 Pkgs/RandomFieldsUtils              Pkgs/RGtk2 
                      1                       1 
              Pkgs/slam         Pkgs/tikzDevice 
                      1                       1 
             Pkgs/vctrs             Pkgs/bibtex 
                      1                       2 
           Pkgs/foreign              Pkgs/IHSEP 
                      2                       2 
     Pkgs/lassoshooting        Pkgs/RPostgreSQL 
                      2                       2 
Pkgs/spatialsegregation         Pkgs/treethresh 
                      2                       2 
              Pkgs/cusp            Pkgs/iotools 
                      3                       3 
            Pkgs/pbdMPI                Pkgs/xts 
                      4                       4 
            Pkgs/expint               Pkgs/Rcpp 
                      6                      13 
              Pkgs/SGCS             Pkgs/wrassp 
                     13                      14 
            Pkgs/actuar 
                    191 
```
So 191/278 from one package actuar and in this package,
there are only 6 routines called and 154 of the 191 calls are to C_actuar_do_dpq.
This is in actuar/src/dqp.c.



# Rcpp

About 1/3 (12953) of the 36608 calls we found are in a file named RcppExports.R,
indicating Rcpp. There may be others using Rcpp not in files with other names.

```
lto = sapply(names(ans2), function(p) { d = read.dcf(file.path(p, "DESCRIPTION")); i = match("LinkingTo", colnames(d)); d[,i] })
table(is.na(lto)
FALSE  TRUE 
 1720  1800 
```
1681 of these contain some Rcpp package, e.g., Rcpp, RcppEigen


Another approach (added later) is 
```
usesRcpp =
function(d)
{
   dc = read.dcf(file.path(d, "DESCRIPTION"))
   v = c("Imports", "Depends", "LinkingTo")
   m = match(v, colnames(dc), 0)
   any(grepl("Rcpp", unlist(dc[, m])))
}

rcpp = sapply(npkgs, usesRcpp)
table(rcpp)
 FALSE  TRUE 
  1776  2158 
```
So slightly over 50% of the packages that have native C/C++ code use Rcpp!



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
20518 26485 
```


```
PACKAGEnaByPkg = with(allCalls, tapply(package, pkg, function(x) table(is.na(x))))
table(sapply(PACKAGEnaByPkg, length))
   1    2 
3415  267 
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



# Mapping Name of Routine in R to Native Code

Packages can provide a mapping of routine names
in various ways


One way is to add a prefix and/or suffix for all routines.
So if we specified `.fixes = c("C_", "_r")`,
the call
`.C("foo")` would map to C_foo_r.
These pre-/sufffixes can be specified in the 

We can find the pagkages that have a .fixes value in the o
```
for f in `cat useDynLib.txt` ; do ag -l \\.fixes $f ; done
```



# Registration Changes
To find native registration routines that map 

```
for f in `cat useDynLib.txt | sed -e 's|/NAMESPACE|/src|g'` ; do ag -l R_registerRoutines $f ; done > registerRoutines.txt 
```



```
make -f ~/GitWorkingArea/NativeCodeAnalysis/examples/IRMakefile native_routine_registration.ir
```


```
library(Rllvm)
m = parseIR("~/CRAN2/Pkgs/easyVerification/src/native_routine_registration.ir")
u = getAllUsers(m$R_registerRoutines)

u[[1]][-1]
```
