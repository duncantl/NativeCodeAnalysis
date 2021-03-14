# Analyzing Native Code in CRAN Packages

Our goal here is to analyze the C/C++ code in R packages -  on CRAN for now.
In each R package that contains a src/ directory that contains C or C++ code, 
we will
+ configure that package 
+ generate the IR code from the .c/.cc/.cpp/.C files 
+ link the IR code into one module `all.bc`

We won't bother installing the packages a) we would have to deal with the dependencies,
and b) we don't actually want to run any of their code or even compile and link
the native code.

## Getting CRAN

We first fetch the packages (and only the packages).
In a directory I name CRAN/, we issue
```
rsync -rtlzv --include='*.tar.gz' --exclude='Recommended' --exclude="Archive" --exclude="Symlink" --exclude="Old" --exclude="Orphaned" cran.r-project.org::CRAN/src/contrib .
```
and that creates the contrib/ directory containing over 16 thousand .tar.gz files.

We possibly want to include Symlink and so might remove the corresponding `--exclude`.

Since we want to look at the source code for these, we extract the files from each archive,
i.e. expand each.  We create a directory parallel to contrib/ named, say Pkgs, and
then expand each .tar.gz there.  
```
mkdir Pkgs
cd Pkgs
for f in ../contrib/*.tar.gz ; do echo $f; tar zxf $f ; done
```
Some of these will fail. They are symbolic links to typically a file in a Symlink/ subdirectory that
we excluded. For each of these, there is another version of the same package.

Putting these in a separate directory rather than directly in contrib/ makes it easier to update
them correctly and to simply remove all the extract files in one go.
This is like our build directory when compiling source.


## Finding the Packages with C/C++ code

To find the R packages that contain C/C++ code we look for
a src/ directory directly under the package root
and check that there is one or more files with a .c/.cc/.cpp/.C extension
We can do this in the shell, but it is actually easier in R.

From within the Pkgs directory
```
src = system("find . -maxdepth 2 -name src ", intern = TRUE)
csrc.files = lapply(src, list.files, pattern = "\\.(c|cc|cpp|C)$")
w = (sapply(csrc.files, length) > 0)
```
The elements of `src` for which `w` is FALSE correspond to packages
that have no .C/C++ code but FORTRAN, RATFOR, make files (Makefile, Makevars, Makevars.win),
autoconf .in files, markdown files:
```
table(gsub(".*\\.", "", unlist(lapply(src[!w], list.files))))
```
So we can ignore these.

```
src = src[w]
```


The next step is to add a symbolic link to the IRMakefile from the Rllvm package in each of the src/
directories of interest.
```
cmds = sprintf("ln -s ~/GitWorkingArea/Rllvm/inst/Make/IRMakefile %s", src)
invisible(lapply(cmds, system))
```
You can link to the IRMakefile in the installed  Rllvm package. I link to the one in my source for
the package as I am still developing this IRMakefile.


We want to treat the packages that use Rcpp separately. So for now, we'll focus on those that don't use Rcpp.
The function `usesRcpp()` checks the DESCRIPTION file to determine if Rcpp is mentioned in the
Imports, Depends or LinkingTo fields.
```
npkgs = dirname(src)
rcpp = sapply(npkgs, usesRcpp)
table(rcpp)
FALSE  TRUE
 1776  2158
```
So 55% do and 45% don't.
% Check again.  New run gives 1844 versus 1962
% 3901 elements in src/ and 95 have no C/C++ files.


We won't consider all package dependencies, but we will look at one-step dependencies we can
easily compute from a package's `DESCRIPTION` file.
```
lto = sapply(src, function(d) { d = read.dcf(file.path(d, "DESCRIPTION")); d[1, match("LinkingTo", colnames(d), 0)]})
names(lto) = src
lto = lto[sapply(lto, length) > 0]
lto2 = sapply(lto, function(x) { x = strsplit(x, ", *")[[1]]; trimws(gsub("\\([^)]+\\)", "", x))})
err = sapply(unique(unlist(lto2)), function(p)  system(sprintf("R CMD INSTALL %s", p)))
```


We loop over the non-Rcpp packages and run the configure script and then create the IR code:
```
cmds = sprintf("(cd %s; if test -f configure ; then R CMD ./configure; fi;  cd src; R CMD make -f IRMakefile all.bc CC=clang CXX=clang++)", npkgs[!rcpp])
e = sapply(cmds, function(x) try(system(x)))
```
This takes a while (more than an hour.)
Note, some of these will legitimately fail. 
Some require third-party libraries, and others are linking-to other packages which we have not yet installed.

The configure script for some packages may actually download and compile third-party libraries. This
is unfortunate, but we don't try to avoid this.


To analyze the native code, we can find all the all.bc files
```
bcFiles = system("find . -name all.bc", intern = TRUE)
```

We determine for which packages we failed to create the IR module with
```
bc.pkgs = dirname(dirname(bcFiles))
no = setdiff(npkgs[!rcpp], bc.pkgs)
```
There are 113 of the 1776 packages that failed, but not necessarily completely.

```
table(nir <- sapply(no, function(d) length(list.files(file.path(d, "src"),  pattern = "\\.bc$"))))
```
88 had no bc files, but the remainder had one or more which suggests some success but then
problems.  These are the ones we can possibly fix.
Some are due to LinkingTo other packages, which we could resolve and did above, but can go further.
Several others fail because OpenMP was not installed on the machine (i.e., the header file omp.h was not found).




We return to the packages that have a LinkingTo field and add the include directories for these
package dependencies to the compiler flags
```
cmds = sprintf("(cd %s/src; R CMD make -f IRMakefile all.bc CC=clang CXX=clang++ RCPP_FLAGS='%s')",
               names(lto2), 
			   sapply(lto2, function(d) paste(sprintf("-I%s/%s/include", "/home/duncantl/Rpackages", d), 
                    		                   collapse = " ")))
e2 = lapply(cmds, function(x) try(system(x, intern = TRUE)))
```
This brings us to 103 packages that don't have an `all.bc` file, so an additonal 10 packages were succesful.







To look at the distribution of the types of instructions
```
ins = lapply(bcFiles, function(f) sapply(unlist(getInstructions(readBitcode(f))), class))
table(unlist(ins))
```







#
+ tripack has one .c file (init.c) and the rest are .f files.


+ log4r - only one routine.  
```
m = parseIR("~/CRAN2/Pkgs/log4r/src/log4r.ir")
compReturnType(m$R_fmt_current_time)
```
  Note that this calls Rf_error() and then returns R_NilValue. The IR code knows Rf_error() never
  returns and so the R_NilValue is never a possible return value.



+ gte/ has only .C 
```
m = readBitcode("~/CRAN2/Pkgs/gte/src/all.bc")
funs = getDefinedRoutines(m, names = FALSE)
lapply(funs, function(f) { p = getParameters(f); p[sapply(p, onlyReadsMemory)]})
```

+ MSCMT
m = readBitcode("~/CRAN2/Pkgs/MSCMT/src/all.bc")
funs = getDefinedRoutines(m, names = FALSE)
funs = funs[sapply(funs, isDotCall)]
ty = compReturnType(m$DE)

types = lapply(funs, compReturnType)

