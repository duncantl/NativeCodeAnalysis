ifeq ($(CLANG),)
CLANG=/usr/local/opt/llvm/bin/clang
endif

# -fno-discard-value-names 

RcppEg.ir: RcppEg.cc
	$(CLANG) -emit-llvm -S -std=gnu++11 -I"/Users/duncan/R-devel/build/include" -DNDEBUG   -I/usr/local/include  -I/Users/duncan/Rpackages/Rcpp/include -fPIC  -g -O2 -c RcppEg.cc -o RcppEg.ir