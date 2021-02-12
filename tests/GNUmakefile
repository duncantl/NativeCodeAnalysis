ifndef CC
CC=clang
endif

ifndef R_INCLUDE
R_INCLUDE=-I$(shell R RHOME)/include
endif

ifndef OPT_LEVEL
OPT_LEVEL=-O2
endif

%.ir: %.c
	$(CC) -S -emit-llvm -fno-discard-value-names $< -o $@ $(CPPFLAGS) $(CFLAGS) $(OPT_LEVEL) $(R_INCLUDE)

