ifndef CC
CC=clang
endif

ifndef R_INCLUDE
R_INCLUDE=-I$(shell R RHOME)/include
endif

%.ir: %.c
	$(CC) -S -emit-llvm -fno-discard-value-names $< -o $@ $(CPPFLAGS) $(CFLAGS) $(R_INCLUDE)

