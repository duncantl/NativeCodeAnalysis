R_HEADERS=/usr/include/R
CFLAGS=-I$(R_HEADERS)
LLFLAGS=-fno-discard-value-names -S -emit-llvm 
CC=clang

TARGET=return42.ll

default: CFLAGS += -O1
default: clean $(TARGET)

opt0: clean $(TARGET)

%.ll: %.c
	$(CC) -o $@ $< $(CFLAGS) $(LLFLAGS)

.PHONY: clean

clean:
	rm -f $(TARGET)
