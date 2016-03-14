# Makefile for ...su/lib
SHELL = /bin/sh

# Adapt the flags in the following paragraph to your system
SU = /files2/data5/zli/lf
ROOT = $(SU)/cwp
OPTC = -xO3 -fast -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64

B = $(ROOT)/bin
I = $(ROOT)/include
L = $(ROOT)/lib

CFLAGS= $(OPTC) -I$I
LFLAGS= -L$L -lpar -lcwp -lm
TFLAGS = -g -I$I
CC = $(CC)

LIB = $L/libsu.a


ARCH =	$(LIB)(fgettra.o) $(LIB)(fgettr.o) $(LIB)(fputtr.o) \
	$(LIB)(hdrpkge.o) $(LIB)(valpkge.o)	\
	$(LIB)(conv_float.o) \
	$(LIB)(tabplot.o) \
	$(LIB)(fgethdr.o) $(LIB)(fputhdr.o) \
	$(LIB)(bhdrpkge.o) \
	$(LIB)(auxgettr.o) \
	$(LIB)(auxputtr.o) \
	$(LIB)(auxgethdr.o) \
	$(LIB)(auxputhdr.o) \
	$(LIB)(fgtra64.o) \
	$(LIB)(hdsearch.o)	\
	$(LIB)(idhdrs.o)



INSTALL:	$(LIB)
	@touch $@

$(LIB)	:	$(ARCH)
	ar rvu $(LIB)
	ranlib $(LIB)

remake	: 
	@rm -f $(LIB)
	@make ROOT=$(ROOT) OPTC=$(OPTC)

list	:
	ar tv $(LIB)

depend:
	makedepend -I$I *.c


# Testing routines
tests: tgettr tputtr tarray

tgettr:	fgettr.c
	$(CC) $(TFLAGS) -DTEST -DGETTR fgettr.c $(LIB) $(LFLAGS) -o tgettr
	@echo tgettr ready--running benchmark
	suplane offset=-100 | sugethw offset
	@echo convert offset to absolute value
	suplane offset=-100 | tgettr | sugethw offset

tputtr:	fputtr.c
	$(CC) $(TFLAGS) -DTEST -DPUTTR fputtr.c $(LIB) $(LFLAGS) -o tputtr
	@echo tputtr ready--running benchmark
	suplane offset=-100 | sugethw offset
	@echo convert offset to absolute value
	suplane offset=-100 | tputtr | sugethw offset

tarray:	arraypkge.c
	$(CC) $(TFLAGS) -DTEST arraypkge.c $(LIB) $(LFLAGS) -o tarray
	@echo arraypkge ready--running benchmark
	-tarray

clean:
	rm -f ato err io tarray tgetname tgetpar tgettr tputtr tstatfil \
		junk* core a.out

list:
	@ls *.c | pr -5 -t -l22 | p 

# Can comment out if your make knows about C libs
.c.a:
	@$(CC) -c $(CFLAGS) $<
	@ar rv $@ $*.o
	@rm -f $*.o

.PRECIOUS: $(LIB)

# DO NOT DELETE THIS LINE -- make depend depends on it.
