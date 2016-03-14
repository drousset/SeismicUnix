# Makefile for ...par/lib
SHELL = /bin/sh

# Adapt the flags in the following paragraph to your system
SU = /pad/data5/zli/lf
ROOT = $(SU)/cwp
OPTC = -xO3 -fast -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64
# Search on the word "Comment" to complete adaptation

B = $(ROOT)/bin
I = $(ROOT)/include
L = $(ROOT)/lib
D = $I/cwp.h $I/par.h $L/libcwp.a
CC = $(CC)

CFLAGS= $(OPTC) -I$I -DSCRATCHDIR=\"/scratch\"
LFLAGS= -L$L -lcwp

LIB = $L/libpar.a

ARCH =				\
	$(LIB)(askdoc.o)	\
	$(LIB)(atopkge.o)	\
	$(LIB)(docpkge.o)	\
	$(LIB)(ealloc.o)	\
	$(LIB)(errpkge.o)	\
	$(LIB)(getpars.o)	\
	$(LIB)(initargs.o)	\
	$(LIB)(syscalls.o)	\
	$(LIB)(subcalls.o)	\
	$(LIB)(gtparsf.o)	\
	$(LIB)(selfdoc.o)

INSTALL:	$(LIB)
	@touch $@

$(ARCH)	: $D

$(LIB)	: $(ARCH)
	ranlib $(LIB)

remake	:
	@rm -f $(LIB)
	@make ROOT=$(ROOT) OPTC=$(OPTC)

list	:
	ar tv $(LIB)

clean:
	rm -f junk* core a.out

# Can comment out if your make knows about C libs
.c.a:
	@$(CC) -c $(CFLAGS) $<
	@ar rv $@ $*.o
	@rm -f $*.o

.PRECIOUS:	 $(LIB)
