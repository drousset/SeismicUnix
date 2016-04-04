# Makefile for ...par/lib
SHELL = /bin/sh

# Adapt the flags in the following paragraph to your system
ROOT = /usr/local
OPTC = -O
# Search on the word "Comment" to complete adaptation

B = $(ROOT)/bin
I = $(ROOT)/include
L = $(ROOT)/lib
D = $I/cwp.h $I/par.h $L/libcwp.a

CFLAGS= $(OPTC) -I$I
LFLAGS= -L$L -lcwp


# Comment out if your make knows about C libs
#.c.a:
#	@$(CC) -c $(CFLAGS) $<
#	@ar rv $@ $*.o
#	@rm -f $*.o

# Comment out unless your ar needs ranlib and you don't have it
#.c.a:
#	@$(CC) -c $(CFLAGS) $<
#	@lorder $*.o | tsort | xargs ar qv $@
#	@rm -f $*.o
# Note: if you don't have xargs, alternate lorder line is:
#	@ar rv $@ `lorder $*.o | tsort`

.PRECIOUS:	 $(LIB)

LIB = $L/libpar.a

ARCH =				\
	$(LIB)(alloe.o)		\
	$(LIB)(askdoc.o)	\
	$(LIB)(atopkge.o)	\
	$(LIB)(errpkge.o)	\
	$(LIB)(getpars.o)	\
	$(LIB)(initargs.o)	\
	$(LIB)(syscalls.o)	\
	$(LIB)(subcalls.o)	\
	$(LIB)(selfdoc.o)

INSTALL:	$(LIB)
	@touch $@

$(ARCH)	: $D

$(LIB)	:	$(ARCH)
# Comment off following line if you don't need or don't have ranlib
#	ranlib $(LIB)

remake	:
	@rm -f $(LIB)
	@make

list	:
	ar tv $(LIB)

clean:
	rm -f junk* core a.out



# Testing routines (under development)
# The files Mktest.sh and Mkbench.sh are shell scripts in this directory.

test:
	Mktest.sh

bench:
	Mkbench.sh
	@touch $@
