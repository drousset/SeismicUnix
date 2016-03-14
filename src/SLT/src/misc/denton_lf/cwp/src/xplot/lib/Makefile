# Makefile for ...xplot/lib
SHELL = /bin/sh

# Adapt the flags in the following paragraph to your system
SU = /files2/data5/zli/lf
ROOT = $(SU)/cwp
OPTC = -xO3 -fast -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64

# Search on the word "Comment" to complete adaptation

B = $(ROOT)/bin
I = $(ROOT)/include
L = $(ROOT)/lib
# Safest to use next line, but ...
#D = $I/xplot.h
D = 
CC = $(CC)

CFLAGS= $(OPTC) -I$I
LFLAGS= -L$L

LIB = $L/libxplot.a

ARCH =				\
	$(LIB)(axesbox.o)	\
	$(LIB)(colormap.o)	\
	$(LIB)(dump2xgraph.o)	\
	$(LIB)(dump2xplot.o)	\
	$(LIB)(fg2bg.o)	\
	$(LIB)(image.o)	\
	$(LIB)(legendbox.o) \
	$(LIB)(xContour.o)  \
	$(LIB)(rubberbox.o) \
	$(LIB)(window.o)    \
	$(LIB)(garnish.o)

INSTALL:	$(LIB)
	@touch $@

$(ARCH)	: $D

$(LIB)	:	$(ARCH)
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
