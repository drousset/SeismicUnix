# Makefile for ...cwp/lib
SHELL = /bin/sh

# Adapt the flags in the following paragraph to your system
ROOT = /usr/local
OPTC = -O
# Search on the word "Comment" to complete adaptation

B = $(ROOT)/bin
I = $(ROOT)/include
L = $(ROOT)/lib
D = $I/cwp.h

CFLAGS= $(OPTC) -I$I
LFLAGS= -L$L


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

LIB = $L/libcwp.a

ARCH =				\
	$(LIB)(airya.o)	\
	$(LIB)(airyb.o)	\
	$(LIB)(alloc.o)	\
	$(LIB)(bf.o)	\
	$(LIB)(bm.o)	\
	$(LIB)(cakima.o)	\
	$(LIB)(cmonot.o)	\
	$(LIB)(complex.o)	\
	$(LIB)(conv.o)	\
	$(LIB)(cpusec.o)	\
	$(LIB)(csplin.o)	\
	$(LIB)(dblas.o)	\
	$(LIB)(dge.o)	\
	$(LIB)(dsinc.o)	\
	$(LIB)(filestat.o)	\
	$(LIB)(frannor.o)	\
	$(LIB)(franuni.o)	\
	$(LIB)(fsinc.o)	\
	$(LIB)(hilbert.o)	\
	$(LIB)(hpsort.o)	\
	$(LIB)(intcub.o)	\
	$(LIB)(intl2b.o)	\
	$(LIB)(intlin.o)	\
	$(LIB)(ints8c.o)	\
	$(LIB)(ints8r.o)	\
	$(LIB)(intt8c.o)	\
	$(LIB)(intt8r.o)	\
	$(LIB)(mkdiff.o)	\
	$(LIB)(mkhdiff.o)	\
	$(LIB)(mksinc.o)	\
	$(LIB)(npfa.o)	\
	$(LIB)(npfao.o)	\
	$(LIB)(npfar.o)	\
	$(LIB)(npfaro.o)	\
	$(LIB)(pfa2cc.o)	\
	$(LIB)(pfa2cr.o)	\
	$(LIB)(pfa2rc.o)	\
	$(LIB)(pfacc.o)	\
	$(LIB)(pfacr.o)	\
	$(LIB)(pfamcc.o)	\
	$(LIB)(pfarc.o)	\
	$(LIB)(pp1d.o)	\
	$(LIB)(pplot1.o)	\
	$(LIB)(qkisort.o)	\
	$(LIB)(qksort.o)	\
	$(LIB)(quest.o)	\
	$(LIB)(questinit.o)	\
	$(LIB)(questupdate.o)	\
	$(LIB)(rfwtva.o)	\
	$(LIB)(sblas.o)	\
	$(LIB)(scaxis.o)	\
	$(LIB)(sge.o)	\
	$(LIB)(shfs8r.o)	\
	$(LIB)(sqr.o)	\
	$(LIB)(stoepd.o)	\
	$(LIB)(stoepf.o)	\
	$(LIB)(tridif.o)	\
	$(LIB)(vanded.o)	\
	$(LIB)(vandef.o)	\
	$(LIB)(wallsec.o)	\
	$(LIB)(xcor.o)	\
	$(LIB)(xindex.o)	\
	$(LIB)(yclip.o)	\
	$(LIB)(yxtoxy.o)

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
