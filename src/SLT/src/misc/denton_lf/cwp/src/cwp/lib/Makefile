# Makefile for ...cwp/lib
SHELL = /bin/sh

# Adapt the flags in the following paragraph to your system
SU = /files2/data5/zli/lf
ROOT = $(SU)/cwp
OPTC = -xO3 -fast
# Search on the word "Comment" to complete adaptation

B = $(ROOT)/bin
I = $(ROOT)/include
L = $(ROOT)/lib
# Safest to use next line, but ...
#D = $I/cwp.h
D = 
CC = $(CC)

CFLAGS= $(OPTC) -I$I
FFLAGS= $(OPTC) -I$I

LIB = $L/libcwp.a

ARCH =				\
	$(LIB)(abel.o)	\
	$(LIB)(airya.o)	\
	$(LIB)(airyb.o)	\
	$(LIB)(alloc.o)	\
	$(LIB)(antialias.o)	\
	$(LIB)(bf.o)	\
	$(LIB)(bm.o)	\
	$(LIB)(cakima.o)	\
	$(LIB)(cmonot.o)	\
	$(LIB)(complex.o)	\
	$(LIB)(conv.o)	\
	$(LIB)(cpusec.o)	\
	$(LIB)(cputime.o)	\
	$(LIB)(csplin.o)	\
	$(LIB)(dblas.o)	\
	$(LIB)(dge.o)	\
	$(LIB)(dsinc.o)	\
	$(LIB)(filestat.o)	\
	$(LIB)(frannor.o)	\
	$(LIB)(franuni.o)	\
	$(LIB)(fsinc.o)	\
	$(LIB)(hankel.o)	\
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
	$(LIB)(ress8c.o)	\
	$(LIB)(ress8r.o)	\
	$(LIB)(rfwtva.o)	\
	$(LIB)(rfwtvaint.o)	\
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
	$(LIB)(walltime.o)	\
	$(LIB)(xcor.o)	\
	$(LIB)(xindex.o)	\
	$(LIB)(yclip.o)	\
	$(LIB)(yxtoxy.o)

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
.f.a:
	@$(FC) -c $(FFLAGS) $<
	@ar rv $@ $*.o
	@rm -f $*.o



.PRECIOUS:	 $(LIB)
