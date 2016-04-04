# Makefile for ...dsu/lib

include $(CWPROOT)/src/Makefile.config

#
# Modifications for dsu
#
LDSU= .
DSUFLAGS= -I../include  -I$I -I$(PVM_ROOT)/include $(OPTC) $(ENDIANFLAG)
CFLAGS = $(DSUFLAGS)
#
# End Modifications for dsu
#

LFLAGS= -L$L -lpar -lcwp -lm
TFLAGS = -g -I$I

LIB = $(LDSU)/libdsu.a

ARCH =	$(LIB)(dsufgettr.o)	\
	$(LIB)(dsufputtr.o)	\
	$(LIB)(dsulib.o)	\
	$(LIB)(errpkge.o)	\
	$(LIB)(getpars.o)


INSTALL:	$(LIB)
	@touch $@

$(LIB)	:	$(ARCH)
	$(AR) $(ARFLAGS) $(LIB)
	$(RANLIB) $(LIB)


remake	:
	@-rm -f $(LIB)
	@$(MAKE)

list	:
	$(AR) tv $(LIB)



clean:
	rm -f junk* core a.out

.PRECIOUS: $(LIB)
