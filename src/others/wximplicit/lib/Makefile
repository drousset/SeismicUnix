# A Makefile to make lib for wximplicit f.d. method

# JUST type 'make all'

IDIR= /usr/local/cwp/include
CFLAGS= -O -qlanglvl=ansi -I$(IDIR)
FFLAGS= -O 

all: compilf compilc arwxsub wxsublib
	touch $@

#compile all fortran subroutines
compilf:
	xlf -c $(FFLAGS) *.f
#compile all c subroutines
compilc:
	cc -c $(CFLAGS) *.c
#archive libwxsub.a
arwxsub:	
	ar rv libwxsub.a *.o
#ranlib
wxsublib:
	ranlib libwxsub.a 
list:
	ar tv libwxsub.a
clean:
	rm -f junk* core a.out *.o 
all:	compilf compilc arwxsub wxsublib clean
