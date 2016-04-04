#
# makefile for the su/lvec directory
#

CFLAGS = -g
FFLAGS = -g

#L     =	./libvec.a
LDIR	= /usr/local/lib/
L = $(LDIR)libvec.a
D = /usr/local/obj/

O =	$Dlibvec.o \
	$Dcefft.o $Dcvfft.o $Drefft.o $Drvfft.o $Dconvv.o $Drowcc.o	\
	$Ddotpow.o $Ddoagc.o $Ddoclip.o $Ddoepow.o $Ddogpow.o \
	$Ddoqbal.o $Ddopbal.o $Ddoscale.o $Ddotrap.o \
	$Dquant.o $Dbalclip.o $Dcopyabs.o $Dbfill.o	\
	$Doperhan.o

$Dlibvec.o	: libvec.c
	cc $(CFLAGS) -c libvec.c
	mv libvec.o $D

$Dcefft.o
$Dcefft.o
	cc $(CFLAGS) -c cefft.o
$Dcefft.o

$Dcvfft.o
$Dcvfft.o
	cc $(CFLAGS) -c cvfft.o
$Dcvfft.o

$Drefft.o
$Drefft.o
	cc $(CFLAGS) -c refft.o
$Drefft.o

$Drvfft.o
$Drvfft.o
	cc $(CFLAGS) -c rvfft.o
$Drvfft.o

$Dconvv.o
$Dconvv.f
	cc $(CFLAGS) -c convv.o
$Dconvv.f

$Drowcc.o
$Drowcc.o
	cc $(CFLAGS) -c rowcc.f
$Drowcc.o

$Ddotpow.o
$Ddotpow.o
	cc $(CFLAGS) -c dotpow.o
$Ddotpow.o

$Ddoagc.o
$Ddoagc.o
	cc $(CFLAGS) -c doagc.o
$Ddoagc.o

$Ddoclip.o
$Ddoclip.o
	cc $(CFLAGS) -c doclip.o
$Ddoclip.o

$Ddoepow.o
$Ddoepow.o
	cc $(CFLAGS) -c doepow.o
$Ddoepow.o

$Ddogpow.o
$Ddogpow.o
	cc $(CFLAGS) -c dogpow.o
$Ddogpow.o

$Ddoqbal.o
$Ddoqbal.o
	cc $(CFLAGS) -c doqbal.o
$Ddoqbal.o

$Ddopbal.o
$Ddopbal.o
	cc $(CFLAGS) -c dopbal.o
$Ddopbal.o

$Ddoscale.o
$Ddoscale.o
	cc $(CFLAGS) -c doscale.o

$Ddotrap.o
$Ddotrap.o
	cc $(CFLAGS) -c dotrap.o
$Ddotrap.o

$Dquant.o
$Dquant.o
	cc $(CFLAGS) -c quant.o
$Dquant.o

$Dbalclip.o
$Dbalclip.o
	cc $(CFLAGS) -c balclip.c
$Dbalclip.o

$Dcopyabs.o
$Dcopyabs.o
	cc $(CFLAGS) -c copyabs.c
$Dcopyabs.o

$Dbfill.o
$Dbfill.o
	cc $(CFLAGS) -c bfill.c
$Dbfill.o

$Doperhan.o
$Doperhan.o
	f77 $(FFLAGS) -c operhan.f
$Doperhan.o

#.SUFFIXES: .c .r .f .o
#.c.o:	;cc $(CFLAGS) -c $*.c;
#.f.o:	;f77 $(FFLAGS) -c $*.f;
#.r.f:	;ratfor $*.r > $*.f

$L lvec : $O
	ar rv $L $O
	ranlib $L

list	:
	ar tv $L

remake	:
	rm -f $L
	make $L

operhan.o	:operhan.f
	f77 -c $(CFLAGS) operhan.f
rowcc.o	:rowcc.f
	f77 -c $(CFLAGS) rowcc.f
convv.o	:convv.f
	f77 -c $(CFLAGS) convv.f

clean	:
	-/bin/rm -f *.o
	-/bin/rm -f *.a
