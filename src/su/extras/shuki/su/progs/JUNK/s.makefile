h09056
s 00002/00002/00234
d D 1.17 88/05/09 08:14:04 shuki 17 16
c 
e
s 00008/00001/00228
d D 1.16 88/05/08 05:54:08 shuki 16 15
c 
e
s 00016/00013/00213
d D 1.15 88/05/04 09:52:05 shuki 15 14
c 
e
s 00001/00000/00225
d D 1.14 88/05/03 09:18:26 shuki 14 13
c 
e
s 00002/00005/00223
d D 1.13 88/04/28 17:23:12 valer 13 12
c mainseqv.c instead of mainseqf.c
e
s 00002/00005/00226
d D 1.12 88/04/28 09:57:53 shemer 12 11
c 
e
s 00001/00001/00230
d D 1.11 88/04/28 09:36:46 valer 11 10
c 
e
s 00010/00001/00221
d D 1.10 88/04/28 09:25:09 valer 10 9
c adding sutrfft
e
s 00001/00000/00221
d D 1.9 88/04/28 08:37:47 shemer 9 8
c 
e
s 00001/00001/00220
d D 1.8 88/04/28 08:31:33 shemer 8 7
c 
e
s 00012/00001/00209
d D 1.7 88/04/27 17:08:17 shemer 7 6
c 
e
s 00144/00059/00066
d D 1.6 88/04/26 16:16:55 shuki 6 5
c /usr/local/obj for heterogeneous system
e
s 00000/00000/00125
d D 1.5 88/04/26 15:07:46 shuki 5 4
c 
e
s 00001/00001/00124
d D 1.4 88/04/26 10:14:20 shuki 4 3
c Added -lI77 to link sudmo on the sun 3's
e
s 00002/00002/00123
d D 1.3 88/04/20 10:30:56 shuki 3 2
c 
e
s 00004/00004/00121
d D 1.2 88/04/20 07:32:13 shuki 2 1
c 
e
s 00125/00000/00000
d D 1.1 88/04/14 13:52:24 shuki 1 0
c date and time created 88/04/14 13:52:24 by shuki
e
u
U
f e 0
t
T
I 1
#
# makefile for the su/progs directory
#

# DEFINE CONSTANTS
D 6
#B     =	/users/shuki/bin/
BIN    =	/usr/local/
E 6
I 6
BIN  = /usr/local/
L    = /usr/local/lib/
O    = /usr/local/obj/
E 6

D 6
#L     =	../lsu/libsu.a ../lCC/libCC.a
#L    =	-lsu -lCC
L     =	/usr/local/lib/libsu.a /usr/local/lib/libCC.a
E 6
I 6
#LSU    =	-lsu -lCC
LSU     =	$Llibsu.a $LlibCC.a
E 6

D 6
#LP    =	../lplot/libplot.a
E 6
#LP   =	-lplot
D 6
LP   =	/usr/local/lib/libcplot.a
E 6
I 6
LP   =	$Llibcplot.a
E 6

D 6
#LV    =	../lvec/libvec.a
E 6
#LV   =	-lvec
D 6
LV   =	/usr/local/lib/libvec.a
E 6
I 6
LV   =	$Llibvec.a
E 6

CFLAGS=	-g
FFLAGS=	-g

X =	$(BIN)sutpow $(BIN)suplot $(BIN)supr $(BIN)chart $(BIN)plot	$(BIN)susort $(BIN)sunmo	\
D 2
	$(BIN)sustack $(BIN)sudmo $(BIN)suahed $(BIN)sumax $(BIN)susub $(BIN)his $(BIN)suwind	\
E 2
I 2
	$(BIN)sustack $(BIN)sudmo $(BIN)suahed $(BIN)sumax $(BIN)susub $(BIN)suhis $(BIN)suwind	\
E 2
	$(BIN)suchw $(BIN)sushw $(BIN)suedit $(BIN)sucat $(BIN)subal $(BIN)suvmute $(BIN)suteep	\
D 7
	$(BIN)sutee $(BIN)subpus $(BIN)sugpow $(BIN)sufilt
E 7
I 7
D 10
	$(BIN)sutee $(BIN)subpus $(BIN)sugpow $(BIN)sufilt $(BIN)sunmm
E 10
I 10
D 15
	$(BIN)sutee $(BIN)subpus $(BIN)sugpow $(BIN)sufilt $(BIN)sunmm $(BIN)sutrfft
E 15
I 15
D 16
	$(BIN)sutee $(BIN)subpus $(BIN)sugpow $(BIN)sufilt $(BIN)sunmm $(BIN)sutrfft $(BIN)w4read
E 16
I 16
	$(BIN)sutee $(BIN)subpus $(BIN)sugpow $(BIN)sufilt $(BIN)sunmm $(BIN)sutrfft $(BIN)w4read \
D 17
	$(BIN)w4read
E 17
I 17
	$(BIN)w4write
E 17
E 16
E 15
E 10
E 7

# GENERAL MAKE ENTRIES
all	:$X
	@echo made

D 6
relink	:
	touch *.o
	make all

E 6
remake	:
D 6
	/bin/rm -f *.o
E 6
I 6
	-(cd $O; /bin/rm -f *.o)
E 6
	make all
	
clean cleanup:
D 6
	-/bin/rm -f *.o
E 6
I 6
	-(cd $O; /bin/rm -f *.o)
I 14
	-/bin/rm $X
E 14
E 6

# SHELL SCRIPTS (ALPHABETIC)

$(BIN)chart chart	:chart.csh
	cp chart.csh $(BIN)chart
	chmod 775 $(BIN)chart

$(BIN)plot plot	:plot.csh
	cp plot.csh $(BIN)plot
	chmod 775 $(BIN)plot

D 2
$(BIN)his his	:his.csh
	cp his.csh $(BIN)his
	chmod 775 $(BIN)his
E 2
I 2
$(BIN)suhis suhis	:suhis.csh
	cp suhis.csh $(BIN)suhis
	chmod 775 $(BIN)suhis
E 2


# APPLICATION PROGRAMS (ALPHABETIC)

D 6
$(BIN)suahed suahed: suahed.o $L
	cc $(CFLAGS) suahed.o $L -o $(BIN)suahed
E 6
I 6
$(BIN)suahed suahed: $Osuahed.o $(LSU)
	cc $(CFLAGS) $Osuahed.o $(LSU) -o $(BIN)suahed
$Osuahed.o	:suahed.c
	cc $(CFLAGS) -c suahed.c
	mv suahed.o $O
E 6

D 6
$(BIN)subal subal: mainseq.o bal.o $L $(LV)
	cc $(CFLAGS) mainseq.o bal.o $L $(LV) -lm -o $(BIN)subal
E 6
I 6
$(BIN)subal subal: $Omainseq.o $Obal.o $(LSU) $(LV)
	cc $(CFLAGS) $Omainseq.o $Obal.o $(LSU) $(LV) -lm -o $(BIN)subal
$Obal.o	:bal.c
	cc $(CFLAGS) -c bal.c
	mv bal.o $O
E 6

D 3
$(BIN)subpus subpus: mainseq.o bpus.o $L $(LV)
	cc $(CFLAGS) mainseq.o bpus.o $L $(LV) -lm -o $(BIN)subpus
E 3
I 3
D 6
$(BIN)subpus subpus: subpus.o $L $(LV)
	cc $(CFLAGS) subpus.o $L $(LV) -lm -o $(BIN)subpus
E 6
I 6
$(BIN)subpus subpus: $Osubpus.o $(LSU) $(LV)
	cc $(CFLAGS) $Osubpus.o $(LSU) $(LV) -lm -o $(BIN)subpus
$Osubpus.o	:subpus.c
	cc $(CFLAGS) -c subpus.c
	mv subpus.o $O
E 6
E 3

D 6
$(BIN)sucat sucat:	sucat.o $L
	cc $(CFLAGS) sucat.o $L -o $(BIN)sucat
E 6
I 6
$(BIN)sucat sucat:	$Osucat.o $(LSU)
	cc $(CFLAGS) $Osucat.o $(LSU) -o $(BIN)sucat
$Osucat.o	:sucat.c
	cc $(CFLAGS) -c sucat.c
	mv sucat.o $O
E 6

D 6
$(BIN)suchw suchw: mainseq.o chw.o $L
	cc $(CFLAGS) mainseq.o chw.o $L -lm -o $(BIN)suchw
E 6
I 6
$(BIN)suchw suchw: $Omainseq.o $Ochw.o $(LSU)
	cc $(CFLAGS) $Omainseq.o $Ochw.o $(LSU) -lm -o $(BIN)suchw
$Ochw.o	:chw.c
	cc $(CFLAGS) -c chw.c
	mv chw.o $O
E 6

D 6
$(BIN)sudmo sudmo: sudmo.o getco.o putco.o zpad2.o trigs.o $L
D 4
	cc $(CFLAGS) sudmo.o getco.o putco.o zpad2.o trigs.o $L $(LV) -lF77 -lm -o $(BIN)sudmo
E 4
I 4
	cc $(CFLAGS) sudmo.o getco.o putco.o zpad2.o trigs.o $L $(LV) -lF77 -lI77 -lm -o $(BIN)sudmo
E 6
I 6
$(BIN)sudmo sudmo: $Osudmo.o $Ogetco.o $Oputco.o $Ozpad2.o $Otrigs.o $(LSU)
	cc $(CFLAGS) $Osudmo.o $Ogetco.o $Oputco.o $Ozpad2.o $Otrigs.o $(LSU) $(LV) -lF77 -lI77 -lm -o $(BIN)sudmo
$Osudmo.o	:sudmo.c
	cc $(CFLAGS) -c sudmo.c
	mv sudmo.o $O
$Ogetco.o	:getco.c
	cc $(CFLAGS) -c getco.c
	mv getco.o $O
$Oputco.o	:putco.c
	cc $(CFLAGS) -c putco.c
	mv putco.o $O
$Ozpad2.o	:zpad2.c
	cc $(CFLAGS) -c zpad2.c
	mv zpad2.o $O
$Otrigs.o	:trigs.c
	cc $(CFLAGS) -c trigs.c
	mv trigs.o $O
E 6
E 4

D 6
$(BIN)suedit suedit: suedit.o $L
	cc $(CFLAGS) suedit.o $L -lm -o $(BIN)suedit
E 6
I 6
$(BIN)suedit suedit: $Osuedit.o $(LSU)
	cc $(CFLAGS) $Osuedit.o $(LSU) -lm -o $(BIN)suedit
$Osuedit.o	:suedit.c
	cc $(CFLAGS) -c suedit.c
	mv suedit.o $O
E 6

D 6
$(BIN)sufilt sufilt: mainseqv.o filt.o $L $(LV)
	cc $(CFLAGS) mainseqv.o filt.o $L $(LV) -lI77 -lF77 -lm -o $(BIN)sufilt
E 6
I 6
$(BIN)sufilt sufilt: $Omainseqv.o $Ofilt.o $(LSU) $(LV)
	cc $(CFLAGS) $Omainseqv.o $Ofilt.o $(LSU) $(LV) -lI77 -lF77 -lm -o $(BIN)sufilt
$Ofilt.o	:filt.c
	cc $(CFLAGS) -c filt.c
	mv filt.o $O
E 6

I 15
$(BIN)sugpow sugpow:$Omainseq.o $Ogpow.o $(LSU) $(LV)
	cc $(CFLAGS) $Omainseq.o $Ogpow.o $(LSU) $(LV) -lm -o $(BIN)sugpow
$Ogpow.o	:gpow.c
	cc $(CFLAGS) -c gpow.c
	mv gpow.o $O

E 15
D 6
$(BIN)sumax sumax: mainseqn.o max.o $L
	cc $(CFLAGS) mainseqn.o max.o $L $(LV) -lm -o $(BIN)sumax
E 6
I 6
$(BIN)sumax sumax: $Omainseqn.o $Omax.o $(LSU)
	cc $(CFLAGS) $Omainseqn.o $Omax.o $(LSU) $(LV) -lm -o $(BIN)sumax
$Omax.o	:max.c
	cc $(CFLAGS) -c max.c
	mv max.o $O
E 6

D 6
$(BIN)sunmo sunmo: mainseq.o nmo.o nmosubs.o $L
	cc $(CFLAGS) mainseq.o nmo.o nmosubs.o $L -lm -o $(BIN)sunmo
E 6
I 6
$(BIN)sunmo sunmo: $Omainseq.o $Onmo.o $Onmosubs.o $(LSU)
	cc $(CFLAGS) $Omainseq.o $Onmo.o $Onmosubs.o $(LSU) -lm -o $(BIN)sunmo
$Onmo.o	:nmo.c
	cc $(CFLAGS) -c nmo.c
	mv nmo.o $O
D 15
$Onmosubs.o	:nmosubs.f
	f77 $(FFLAGS) -c nmosubs.f
	mv nmosubs.o $O
E 15
I 7

D 8
$(BIN)sunmm sunmm: $Omainseq.o $Onmo1.o $Onmosubs.o $(LSU) $Ovelfile.o
E 8
I 8
D 12
$(BIN)sunmm sunmm: $Omainseq.o $Onmo1.o $Onmosubs.o $Ovelfile.o $(LSU)
E 8
	cc $(CFLAGS) $Omainseq.o $Onmo1.o $Onmosubs.o $Ovelfile.o $(LSU) -lm -o $(BIN)sunmm
E 12
I 12
$(BIN)sunmm sunmm: $Omainseq.o $Onmo1.o $Onmosubs.o  $(LSU)
	cc $(CFLAGS) $Omainseq.o $Onmo1.o $Onmosubs.o $(LSU) -lm -o $(BIN)sunmm
E 12
$Onmo1.o	:nmo1.c
	cc $(CFLAGS) -c nmo1.c
	mv nmo1.o $O
D 15
$Onmosubs.o	:nmosubs.f
	f77 $(FFLAGS) -c nmosubs.f
	mv nmosubs.o $O
E 15
D 12
$Ovelfile.o	:velfile.c
I 9
	cc $(CFLAGS) -c velfile.c
E 9
	mv velfile.o $O
E 12
E 7
E 6

D 6
$(BIN)suplot suplot:	mainseqn.o plot.o $(LP) $L
	cc $(CFLAGS) mainseqn.o plot.o $(LP) $L $(LV) -lm -o $(BIN)suplot
E 6
I 6
$(BIN)suplot suplot:	$Omainseqn.o $Oplot.o $(LP) $(LSU)
	cc $(CFLAGS) $Omainseqn.o $Oplot.o $(LP) $(LSU) $(LV) -lm -o $(BIN)suplot
$Oplot.o	:plot.c
	cc $(CFLAGS) -c plot.c
	mv plot.o $O
E 6

D 6
$(BIN)supr supr:	mainseqn.o pr.o $L
	cc $(CFLAGS) mainseqn.o pr.o $L -o $(BIN)supr
E 6
I 6
$(BIN)supr supr:	$Omainseqn.o $Opr.o $(LSU)
	cc $(CFLAGS) $Omainseqn.o $Opr.o $(LSU) -o $(BIN)supr
$Opr.o	:pr.c
	cc $(CFLAGS) -c pr.c
	mv pr.o $O
E 6

D 6
$(BIN)sushw sushw: mainseq.o shw.o $L
	cc $(CFLAGS) mainseq.o shw.o $L -lm -o $(BIN)sushw
E 6
I 6
$(BIN)sushw sushw: $Omainseq.o $Oshw.o $(LSU)
	cc $(CFLAGS) $Omainseq.o $Oshw.o $(LSU) -lm -o $(BIN)sushw
$Oshw.o	:shw.c
	cc $(CFLAGS) -c shw.c
	mv shw.o $O
E 6

D 6
$(BIN)susort susort: susort.o $L
	cc $(CFLAGS) susort.o $L -o $(BIN)susort
E 6
I 6
$(BIN)susort susort: $Osusort.o $(LSU)
	cc $(CFLAGS) $Osusort.o $(LSU) -o $(BIN)susort
$Osusort.o	:susort.c
	cc $(CFLAGS) -c susort.c
	mv susort.o $O
E 6

D 6
$(BIN)sustack sustack: sustack.o $L $(LV)
	cc $(CFLAGS) sustack.o $L $(LV) -lm -o $(BIN)sustack
E 6
I 6
$(BIN)sustack sustack: $Osustack.o $(LSU) $(LV)
	cc $(CFLAGS) $Osustack.o $(LSU) $(LV) -lm -o $(BIN)sustack
$Osustack.o	:sustack.c
	cc $(CFLAGS) -c sustack.c
	mv sustack.o $O
E 6

D 6
$(BIN)susub susub: susub.o $L $(LV)
	cc $(CFLAGS) susub.o $L $(LV) -lm -o $(BIN)susub
E 6
I 6
$(BIN)susub susub: $Osusub.o $(LSU) $(LV)
	cc $(CFLAGS) $Osusub.o $(LSU) $(LV) -lm -o $(BIN)susub
$Osusub.o	:susub.c
	cc $(CFLAGS) -c susub.c
	mv susub.o $O
E 6

D 6
$(BIN)sutee sutee: sutee.o $L
	cc $(CFLAGS) sutee.o $L -lm -o $(BIN)sutee
E 6
I 6
$(BIN)sutee sutee: $Osutee.o $(LSU)
	cc $(CFLAGS) $Osutee.o $(LSU) -lm -o $(BIN)sutee
$Osutee.o	:sutee.c
	cc $(CFLAGS) -c sutee.c
	mv sutee.o $O
E 6

D 6
$(BIN)suteep suteep: suteep.o $L
	cc $(CFLAGS) suteep.o $L -lm -o $(BIN)suteep
E 6
I 6
$(BIN)suteep suteep: $Osuteep.o $(LSU)
	cc $(CFLAGS) $Osuteep.o $(LSU) -lm -o $(BIN)suteep
$Osuteep.o	:suteep.c
	cc $(CFLAGS) -c suteep.c
	mv suteep.o $O
E 6

D 6
$(BIN)sutpow sutpow: mainseq.o tpow.o $L $(LV)
	cc $(CFLAGS) mainseq.o tpow.o $L $(LV) -lm -o $(BIN)sutpow
E 6
I 6
$(BIN)sutpow sutpow: $Omainseq.o $Otpow.o $(LSU) $(LV)
	cc $(CFLAGS) $Omainseq.o $Otpow.o $(LSU) $(LV) -lm -o $(BIN)sutpow
$Otpow.o	:tpow.c
	cc $(CFLAGS) -c tpow.c
	mv tpow.o $O
E 6

I 10
D 13
$(BIN)sutrfft sutrfft: $Omainseqf.o $Otrfft.o $(LSU) $(LV)
D 11
	cc $(CFLAGS) $Omainseqf.o $Otrfft.o $L $(LV) -lI77 -lF77 -lm -o $(BIN)sutrfft
E 11
I 11
	cc $(CFLAGS) $Omainseqf.o $Otrfft.o $(LSU) $(LV) -lI77 -lF77 -lm -o $(BIN)sutrfft
E 13
I 13
$(BIN)sutrfft sutrfft: $Omainseqv.o $Otrfft.o $(LSU) $(LV)
	cc $(CFLAGS) $Omainseqv.o $Otrfft.o $(LSU) $(LV) -lI77 -lF77 -lm -o $(BIN)sutrfft
E 13
E 11
$Otrfft.o       :trfft.c
	cc $(CFLAGS) -c trfft.c
	mv trfft.o $O

E 10
D 6
$(BIN)suvmute suvmute: mainseq.o vmute.o $L
	cc $(CFLAGS) mainseq.o vmute.o $L -lm -o $(BIN)suvmute
E 6
I 6
$(BIN)suvmute suvmute: $Omainseq.o $Ovmute.o $(LSU)
	cc $(CFLAGS) $Omainseq.o $Ovmute.o $(LSU) -lm -o $(BIN)suvmute
$Ovmute.o	:vmute.c
	cc $(CFLAGS) -c vmute.c
	mv vmute.o $O
E 6

I 15
$(BIN)w4read w4read: $Ow4read.o $(LSU)
	cc $(CFLAGS) $Ow4read.o $(LSU) -o $(BIN)w4read
$Ow4read.o	:w4read.c
	cc $(CFLAGS) -c w4read.c
	mv w4read.o $O

E 15
D 6
$(BIN)suwind suwind: mainseq.o wind.o $L
	cc $(CFLAGS) mainseq.o wind.o $L -o $(BIN)suwind
E 6
I 6
$(BIN)suwind suwind: $Omainseq.o $Owind.o $(LSU)
	cc $(CFLAGS) $Omainseq.o $Owind.o $(LSU) -o $(BIN)suwind
$Owind.o	:wind.c
	cc $(CFLAGS) -c wind.c
	mv wind.o $O
E 6
 
I 16
$(BIN)w4write w4write: $Ow4write.o $(LSU)
D 17
	cc $(CFLAGS) $Ow4write.o $(LSU) -o $(BIN)w4write
E 17
I 17
	cc $(CFLAGS) $Ow4write.o $(LSU) -lm -o $(BIN)w4write
E 17
$Ow4write.o	:w4write.c
	cc $(CFLAGS) -c w4write.c
	mv w4write.o $O

E 16
D 6
$(BIN)sugpow sugpow:mainseq.o gpow.o $L $(LV)
	cc $(CFLAGS) mainseq.o gpow.o $L $(LV) -lm -o $(BIN)sugpow
E 6
I 6
D 15
$(BIN)sugpow sugpow:$Omainseq.o $Ogpow.o $(LSU) $(LV)
	cc $(CFLAGS) $Omainseq.o $Ogpow.o $(LSU) $(LV) -lm -o $(BIN)sugpow
$Ogpow.o	:gpow.c
	cc $(CFLAGS) -c gpow.c
	mv gpow.o $O

E 15
$Omainseqv.o	:mainseqv.c
	cc $(CFLAGS) -c mainseqv.c
	mv mainseqv.o $O
I 10
D 13
$Omainseqf.o	:mainseqf.c
	cc $(CFLAGS) -c mainseqf.c
	mv mainseqf.o $O
E 13
E 10
$Omainseq.o	:mainseq.c
	cc $(CFLAGS) -c mainseq.c
	mv mainseq.o $O
$Omainseqn.o	:mainseqn.c
	cc $(CFLAGS) -c mainseqn.c
	mv mainseqn.o $O
I 15
$Onmosubs.o	:nmosubs.f
	f77 $(FFLAGS) -c nmosubs.f
	mv nmosubs.o $O
E 15
E 6
E 1
