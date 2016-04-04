#makefile for vplusz

include $(CWPROOT)/src/Makefile.config

LDLIBS = -lm 

S = .

vpluszf = $S/vpluszf
vpluszc = $S/vpluszc

OBJS = $(vpluszf).o \
       $(vpluszc).o

$B/vpluszf: $(OBJS) 
	$(F77) $(FFLAGS) $(OBJS) -o $B/vplusz $(LDLIBS) 

$(vpluszf).o: $(vpluszf).f
	$(F77) $(FFLAGS)  -c $(vpluszf).f -o $(vpluszf).o 

$(vpluszc).o: $(vpluszc).c
	$(CC) $(CFLAGS) -c $(vpluszc).c -o $(vpluszc).o

clean:
	/bin/rm *.o

remake:
	/bin/rm *.o
	$(MAKE)
