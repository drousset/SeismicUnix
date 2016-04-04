#makefile for vzest

include $(CWPROOT)/src/Makefile.config

LDLIBS =  -lm 

# Please do not leave space at the end
vzestf	        = ./vzestf
vzestsubf       = ./vzestsubf
vzestc	        = ./vzestc

OBJS =  $(vzestf).o \
        $(vzestsubf).o \
        $(vzestc).o 

$B/vzest: $(OBJS) 
	$(F77) $(OBJS) -o $B/vzest $(LDLIBS) 

$(vzestf).o: $(vzestf).f
	$(F77) $(FFLAGS) -c $(vzestf).f -o $(vzestf).o 

$(vzestsubf).o: $(vzestsubf).f
	$(F77) $(FFLAGS) -c $(vzestsubf).f -o $(vzestsubf).o 

$(vzestc).o: $(vzestc).c
	$(CC) $(CFLAGS) -c -I. $(vzestc).c -o $(vzestc).o 

remake:	
	@-rm ${OBJS} ./vzestf ./vseztsubf ./vzestc ; make
	${MAKE}
