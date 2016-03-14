
ARCH = $(shell uname -srm | sed 's/.[0-9]*-[^ ]*//' | sed 's/ /_/g' )

include $(SLTROOT)/src/Config/Makefile.config.$(ARCH)

CFLAGS += -DLIB_OBJ

LIB = libumisc.a

OBJS =                  \
        agc.o           \
        bilint.o        \
        cancel.o        \
        cfft.o          \
        cfft2d.o        \
        ctris.o         \
        diskio.o        \
        dskmpx.o        \
        dummyopen.o     \
        four1.o         \
        ftncls.o        \
        gettime.o       \
        ieeefinite.o    \
        intp2d.o        \
        intsln.o        \
        lens.o          \
        linefit.o       \
        linint.o        \
        msgsc.o         \
        pfft2d.o        \
        pol3coe.o       \
        polyint.o       \
        qcksrt.o        \
        qcksti.o        \
        qkisort.o       \
        radix.o         \
        rfft.o          \
        smooth.o        \
        spint.o         \
        tp.o            \
        v2tris.o        \
        vctris.o        \
        vrfft.o         \
        zeroal.o        \

	
all: $(LIB)
	@ if [ ! -d $(TOP)/$(ARCH)/lib ]; \
           then \
           mkdir -p $(TOP)/$(ARCH)/lib;\
        fi
	@echo "Installing $(LIB) in $(TOP)/$(ARCH)/lib"
	rm -f $(TOP)/$(ARCH)/lib/$(LIB)
	cp $(LIB) $(TOP)/$(ARCH)/lib/$(LIB)

	
install: $(LIB)
	@ if [ ! -d $(SLTROOT)/$(ARCH)/lib ]; \
           then \
           mkdir -p $(SLTROOT)/$(ARCH)/lib;\
        fi
	@echo "Installing $(LIB) in $(SLTROOT)/$(ARCH)/lib"
	rm -f $(SLTROOT)/$(ARCH)/lib/$(LIB)
	cp $(LIB) $(SLTROOT)/$(ARCH)/lib/$(LIB)


$(LIB) : $(OBJS)
	ar crv $(LIB) $(OBJS)
	
clean:
	rm -f *.o $(LIB)


