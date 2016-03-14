
ARCH = $(shell uname -srm | sed 's/.[0-9]*-[^ ]*//' | sed 's/ /_/g' )

include $(SLTROOT)/src/Config/Makefile.config.$(ARCH)

CFLAGS += -DLIB_OBJ

LIB = libsu.a

OBJS =                \
        fgettra.o     \
        fgettr.o      \
        fputtr.o      \
        hdrpkge.o     \
        valpkge.o     \
        conv_float.o  \
        tabplot.o     \
        fgethdr.o     \
        fputhdr.o     \
        bhdrpkge.o    \
        auxgettr.o    \
        auxputtr.o    \
        auxgethdr.o   \
        auxputhdr.o   \
        fgtra64.o     \
        hdsearch.o    \
        idhdrs.o


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

