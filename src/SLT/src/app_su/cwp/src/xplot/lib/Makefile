
ARCH = $(shell uname -srm | sed 's/.[0-9]*-[^ ]*//' | sed 's/ /_/g' )

include $(SLTROOT)/src/Config/Makefile.config.$(ARCH)

CFLAGS += -DLIB_OBJ

LIB = libxplot.a

OBJS =                  \
        axesbox.o       \
        colormap.o      \
        dump2xgraph.o   \
        dump2xplot.o    \
        fg2bg.o         \
        garnish.o       \
        image.o         \
        legendbox.o     \
        rubberbox.o     \
        window.o        \
        xContour.o      \
	
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


