
ARCH = $(shell uname -srm | sed 's/.[0-9]*-[^ ]*//' | sed 's/ /_/g' )

include $(SLTROOT)/src/Config/Makefile.config.$(ARCH)

LIB = librayt3d.a

OBJS =                  \
      dv2.o             \
      ov2int.o          \
      ray3d.o           \
      raytime.o         \
      rt3dp.o           \
      trans.o           \
      vel3d.o           \

#      tthorizon.o       \
	
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

