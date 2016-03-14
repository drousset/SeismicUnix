
ARCH = $(shell uname -srm | sed 's/.[0-9]*-[^ ]*//' | sed 's/ /_/g' )

include $(SLTROOT)/src/Config/Makefile.config.$(ARCH)

CFLAGS += -DLIB_OBJ

LIB = libcwp.a

OBJS =                  \
        abel.o          \
        airya.o         \
        airyb.o         \
        alloc.o         \
        antialias.o     \
        bf.o            \
        bm.o            \
        cakima.o        \
        cmonot.o        \
        complex.o       \
        conv.o          \
        cpusec.o        \
        cputime.o       \
        csplin.o        \
        dblas.o         \
        dge.o           \
        dsinc.o         \
        filestat.o      \
        frannor.o       \
        franuni.o       \
        fsinc.o         \
        hankel.o        \
        hilbert.o       \
        hpsort.o        \
        intcub.o        \
        intl2b.o        \
        intlin.o        \
        ints8c.o        \
        ints8r.o        \
        intt8c.o        \
        intt8r.o        \
        mkdiff.o        \
        mkhdiff.o       \
        mksinc.o        \
        npfa.o          \
        npfao.o         \
        npfar.o         \
        npfaro.o        \
        pfa2cc.o        \
        pfa2cr.o        \
        pfa2rc.o        \
        pfacc.o         \
        pfacr.o         \
        pfamcc.o        \
        pfarc.o         \
        pp1d.o          \
        pplot1.o        \
        qkisort.o       \
        qksort.o        \
        quest.o         \
        questinit.o     \
        questupdate.o   \
        ress8c.o        \
        ress8r.o        \
        rfwtva.o        \
        rfwtvaint.o     \
        sblas.o         \
        scaxis.o        \
        sge.o           \
        shfs8r.o        \
        sqr.o           \
        stoepd.o        \
        stoepf.o        \
        tridif.o        \
        vanded.o        \
        vandef.o        \
        wallsec.o       \
        walltime.o      \
        xcor.o          \
        xindex.o        \
        yclip.o         \
        yxtoxy.o

	
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


