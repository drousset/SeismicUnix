# Makefile for  CWPROOT/man

include $(CWPROOT)/src/Makefile.config

M3 = $M/man3

MTARGET = $$(@F).man

MAN3 =				\
	$(M3)/alloc.3		\
	$(M3)/abel.3		\
	$(M3)/airy.3		\
	$(M3)/antialias.3	\
	$(M3)/bf.3

LINKS31 =	$(M3)/alloc1.3	$(M3)/alloc1complex.3	\
	$(M3)/alloc1double.3	$(M3)/alloc1float.3	\
	$(M3)/alloc1int.3	$(M3)/alloc2.3		\
	$(M3)/alloc2complex.3	$(M3)/alloc2double.3	\
	$(M3)/alloc2float.3	$(M3)/alloc2int.3	\
	$(M3)/alloc3.3	$(M3)/alloc3complex.3		\
	$(M3)/alloc3double.3	$(M3)/alloc3float.3	\
	$(M3)/alloc3int.3	$(M3)/alloc4.3		\
	$(M3)/free1.3					\
	$(M3)/free1complex.3	$(M3)/free1double.3	\
	$(M3)/free1float.3	$(M3)/free1int.3 $(M3)/free2.3	\
	$(M3)/free2complex.3	$(M3)/free2double.3	\
	$(M3)/free2float.3	\
	$(M3)/free2int.3 $(M3)/free3.3	$(M3)/free3complex.3	\
	$(M3)/free3double.3	$(M3)/free3float.3	\
	$(M3)/free3int.3	\
	$(M3)/free4.3 $(M3)/realloc1.3 \
	$(M3)/realloc1complex.3	\
	$(M3)/realloc1double.3	$(M3)/realloc1float.3	\
	$(M3)/realloc1int.3


LINKS32	=	$(M3)/abelalloc.3 $(M3)/abelfree.3

LINKS33 =	$(M3)/airya.3 $(M3)/airyap.3	\
		$(M3)/airyb.3 $(M3)/airybp.3

LINKS34 =	$(M3)/bfdesign.3 $(M3)/bfhighpass.3	\
		$(M3)/bflowpass.3


INSTALL:	$(MAN3) $(LINKS31) $(LINKS32) $(LINKS33) $(LINKS34) $(M3)
	@touch $@

$(MAN3):	$(MTARGET)
	@-rm -f $@
	@cp $? $@
	@echo $(@F) installed in $(M3)

$(LINKS31):
	@-rm -f $@ 
	@-ln -s alloc.3 $@
	@echo $(@F) installed in $(M3)


$(LINKS32):
	@-rm -f $@ 
	@-ln -s abel.3 $@
	@echo $(@F) installed in $(M3)

$(LINKS33):
	@-rm -f $@ 
	@-ln -s airy.3 $@
	@echo $(@F) installed in $(M3)

$(LINKS34):
	@-rm -f $@ 
	@-ln -s bf.3 $@
	@echo $(@F) installed in $(M3)

$(M3)	:
	@-mkdir $(M3)

remake:
	@-touch *.man
	@-rm -f $(MAN3) $(LINKS31)
	@$(MAKE)
