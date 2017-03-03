
# Do not create dependencies while compiling
AUTOMAKE_OPTIONS = no-dependencies

CFLAGS           += $(DEBUG) $(OPTIM)

# Compiled programs to install
bin_PROGRAMS = sgftops saclst sacswap bbfswap unvis
sgftops_SOURCES = sgftops.c
saclst_SOURCES = saclst.c sac.h
sacswap_SOURCES = sacswap.c 
bbfswap_SOURCES = bbfswap.c
unvis_SOURCES = unvis.c

EXTRA_DIST = Makefile.utils \
	readsac.m writesac.m \
	padcat.m getsacdata.m \
	README_utils





