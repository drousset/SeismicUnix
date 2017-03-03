
# Do not create dependencies while compiling
AUTOMAKE_OPTIONS = no-dependencies

CFLAGS          += -O -Wall -pipe -g3

# Scripts to install
#bin_SCRIPTS      = sgftoeps.csh

# Compiled libraries to not install
noinst_LIBRARIES  = lib/libedit.a

LIBEDIT_DIR = libedit-2.6.9
LIBEDIT_TAR = $(LIBEDIT_DIR).tar.gz
LIBEDIT_OPT = --prefix=..
LIBEDIT_ENV = env CFLAGS="$(CFLAGS)" LDFLAGS="$(LDFLAGS)" INSTALL="../../config/install-sh"
CONFIG_H_IN = config.h.in

EXTRA_DIST  = License.BSD
lib_libedit_a_SOURCES = libedit-2.6.9.tar.gz $(CONFIG_H_IN)

all: lib/libedit.a

$(LIBEDIT_DIR):
	echo "Creating the Line Editing Library"
	gzip -dc $(LIBEDIT_TAR) | tar xvf -
	cp -f $(CONFIG_H_IN) $(LIBEDIT_DIR)

lib/libedit.a: $(LIBEDIT_DIR) 
	(cd $(LIBEDIT_DIR) && $(LIBEDIT_ENV) ./configure $(LIBEDIT_OPT) )
	(cd $(LIBEDIT_DIR) && $(MAKE) lib_a )
	(cd $(LIBEDIT_DIR) && $(MAKE) install_hdr install_man install_lib_a )
	(cd lib && ranlib libedit.a)

clean:
	rm -rf lib man include $(LIBEDIT_DIR)

#       This code is here to Force a 32 Bit Compilation of the libedit shared library
#       Note: We do not use the libedit shared library, only the static version.
#	(cd $(LIBEDIT_DIR) && sed "s/S_LDFLAGS = -shared/S_LDFLAGS = -shared  /" Makefile > Makefile.m32 )
#	(cd $(LIBEDIT_DIR) && mv -f Makefile.m32 Makefile )
#	(cd $(LIBEDIT_DIR) && $(MAKE) )
#	(cd $(LIBEDIT_DIR) && $(MAKE) install )
