/* ../../inc/dload.h */
#define MEXTCOMS 50

struct t_cmextcom {
	long int nfiles;
        long (*extfuncs[MEXTCOMS])();
	}	cmextcom;

