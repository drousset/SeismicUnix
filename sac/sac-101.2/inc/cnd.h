/* ../../inc/cnd.h */

#define	MDOLEVEL	10
#define	MIFLEVEL	10



struct t_cnd {
	long int niflevel;
	long lifresp[MIFLEVEL];
	long int ndolevel, ndolines[MDOLEVEL], ndotype[MDOLEVEL], idoin1[MDOLEVEL], 
	 idoin2[MDOLEVEL];
	}	cnd;
struct t_kcnd {
	char kdovar[MDOLEVEL][MCPFN+1], kdolist[MDOLEVEL][MCPFN+1], kdoname[MDOLEVEL][MCPFN+1];
	}	kcnd;


#ifdef DOINITS
long *const Idoin1 = &cnd.idoin1[0] - 1;
long *const Idoin2 = &cnd.idoin2[0] - 1;
long *const Lifresp = &cnd.lifresp[0] - 1;
long *const Ndolines = &cnd.ndolines[0] - 1;
long *const Ndotype = &cnd.ndotype[0] - 1;
#else
extern long *const Idoin1;
extern long *const Idoin2;
extern long *const Lifresp;
extern long *const Ndolines;
extern long *const Ndotype;
#endif

