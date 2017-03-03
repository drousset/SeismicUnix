/* ../../inc/site.h */

#define	MODULESITECOM	99
#define	MSITECOMNAMES	10



struct t_cmsite {
	long int nsitecomnames, isitecomindex[MSITECOMNAMES];
	}	cmsite;
struct t_kmsite {
	char ksitecomnames[MSITECOMNAMES][9];
	}	kmsite;


#ifdef DOINITS
long *const Isitecomindex = &cmsite.isitecomindex[0] - 1;
#else
extern long *const Isitecomindex;
#endif

