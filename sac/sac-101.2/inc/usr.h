/* ../../inc/usr.h */

#define	MLUSR	10
#define	MUSR	30
#define	MVUSR	100



struct t_cmusr {
	long int nusr, ndxusr[MUSR], numusr[MUSR];
	float vusr[MVUSR];
	long int nlusr;
	long lusr[MLUSR];
	}	cmusr;
struct t_kmusr {
	char kusr[MUSR][9], klusr[MLUSR][9];
	}	kmusr;


#ifdef DOINITS
long *const Lusr = &cmusr.lusr[0] - 1;
long *const Ndxusr = &cmusr.ndxusr[0] - 1;
long *const Numusr = &cmusr.numusr[0] - 1;
float *const Vusr = &cmusr.vusr[0] - 1;
#else
extern long *const Lusr;
extern long *const Ndxusr;
extern long *const Numusr;
extern float *const Vusr;
#endif

