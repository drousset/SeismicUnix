/* ../../inc/xyz.h */

#define	MZLLIST	40
#define	MZREGIONS	(2*MZLLIST + 1)



struct t_cmxyz {
	long lcleanup;
	float zllist[MZLLIST];
	long int nzllist;
	long lzllist, lzlmin;
	float zlmin;
	long lzlmax;
	float zlmax;
	long lzlinc;
	float zlinc;
	long lzlines;
	long int izlines[MZLLIST], nzlines;
	float zregions[MZREGIONS];
	long int nzregions;
	long laspect;
	}	cmxyz;


#ifdef DOINITS
long *const Izlines = &cmxyz.izlines[0] - 1;
float *const Zllist = &cmxyz.zllist[0] - 1;
float *const Zregions = &cmxyz.zregions[0] - 1;
#else
extern long *const Izlines;
extern float *const Zllist;
extern float *const Zregions;
#endif

