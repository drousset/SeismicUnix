/* ../../inc/tt.h */

#define	MTTLEN	5
#define	MTTRD	2
#define	MXTT	60
#define TTKILOMETER     1       /* indicates distance in kilometers */
#define	TTDEGREE	2	/* indicates distance in degrees */



struct t_kmtt {
	char kttnm[MXTT][6], kttmodl[MXTT][9], kttrd[MTTRD][9], krdph[6], 
	 kphases[MXTT][6], kmodel[9], **kphaseNames;
	}	kmtt;
struct t_cmtt {
	long	lttm, 		/* 1 means plot travel time curves in prs, 0 means don't */
		ltteven[MXTT], 	/* 1 means data is evenly spaced */
		lttplt[MXTT],
		lpreviousModel, /* 1 means a model was used last time tt command used. maf 960829 */
		npreviousFileNames ;	/* number of files used last time. maf 960829 */
	char	* previousFileNames ;	/* space delimited list of file names used last time. maf 960829 */
	float	xttfirst[MXTT], 
		xttdel[MXTT], 
		ttdist, 
		ttdep;
	long int ittunit;	/* distance units (TTDEGREE or TTKILOMETER) */
	long lrdtt;
	long int nttrd ,
		 nhlines ;	/* number of header lines to skip.  maf 970808 */
	float rdvel;
	long int nrdph, 
		nphases, 	/* number of phases, eg P, S, Pn etc. */
		nttm, 
		ndxtty[MXTT], 	/* indicates the beginning of a tt data curve in cmmem.sacmem */
		ndxttx[MXTT], 	/* same for x axis */
		nttpt[MXTT];
	}	cmtt;


#ifdef DOINITS
long *const Ltteven = &cmtt.ltteven[0] - 1;
long *const Lttplt = &cmtt.lttplt[0] - 1;
long *const Ndxttx = &cmtt.ndxttx[0] - 1;
long *const Ndxtty = &cmtt.ndxtty[0] - 1;
long *const Nttpt = &cmtt.nttpt[0] - 1;
float *const Xttdel = &cmtt.xttdel[0] - 1;
float *const Xttfirst = &cmtt.xttfirst[0] - 1;
#else
extern long *const Ltteven;
extern long *const Lttplt;
extern long *const Ndxttx;
extern long *const Ndxtty;
extern long *const Nttpt;
extern float *const Xttdel;
extern float *const Xttfirst;
#endif

