/* ../../inc/icm.h */

#define	MAXFP	10
#define	MAXIP	1
#define	MAXKP	2
#define	MINSTR	50



struct t_cmicm {
	float fpfrom[MAXFP];
	long lfpfrom[MAXFP];
	long int ipfrom[MAXIP];
	long lipfrom[MAXIP], lkpfrom[MAXKP];
	float fpto[MAXFP];
	long lfpto[MAXFP];
	long int ipto[MAXIP];
	long lipto[MAXIP], lkpto[MAXKP], lfreql, lprew;
	long int iprew;
	float freq[4];
	long int ninstr;
	long lfd ;
	}	cmicm;
struct t_kmicm {
	char kpfrom[MAXKP][MCPFN+1], kpto[MAXKP][MCPFN+1], kinstr[MINSTR][9];
	}	kmicm;


#ifdef DOINITS
float *const Fpfrom = &cmicm.fpfrom[0] - 1;
float *const Fpto = &cmicm.fpto[0] - 1;
float *const Freq = &cmicm.freq[0] - 1;
long *const Ipfrom = &cmicm.ipfrom[0] - 1;
long *const Ipto = &cmicm.ipto[0] - 1;
long *const Lfpfrom = &cmicm.lfpfrom[0] - 1;
long *const Lfpto = &cmicm.lfpto[0] - 1;
long *const Lipfrom = &cmicm.lipfrom[0] - 1;
long *const Lipto = &cmicm.lipto[0] - 1;
long *const Lkpfrom = &cmicm.lkpfrom[0] - 1;
long *const Lkpto = &cmicm.lkpto[0] - 1;
#else
extern float *const Fpfrom;
extern float *const Fpto;
extern float *const Freq;
extern long *const Ipfrom;
extern long *const Ipto;
extern long *const Lfpfrom;
extern long *const Lfpto;
extern long *const Lipfrom;
extern long *const Lipto;
extern long *const Lkpfrom;
extern long *const Lkpto;
#endif

