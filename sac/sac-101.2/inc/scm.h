/* ../../inc/scm.h */

#define	MQGAIN	8
#define	MRGLMT	3
#define	MRGLTP	2



struct t_cmscm {
	float usraz, usrang;
	long lnpreq;
	float rqqcon, rqrcon, rqccon, dtnew, eps;
	long lbreq;
	float breq;
	long lnreq;
	long int nreq, iqgain[MQGAIN + 1];
	float qlevel;
	long int nqmant, nstrfc;
	long lstrfi, lmean;
	long int nhalf, irgltp, irglmt;
	float thold;
	long lrglwin;
	float orglwin[2];
	long int ndecfc;
	long ldecfi;
	}	cmscm;
struct t_kmscm {
	char krottp[9], krgltp[MRGLTP][9], krglmt[MRGLMT][9], krglwin[2][9];
	}	kmscm;


#ifdef DOINITS
long *const Iqgain = &cmscm.iqgain[0] - 1;
float *const Orglwin = &cmscm.orglwin[0] - 1;
#else
extern long *const Iqgain;
extern float *const Orglwin;
#endif

