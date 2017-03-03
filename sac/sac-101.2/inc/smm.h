/* ../../inc/smm.h */

#define	MVEL	10



struct t_cmsmm {
	long lmtw;
	float omtw[2];
	long int nvel;
	float vel[MVEL];
	long ldistr;
	float distr;
	long loriginr;
	float originr;
	long lgmt;
	long int iodttm[6];
	float value;
	long lgedata;
	float winlen;
	long lnoisemtw;
	float onoisemtw[2];
	long int irmspick;
	}	cmsmm;
struct t_kmsmm {
	char kmtw[2][9], ktmark[9], kvmark[9], kpmark[9], knoisemtw[2][9];
	}	kmsmm;


#ifdef DOINITS
long *const Iodttm = &cmsmm.iodttm[0] - 1;
float *const Omtw = &cmsmm.omtw[0] - 1;
float *const Onoisemtw = &cmsmm.onoisemtw[0] - 1;
float *const Vel = &cmsmm.vel[0] - 1;
#else
extern long *const Iodttm;
extern float *const Omtw;
extern float *const Onoisemtw;
extern float *const Vel;
#endif

