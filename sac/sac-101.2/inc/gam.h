/* ../../inc/gam.h */

#define	MFIDFM	5
#define	MFIDLC	5
#define	MFIDNM	10
#define	MFIDTP	5
#define	MOP1	20
#define	MOP2	20
#define	MOPE	100
#define	MOPN	10
#define	MOPNLI	50
#define	MOPT	10
#define	MP2LOC	5
#define	MPKNAM	13
#define	MPKTYP	3
/* #define	MYLIM	100 */
#define	MYLIM	1000
#define MDEFAULT 0
#define MGREY   1
#define MCOLOR  2


struct t_cmgam {
	long lppkpp;
	long int nppkpp;
	long lppkut, lppkrl;
	float vppkrl;
	long lmkall, lsavelocs, lp2abs;
	long int ip2loc;
	float tsp2;
	long int nope, nop1, nop2, nopn, nopt;
	long lrplrq, lpcfil;
	float scamac, rotmac, xopnli[MOPNLI], yopnli[MOPNLI], pcdegi;
	long int npcpsi;
	long lglori;
	float xori, yori, xcdp, ycdp, xcen, ycen, xcdpe, ycdpe, pcalen, 
	 pcamul, pcaang;
	long lpcavi, lpcafi;
	long int icsize;
	long lbdrrq;
	long int nopei, nhtick, nvtick;
	long lbdron, ldsppk;
	long int ipktyp[MPKNAM];
	float pkwdth, pkhgth, tspk, tsfid;
	long lfidrq;	/* 1 or 0 if fileid is on or off respectively */
	long lfinorq;	/* 1 or 0 if filenumber is on or off respectively. maf 970204 */
	long int nfidtp, ifidtp, nfidlc, ifidlc;
	float xfidlc, yfidlc;
	long int nfidnm, nfidst, nfidtx, ifidfm, nfidfm, iur, iul, ilr, 
	 ill;
	long lwaitr, lwaite, lrtwxl;
	float ortwxl[2];
	long int nylim;
	float ylims[MYLIM][2], rngmin, rngmax, fidbdr, xrect[4], yrect[4], 
	 extgam[11];
        long cmap;
	}	cmgam;
struct t_kmgam {
	char kp2loc[MP2LOC][9], kope[MOPE][3];
	byte kop1[MOP1], kop2[MOP2], kopn[MOPN], kopt[MOPT];
	char kopetx[81];
	byte kopqui, kopdel, kopmac, kopbe, kopee, kopcmt;
	char kpcfil[MCPFN+1], kpcmac[MCPFN+1], kpcfsu[5], kpcmsu[5], kopnli[MOPNLI][9], 
	 kpknam[MPKNAM][9], kpktyp[MPKTYP][9], kfidtp[MFIDTP][9], kfidlc[MFIDLC][9], 
	 kfidnm[MFIDNM][9], kfidst[MFIDNM][9], kfidtx[MFIDNM][41], kfidfm[MFIDFM][9], 
	 krtwxl[2][9], kylims[MYLIM][9], kgddef[9], kxtgam[9][9];
	}	kmgam;


#ifdef DOINITS
float *const Extgam = &cmgam.extgam[0] - 1;
long *const Ipktyp = &cmgam.ipktyp[0] - 1;
byte *const Kop1 = &kmgam.kop1[0] - 1;
byte *const Kop2 = &kmgam.kop2[0] - 1;
byte *const Kopn = &kmgam.kopn[0] - 1;
byte *const Kopt = &kmgam.kopt[0] - 1;
float *const Ortwxl = &cmgam.ortwxl[0] - 1;
float *const Xopnli = &cmgam.xopnli[0] - 1;
float *const Xrect = &cmgam.xrect[0] - 1;
float *const Yopnli = &cmgam.yopnli[0] - 1;
float *const Yrect = &cmgam.yrect[0] - 1;
#else
extern float *const Extgam;
extern long *const Ipktyp;
extern byte *const Kop1;
extern byte *const Kop2;
extern byte *const Kopn;
extern byte *const Kopt;
extern float *const Ortwxl;
extern float *const Xopnli;
extern float *const Xrect;
extern float *const Yopnli;
extern float *const Yrect;
#endif

