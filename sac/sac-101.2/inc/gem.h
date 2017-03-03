/* ../../inc/gem.h */

#define SAC_LABEL_LOCATIONS  4
#define SAC_FONT_SIZES       4
#define SAC_KEY_SIZE_LENGTH  9

#define	MCMGEM	611
#define	MCPTXT	144
#define	MICOL	10
#define	MILINE	10
#define	MIPEN	10
#define	MISYM	20
#define	MKMGEM	323
#define	MPLAB	25
#define	MSISYM	8
#define	MSYM	16
#define	MSYMTB	40
#define	MTXSIZ	4
#define	MUNSYM	1
#define	MWIDTH	10
#define MAXPRNTRNAMELEN  128



struct t_cmgem {
	long lxlim;
	float ximn, ximx;
	long lylim;
	float yimn, yimx, ximnu, ximxu, yimnu, yimxu, xvspmn, xvspmx, 
	 yvspmn, yvspmx, vspreq, ximnz, ximxz, yimnz, yimxz, xpmn, xpmx, 
	 ypmn, ypmx, xpmnu, xpmxu, ypmnu, ypmxu, xmpip1, xmpip2, ympip1, 
	 ympip2;
	long lxfudg;
	float xfudg;
	long lyfudg;
	float yfudg;
	long lxrev, lyrev;
	long int ixint, iyint;
	long lxfull, lyfull, lloglb, lnice, lxdiv;
	float xdiv;
	long lnxdiv;
	long int nxdiv;
	long lydiv;
	float ydiv;
	long lnydiv;
	long int nydiv;
	long lxpowr, lypowr, ltopax, lbotax, lrigax, llefax, ltoptc, 
	 lbottc, lrigtc, lleftc, lxgen;
	float xfirst, xdelta;
	long lygen;
	float yfirst, ydelta, axwtop, axwbot, axwrig, axwlef, tsaxis;
	long lfloor;
	float floor;
	long lflusd, lrqclp, ltqdp;
	long int ntqdp;
	long lnull;
	float vnull;
	long lbdr, lxgrd;
	long int ixgrd;
	long lygrd;
	long int iygrd;
	long ltitl;
	long int ntitl;
	float tstitl;
	long int ititlp;
	long lxlab;
	long int nxlab;
	float tsxlab;
	long int ixlabp;
	long lylab;
	long int nylab;
	float tsylab;
	long int iylabp, nplab;
	long lplab[MPLAB], lplabl[MPLAB];
	float xplabl[MPLAB-(0)+1], yplabl[MPLAB-(0)+1], tsplab[MPLAB-(0)+1], 
	 taplab[MPLAB-(0)+1];
	long lline;
	long int icline;
	long liline;
	long int iiline[MILINE], niline, jiline, isklin;
	long lsym;
	long int isym;
	long lisym;
	long int iisym[MISYM], nisym, jisym;
	float symsz, symsp;
	long lpen;
	long int ipen;
	long lipen;
	long int iipen[MIPEN], nipen, jipen, iskpen;
	long lcol;
	long int icol;
	long licol;
	long int iicol[MICOL], nicol, jicol, iskcol, ibacol;
	long lwidth;
	long int iwidth, iswidth, isymwidth;
	long liwidth;
	long int iiwidth[MWIDTH], iskwidth, isskwidth, niwidth, jiwidth;
	float skdevfudge, chwid, chht, tscur, txsiz[MTXSIZ], otxsiz[MTXSIZ];
	long int igtfnt;
	float tsdef, txrat, otxrat;
	long lframe;
	float dtxsiz[MTXSIZ], dtxrat;
	long lfqdp;
	long int nfqdp;
	float extgem[43];
	long int ilin, ilog, isolid, idot, ithin;
	float vert, horz;
	long int itiny, ismall, imed, ilarge, itop, ibot, iright, ileft, 
	 itjlef, itjrig, itjbot, itjtop, itjcen;
	long lnwsym, lscsym;
	long int jsyml1, jsym1b, jsym1e;
	long ldbsym;
	long int jsyml2, jsym2b, jsym2e, isyml1[MSYM], isyml2[MSYM], nsymlc[MSISYM + 1];
	long ldrsym[MSYMTB];
	float xsymtb[MSYMTB], ysymtb[MSYMTB], fac[9];
	long lprint ;	/* TRUE if user wants to print a plot */
	long lSGFtemp ; /* TRUE if SGF turned on exclusively for PRINT option */
	}	cmgem;
struct t_kmgem {
	char ksides[4][9], ktitl[145], kxlab[145], kylab[145], kplab[MPLAB][145], 
	 ktxsiz[MTXSIZ][9], kgtqua[9], khjust[9], kvjust[9], ktxpos[5][9], 
	 ktxori[2][9], kxtgem[20][9], kptrName[ MAXPRNTRNAMELEN+1 ] ;
	byte kfac[9];
	}	kmgem;

/* structures used for saving/restoring graphics environment */
long lgems;

struct t_cmgem cmgemsav;

struct t_kmgem kmgemsav;



#ifdef DOINITS
float *const cmgema = (float*)&cmgem.lxlim;
char (*const kmgema)[9] = (char(*)[9])kmgem.ksides;
float *const xvsp = (float*)&cmgem.xvspmn;
float *const yvsp = (float*)&cmgem.yvspmn;

/* float *const Cmgema = &cmgema[0] - 1; */
float *const Cmgema = (float*)(&cmgem.lxlim - 1);

float *const Dtxsiz = &cmgem.dtxsiz[0] - 1;
float *const Extgem = &cmgem.extgem[0] - 1;
float *const Fac = &cmgem.fac[0] - 1;
long *const Iicol = &cmgem.iicol[0] - 1;
long *const Iiline = &cmgem.iiline[0] - 1;
long *const Iipen = &cmgem.iipen[0] - 1;
long *const Iisym = &cmgem.iisym[0] - 1;
long *const Iiwidth = &cmgem.iiwidth[0] - 1;
long *const Isyml1 = &cmgem.isyml1[0] - 1;
long *const Isyml2 = &cmgem.isyml2[0] - 1;
byte *const Kfac = &kmgem.kfac[0] - 1;
long *const Ldrsym = &cmgem.ldrsym[0] - 1;
long *const Lplab = &cmgem.lplab[0] - 1;
long *const Lplabl = &cmgem.lplabl[0] - 1;
long *const Nsymlc = &cmgem.nsymlc[0] - 1;
float *const Otxsiz = &cmgem.otxsiz[0] - 1;
float *const Txsiz = &cmgem.txsiz[0] - 1;
float *const Xsymtb = &cmgem.xsymtb[0] - 1;

/* float *const Xvsp = &xvsp[0] - 1; */
float *const Xvsp = (float*)(&cmgem.xvspmn - 1);
float *const Ysymtb = &cmgem.ysymtb[0] - 1;

/* float *const Yvsp = &yvsp[0] - 1; */
float *const Yvsp = (float*)(&cmgem.yvspmn - 1);
#else
extern float *const cmgema;
extern char (*const kmgema)[9];
extern float *const xvsp;
extern float *const yvsp;
extern float *const Cmgema;
extern float *const Dtxsiz;
extern float *const Extgem;
extern float *const Fac;
extern long *const Iicol;
extern long *const Iiline;
extern long *const Iipen;
extern long *const Iisym;
extern long *const Iiwidth;
extern long *const Isyml1;
extern long *const Isyml2;
extern byte *const Kfac;
extern long *const Ldrsym;
extern long *const Lplab;
extern long *const Lplabl;
extern long *const Nsymlc;
extern float *const Otxsiz;
extern float *const Txsiz;
extern float *const Xsymtb;
extern float *const Xvsp;
extern float *const Ysymtb;
extern float *const Yvsp;
#endif


