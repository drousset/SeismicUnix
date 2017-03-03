/* ../../inc/gdm.h */

#define	MCTSIZE	256
#define	MGD	5
#define	MWINDOWS 10
#define NPSCIMAGE 237


struct t_kmgdm {
	char kgdnam[MGD][13], ctname[MCTSIZE - 1-(0)+1][9], kgtfn[4][41];
	}	kmgdm;
struct t_cmgdm {
	long lgdon[MGD];
	long int igdtyp[MGD];
	long lactiv, lpasiv, lcur;
	long int igdcur, iflcur[MGD], igdhc, iflhc[MGD], igdtxt;
	long lginit, lbegf;
	long int iwindow;
	float xwindowmin[MWINDOWS], xwindowmax[MWINDOWS], ywindowmin[MWINDOWS], 
	 ywindowmax[MWINDOWS];
	long lvsful;
	float vsrat, xvs[2], yvs[2];
	long lvsclip, ltsoft;
	float thgt, twidth, tangle;
	long int ihjust, ivjust, itcol;
	long lfhard;
	long int icolor, nctsize;
        long npscimage;
	float ctred[MCTSIZE - 1-(0)+1], ctgreen[MCTSIZE - 1-(0)+1], ctblue[MCTSIZE - 1-(0)+1];
	long int iline;
	float xold, yold;
	long int iold, nfont, iofset, ispace, iyoff, iyhigh, maxstr;
	float fnthit, fntwid, fntrot;
	short int ascstr[128], stxmin[128], stxmax[128], stroke[3252];
        long lgui;
	}	cmgdm;



#ifdef DOINITS
short *const Ascstr = &cmgdm.ascstr[0] - 1;
long *const Iflcur = &cmgdm.iflcur[0] - 1;
long *const Iflhc = &cmgdm.iflhc[0] - 1;
long *const Igdtyp = &cmgdm.igdtyp[0] - 1;
long *const Lgdon = &cmgdm.lgdon[0] - 1;
short *const Stroke = &cmgdm.stroke[0] - 1;
short *const Stxmax = &cmgdm.stxmax[0] - 1;
short *const Stxmin = &cmgdm.stxmin[0] - 1;
float *const Xvs = &cmgdm.xvs[0] - 1;
float *const Xwindowmax = &cmgdm.xwindowmax[0] - 1;
float *const Xwindowmin = &cmgdm.xwindowmin[0] - 1;
float *const Yvs = &cmgdm.yvs[0] - 1;
float *const Ywindowmax = &cmgdm.ywindowmax[0] - 1;
float *const Ywindowmin = &cmgdm.ywindowmin[0] - 1;
#else
extern short *const Ascstr;
extern long *const Iflcur;
extern long *const Iflhc;
extern long *const Igdtyp;
extern long *const Lgdon;
extern short *const Stroke;
extern short *const Stxmax;
extern short *const Stxmin;
extern float *const Xvs;
extern float *const Xwindowmax;
extern float *const Xwindowmin;
extern float *const Yvs;
extern float *const Ywindowmax;
extern float *const Ywindowmin;
#endif

