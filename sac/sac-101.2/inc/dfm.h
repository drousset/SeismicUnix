/* ../../inc/dfm.h */

#include "mach.h"
#include "dblPublicDefs.h"
#include "cssStrucs.h"
#include "sacIO.h"

#define MAXSORTFIELDS	5
#define	MCOMP	2
#define	MCUTER	3
#define	MDGSUB	3
#define	MHDRCO	20
#define	MPICK	18
#define	MRWFMT	5
#define COMMIT	0
#define RECALL	1
#define ROLLBACK 2
#define BINARY	0
#define ASCII	1

typedef enum{Ascending, Descending} SortOrder;

/* TWODAYS in seconds is 48 hours times 3600 seconds per hour. maf 970908 */
#define TWODAYS ( 48 * 3600 )

enum readFlag { RDB , HIGH , LOW } ;

struct t_cmdfr {
	long int nstart[MDFL], nfillb[MDFL], nstop[MDFL], nfille[MDFL], 
	 ntotal[MDFL], nxsdd[MDFL];
}	cmdfr;

struct t_cmdfm {
	long int ndfl, idflc;
	long int ndsndx[MDFL], ndsflcnt ;
	long int ncdir;
	long lechof;
	long int ndxhdr[MDFL], ndxdta[MDFL][MCOMP], nlndta[MDFL], ncomp[MDFL];
	long int nwfl;
	long lovrrq, lcut;
	float ocut[2];
	long int icuter;
	long int nrwfmt, iwfmt, icfmt[2], /* nsamp, */ ipckn, ipckz, 
	 ipckg, ipckb, ipcke, ipcko, ipcka, ipckt0, ipckt1, ipckt2, ipckt3, 
	 ipckt4, ipckt5, ipckt6, ipckt7, ipckt8, ipckt9, ipckf, ipckhd[MPICK];
	long lround;
	long int icatco[MHDRCO], itemco[MHDRCO];
	long ldfree;
	long int idgsub, ndgfil;
	long lshift ,	/* TRUE if rcss should perform a time shift */
	     lscale ;	/* TRUE if rcss should calibrate data */
	MagType  nMagSpec ; /* specify which magnitude to read in rcss. maf 970206 */
	int  iauthors ; /* Number of authors names to search for in .arrival css file. maf 970409 */
	long int nSortOrder ; /* added for SORT command. maf 980812 */
	long larray ;	/* true if ARRAY option for READCSS is on */
	long lrascii ;	/* true if RCSS reads ascii flat files, else reads CSSB */
	long lwascii ;  /* true if WCSS writes ascii flat files, else writes CSSB */
	long ldata ;	/* true if waveform committed in COMMIT command. maf 980915 */
        long lpref ;	/* true if CSS picks should be read through the preferences */

	/* for COMMIT / ROLLBACK options. 0: commit, 1: recall trace, 2: rollback */
	long int icomORroll ;

	/* ASCEND / DESCEND for SORT */
	SortOrder idirection[ MAXSORTFIELDS ] ;

	/* for COMMIT option on deletechannel. */
	long int lcommit ;

	/* added for rdsegy, 000327 */
	long int iztype ;

        /* added for writetable */
        long liftype ;
        long lheader ;

	/* added to fix data transfer from SAC buffers to CSS buffers. */
	long ltrust ;
	enum readFlag nreadflag ;
	long lread ;
	int  nfilesFirst ;

        long lcm6 ;  /* GSE CM6 compressed format instead of integer format */
}	cmdfm;


struct t_kmdfm {
	char kdfl[MAXCHARS], kdflrq[MAXCHARS], krddir[MCPFN+1], kwrdir[MCPFN+1],
	     krdcssdir[MCPFN+1] ;
	long lstation;		/* gain became station.  maf 961216 */
	long lchannel;
	char kstation[7];	/* " */
	char kchannel[9];
	long lbandw;
	char kbandw[9];
	long lorient;
	char korient[9], kdirnm[MCPFN+1], kwfl[MAXCHARS], ksuffx[MDFL][3], 
	 kcut[2][9], kcuter[MCUTER][9], kecbdf[9], kecmem[9], krwfmt[MRWFMT][9], 
	 kcfile[2][MCPFN+1], kcfmt[2][9], kpick[MPICK][9], kdcont[MCMSG+1], kdform[MCMSG+1], 
	 kdgsub[MDGSUB][9], kdgfil[MCMSG+1];

	/* The following were added to facilitate rcss reading 
	   picks from .arrival file. maf 970409 */
	char kprefsFileName[ MCPFN ] ;
	char **kauthors ;
	char ktPh[10][9] , ktAu[10][16] ;

	/* Added for COMMIT and ROLLBACK options. */
	/* char kcomORroll[3][9] ; */

	/* BINARY or ASCII options */
	char kbinORasc[2][9] ;

	/* Added for the sort command.  maf 980812 */
	char ksort[ MAXSORTFIELDS ][ 9 ] ;
}	kmdfm;


#ifdef DOINITS
long *const Icatco = &cmdfm.icatco[0] - 1;
long *const Icfmt = &cmdfm.icfmt[0] - 1;
long *const Ipckhd = &cmdfm.ipckhd[0] - 1;
long *const Itemco = &cmdfm.itemco[0] - 1;
long *const Ncomp = &cmdfm.ncomp[0] - 1;
long *const Ndsndx = &cmdfm.ndsndx[0] - 1;
long *const Ndxhdr = &cmdfm.ndxhdr[0] - 1;
long *const Nfillb = &cmdfr.nfillb[0] - 1;
long *const Nfille = &cmdfr.nfille[0] - 1;
long *const Nlndta = &cmdfm.nlndta[0] - 1;
long *const Nstart = &cmdfr.nstart[0] - 1;
long *const Nstop = &cmdfr.nstop[0] - 1;
long *const Ntotal = &cmdfr.ntotal[0] - 1;
long *const Nxsdd = &cmdfr.nxsdd[0] - 1;
float *const Ocut = &cmdfm.ocut[0] - 1;
long const wfHeader = -1 ;
long const allHeader = 0 ;
long const eventHeader = 1 ;
float const MaxMem = 0.3 ;
#else
extern long *const Icatco;
extern long *const Icfmt;
extern long *const Ipckhd;
extern long *const Itemco;
extern long *const Ncomp;
extern long *const Ndsndx;
extern long *const Ndxhdr;
extern long *const Nfillb;
extern long *const Nfille;
extern long *const Nlndta;
extern long *const Nstart;
extern long *const Nstop;
extern long *const Ntotal;
extern long *const Nxsdd;
extern float *const Ocut;
extern long const wfHeader ;
extern long const allHeader ;
extern long const eventHeader ;
extern float MaxMem ;
#endif

