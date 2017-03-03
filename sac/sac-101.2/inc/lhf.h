/* ../../inc/lhf.h */

#define	MAHDR	20
#define	MLHLST	100
#define	MPKRPT	18
#define	MRPT	MSTRPT
#define	MRPTTP	10
#define	MSPRPT	20
#define	MSTRPT	94   /* 81 became 91 as new fields were added. maf 961212 */
		     /* 91 became 94 as mag, imagtyp, and imagsrc (magnitude,
			magnitude type, and magnitude source) were added.
			maf 970205 */
#define	MTM	13



struct t_cmlhf {
	long int nstrpt, npkrpt, nsprpt, nrpt, nrpttp, irpttp;
	long lstall;
	long int nlhlst, ilhlst[MLHLST], nlhcol, itmkrf, itmfnm[MTM], 
	 icatf, icatn, icati, icatl, icatk, icata, nlhdr, nkhdr[MKHDR];
	}	cmlhf;
struct t_kmlhf {
	char kstrpt[MSTRPT][9], kpkrpt[MPKRPT][9], ksprpt[MSPRPT][9], 
	 krpt[MRPT][9], krpttp[MRPTTP][9], kfhdr[MFHDR][9], knhdr[MNHDR][9], 
	 kihdr[MIHDR][9], klhdr[MLHDR][9], kkhdr[MKHDR][9], kahdr[MAHDR][9], 
	 kiv[MIV][9], kdiv[MIV][33];
	}	kmlhf;


#ifdef DOINITS
long *const Ilhlst = &cmlhf.ilhlst[0] - 1;
long *const Itmfnm = &cmlhf.itmfnm[0] - 1;
long *const Nkhdr = &cmlhf.nkhdr[0] - 1;
#else
extern long *const Ilhlst;
extern long *const Itmfnm;
extern long *const Nkhdr;
#endif

