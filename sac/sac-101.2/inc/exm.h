/* ../../inc/exm.h */

#define	MECTP	5
#define	MEVAL	10
#define	MFEVAL	30
#define	MFGTP	15
#define	MLEVAL	10
#define	MMCDIR	100
#define	MREPTP	30
#define	MTRACES	20
#define	MTRANSCRIPTS	5
#define	MTYPES	6



struct t_kmexm {
	char	kstate[9],
		kprmt[13],	/* string containing the prompt, terminated with '$' */
	 kecnof[9], ktime[9], kdate[9], kmach[9], 
	 kfgtp[MFGTP][9], kreptp[MREPTP][9], kbbcl[MCMSG+1], knmbbwrite[MCPFN+1], 
	 kectp[MECTP][9], kpause[MCMSG+1], kfeval[MFEVAL][9], kleval[MLEVAL][9], 
	 kevaln[MCMSG+1], knmprb[MCPFN+1], knmbbf[MCPFN+1], kmcdir[MMCDIR][MCPFN+1], kmcreq[MCPFN+1], 
	 kmcnam[MCPFN+1], kargs[MCMSG+1], knametranscript[MTRANSCRIPTS][MCPFN+1], 
	 ktextwait[9];
	}	kmexm;
struct t_cmexm {
	long lprod, lcomf, lcomcr, linsys;
	long int nfgtp, ifgtp, nfgpts;
	float fgdel, fgbeg, fgsico[2], fglico[2], fgquco[3], fgcuco[4], 
	 fgraco[2], fgistr[1000];
	long int nreptp, irep[MREPTP], nrep;
	long lbball, lnames, lnewline;
	FILE *nunbbwrite;
	long int nectp;
	long lecho;
	long int nfeval, nleval;
	long lfeval;
	long int neval, ifeval[MEVAL];
	float feval[MEVAL-(0)+1];
	long int ileval;
	long lfloat, lprbon, lprbfi;
	FILE *nunprb;
	long lperio;
	long int nperio, nmcdir, ntranscripts, itranscript, imodetranscript; 
	FILE *nuntranscript[MTRANSCRIPTS];
	long lsendtranscript[MTRANSCRIPTS][MTYPES];
	long int ntraces;
	char ktracename[MTRACES][17];
	long lblackboard[MTRACES];
	char ktracevalue[MTRACES][MCMSG+1];
	}	cmexm;


#ifdef DOINITS
float *const Fgcuco = &cmexm.fgcuco[0] - 1;
float *const Fglico = &cmexm.fglico[0] - 1;
float *const Fgquco = &cmexm.fgquco[0] - 1;
float *const Fgraco = &cmexm.fgraco[0] - 1;
float *const Fgsico = &cmexm.fgsico[0] - 1;
long *const Ifeval = &cmexm.ifeval[0] - 1;
long *const Irep = &cmexm.irep[0] - 1;
long *const Lblackboard = &cmexm.lblackboard[0] - 1;
#else
extern float *const Fgcuco;
extern float *const Fglico;
extern float *const Fgquco;
extern float *const Fgraco;
extern float *const Fgsico;
extern long *const Ifeval;
extern long *const Irep;
extern long *const Lblackboard;
#endif

