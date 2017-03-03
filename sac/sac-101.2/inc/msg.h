/* ../../inc/msg.h */

#define	MCOMMANDS	4
#define	MERRORS	1
#define	MFMSG	500
#define	MLIMSG	5
#define	MMACROS	5
#define	MOUTPUT	3
#define	MPROCESSED	6
#define	MTPMSG	6
#define	MUNITS	5
#define	MWARNINGS	2



struct t_kmmsg {
	char	ktpmsg[MTPMSG][9], 
		klimsg[MLIMSG][MCMSG+1],	/* list of messages */
		kfmsg[MFMSG][MCMSG+1];
	}   kmmsg;
struct t_cmmsg {
	long int nummsg, itpmsg, nlimsg, nchmsg;
	long autoout;
	long int nunits;
        FILE *iunits[MUNITS];
	long lsend[MUNITS][MTPMSG];
	long int nfmsg, ifmsg[MFMSG];
	}	cmmsg;


#ifdef DOINITS
long *const Ifmsg = &cmmsg.ifmsg[0] - 1;
#else
extern long *const Ifmsg;
extern long *const Iunits;
#endif

