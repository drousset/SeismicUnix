/* ../../inc/datafilelist.h */

struct t_kmdatafilelist {
	char kselectmode[9];
	}	kmdatafilelist;
struct t_cmdatafilelist {
	long int nentries, iselect[MDFL], nselect, jselect;
	}	cmdatafilelist;


#ifdef DOINITS
long *const Iselect = &cmdatafilelist.iselect[0] - 1;
#else
extern long *const Iselect;
#endif

