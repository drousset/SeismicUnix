/* ../../inc/comlists.h */

#define	MCOMNAMES	800
#define	MEXTCOMNAMES	100
#define	MODULEEXTCOM	100
#define	MPROCESSES	5



struct t_cmcomlists {
	long int icomlist, icomliststart[MPROCESSES], ncomlistentries[MPROCESSES], 
	 icommodule[MCOMNAMES], icomindex[MCOMNAMES], nextcomnames, iextcomindex[MEXTCOMNAMES];
	}	cmcomlists;
struct t_kmcomlists {
	char kcomnames[MCOMNAMES][9], kextcomnames[MEXTCOMNAMES][9];
	}	kmcomlists;


#ifdef DOINITS
long *const Icomindex = &cmcomlists.icomindex[0] - 1;
long *const Icomliststart = &cmcomlists.icomliststart[0] - 1;
long *const Icommodule = &cmcomlists.icommodule[0] - 1;
long *const Iextcomindex = &cmcomlists.iextcomindex[0] - 1;
long *const Ncomlistentries = &cmcomlists.ncomlistentries[0] - 1;
#else
extern long *const Icomindex;
extern long *const Icomliststart;
extern long *const Icommodule;
extern long *const Iextcomindex;
extern long *const Ncomlistentries;
#endif

