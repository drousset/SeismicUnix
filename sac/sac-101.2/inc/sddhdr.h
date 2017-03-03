/* ../../inc/sddhdr.h */

#define	MBSHDR	716
#define	MSCOM	10
#define	MSREP	150
#define	MWESHD	(MSREP + 12)
#define	MWSHDR	179



struct t_kmshdr {
	char kshdr[MBSHDR][9];
	}	kmshdr;


#ifdef DOINITS
long int *const iscalg = (long*)((char*)kmshdr.kshdr + 112);
long int *const isclas = (long*)kmshdr.kshdr;
long int *const iscom = (long*)((char*)kmshdr.kshdr + 40);
long int *const isdate = (long*)((char*)kmshdr.kshdr + 80);
long int *const isdelt = (long*)((char*)kmshdr.kshdr + 88);
long int *const isfrmt = (long*)((char*)kmshdr.kshdr + 4);
long int *const ishdr = (long*)kmshdr.kshdr;
long int *const isnpts = (long*)((char*)kmshdr.kshdr + 92);
long int *const isrep = (long*)((char*)kmshdr.kshdr + 116);
long int *const issdep = (long*)((char*)kmshdr.kshdr + 108);
long int *const issel = (long*)((char*)kmshdr.kshdr + 96);
long int *const issla = (long*)((char*)kmshdr.kshdr + 100);
long int *const isslo = (long*)((char*)kmshdr.kshdr + 104);
long int *const istime = (long*)((char*)kmshdr.kshdr + 84);
char *const kschan = (char*)((char*)kmshdr.kshdr + 28);
char *const kschdr = (char*)kmshdr.kshdr;
char *const ksclas = (char*)kmshdr.kshdr;
char *const kscom = (char*)((char*)kmshdr.kshdr + 40);
char *const ksevnm = (char*)((char*)kmshdr.kshdr + 12);
char *const ksfrmt = (char*)((char*)kmshdr.kshdr + 4);
char *const ksstnm = (char*)((char*)kmshdr.kshdr + 20);


/* long *const Iscom = &iscom[0] - 1; */
long *const Iscom = (long*)((char*)kmshdr.kshdr + 40 - 4);

/* long *const Ishdr = &ishdr[0] - 1; */
long *const Ishdr = (long*)((char*)kmshdr.kshdr - 4);

/* long *const Isrep = &isrep[0] - 1; */
long *const Isrep = (long*)((char*)kmshdr.kshdr + 116 - 4);

#else
extern long int *const iscalg;
extern long int *const isclas;
extern long int *const iscom;
extern long int *const isdate;
extern long int *const isdelt;
extern long int *const isfrmt;
extern long int *const ishdr;
extern long int *const isnpts;
extern long int *const isrep;
extern long int *const issdep;
extern long int *const issel;
extern long int *const issla;
extern long int *const isslo;
extern long int *const istime;
extern char *const kschan;
extern char *const kschdr;
extern char *const ksclas;
extern char *const kscom;
extern char *const ksevnm;
extern char *const ksfrmt;
extern char *const ksstnm;
extern long *const Iscom;
extern long *const Ishdr;
extern long *const Isrep;
#endif


