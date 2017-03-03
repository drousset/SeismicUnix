/* #include <malloc.h> */
#include <stdio.h>
#include <stdlib.h> 
#include <stddef.h>

#include <math.h>
#include "config.h"
#include "isnan.h"

/* This is taken from src/co/stdu.h */
#define ADDRESS_TYPE long

#ifndef const
#define const
#endif

#ifndef TRUE
#define TRUE (1)
#endif

#ifndef FALSE
#define FALSE (0)
#endif

#define ENDIAN_BIG       1
#define ENDIAN_LITTLE    0
#define ENDIAN_UNKNOWN  -1

#define SAC_USE_DATABASE         "SAC_USE_DATABASE"
#define SAC_DISPLAY_COPYRIGHT    "SAC_DISPLAY_COPYRIGHT"
#define SAC_PPK_LARGE_CROSSHAIRS "SAC_PPK_LARGE_CROSSHAIRS"


struct t_cmmem {
        long nallocated;
	float **sacmem;
	};

typedef char byte;

#define ichar(s) (int)(*(s))
#define maxfi(f1,f2) (long) fmax(f1,f2)
#define minfi(f1,f2) (long) fmin(f1,f2)
#ifndef MIN
#define MIN(f1,f2) ((f1) < (f2) ? (f1) : (f2))
#endif

int CheckByteOrder();

void CSStoSAC () ;
void DBheaderToSac () ;
void DBwfToSac () ;
int  GreaterThanFloat () ;
int  GreaterThanLong  () ;
int  GreaterThanStrg  () ;
void Index () ;
int  LessThanFloat () ;
int  LessThanLong  () ;
int  LessThanStrg  () ;
void SacHeaderToDB () ;
int SeisMgrCode () ;
void SeisMgrToSac () ;
long OnOrOff () ;
void acc();
void acopy();
void adj_geometry();
void adjust_height();
void adjust_width();
void adjustnlist();
void afr();
double aimag();
void alias();

void allamb(struct t_cmmem *memstruct, 
	    long int        nsize, 
	    long int       *index, 
	    long int       *nerr);

void allocatevnode(long int  *node,
		   long int  *nerr);

void alloclabels();
void allocpoints();
void allocsegments();

void apcmsg(char  *kalpha,
	    int    kalpha_s);

void apcmsg2(char *kalpha,
	     int   kalpha_s);

void apcmsg_dfname();

void apcmsgnum(long int number);

void apfmsg();

void apimsg(long int integr);

void aplmsg(char *kalpha,
	    int   kalpha_s);

void aplmsgnum();
void apnmsg();
void append();
void appendstring();
void apply();
void autcor(float      data[], 
	    double     delta, 
	    long int   nsamps, 
	    long int   nwin, 
	    long int   wlen, 
	    char      *type, 
	    char      *stype, 
	    float      ac[], 
	    long int  *nfft, 
	    long int  *nlags, 
	    char      *err, 
	    int        err_s, 
	    float      aux[], 
	    float      ridge_fac);

void autooutmsg();
int  backspace();
void beamadd();
void badd();
void basenm();
void bbdisp();
void bbvel();
void beam();
void begindevice();
void begindevice1();
void begindevice2();
void begindevices();
void beginframe();
void beginframe1();
void beginframe2();
void begingraphics();
void beginwindow();
void beginwindow1();
void beginwindow2();
void benbog();
void beroots();
void bilin2();
void bkin();
void blkdata0();
void brnset();
void buroots();

int  byteswap(void  *swappee,
	      int    Nbytes);

void c1roots();
void c2roots();
void calccontlabel1();
void calccontlabel4();
void calccontlabels();
void calccontrlinks();
void calccontsegs();
void calccontticks();
long calcfftsize();
void calclastpoint();
void calcloc();
void calc_loc2();
void calc_loc3();
void calcsegangle();
void calcsize();
void calstatus();
void calvport();
void calvspace();
void calwvtransform();
double cmplxang();
void capf();
void catchbus();
void catchemt();
void catchfpe();
void catchill();
void catchiot();
void catchquit();
void catchsegv();
void catchsys();
void catchtrap();
void ceigvv();
void centxt();
void cerr();
double cmplxabs();
complexf cmplxadd();
complexf cmplxdiv();
complexf cmplxexp();
complexf cmplxlog();
void cfmt();
complexf cmplxmul();
complexf cmplxneg();
complexf cmplxpow();
complexf cmplxsqrt();
complexf cmplxsub();
double cmplxtof();
void changectable();
void changectable3();
void changestring();
void chebparm();
void check();
void checkdscnt();
void chisq();
void chkpha();
void chpf();
void circle();
void ckinst();
void clearallvlists();
void cleardfl();
void cleards();
void clearws();
void clh();
void clipdp();
void closemacro();

void clrmsg( void );

void clz();
void cmh();
void cmz();
void cnvatf();
void cnvati();
void cnvfmt();
void cnvfre();
void cnvfta();

void cnvita(long int  intgr,
	    char     *kintgr,
	    int       kintgr_s);

void cnvito();
void cnvoti();
void colorcode();
void colorcode1();
void colorcode2();
complexf cmplxcj();
void continuesac();
void contour();
void contourrow();
void convcolorname();
void convcolornum();

void convlistname(char      *vars,
		  int        vars_s,
		  char      *fullvars,
		  int        fullvars_s,
		  long int  *ncfullvars,
		  long int  *nerr);

void convtermfile();

void copy(long int  srce[],
	  long int  sink[],
	  long int  ncopy);

void copydouble();
void copyi();
void copykc();
void copyvlist();
void copyvlist2();
void corr();
void covmat();
void cpft();
void cpftdp();
void cpolar();
void createbbs();

void createvlist(char      *fullvars,
		 int        fullvars_s,
		 long int   length,
		 long int  *node,
		 long int  *nerr);

void createwindow();
void createwindow1();
void createwindow2();
void cresp();
void crit();

void crname(char     *kname, 
	    int       kname_s,
	    byte      kdelim,
	    char     *kappnd,
	    int       kappnd_s,
	    long int *nerr);

void crsac();
void crscor();
void csh();
void csinit();
void cspop();
void cspush();
void csz();
void cszero();
void ctype();
void currentvlist();
void currentvnode();
/* void cursor(); */
void cursor0();
void cursor1();
void cursor2();
void cursoroff();
void cursoron();
void cursortext();
void cursortext1();
void cursortext2();
void cutoffs();
void dcpft();
void ddttm();
void decim();

void decodevdesc(long *descriptor,
		 long *deleteflag,
		 long *readonlyflag,
		 long *indirectflag,
		 long *sharedflag,
		 long *reservedflag,
		 long *applflag1,
		 long *applflag2,
		 long *valueflag,
		 long *desclength,
		 long *namelength,
		 long *valuelength);

void decont();
void defcut();
void defdfl();
void definelimits();
void defmem();
int  deleteAllSacFiles () ;
void deletebbs();
void deletestring();

void deletev(char      *vars,
	     int        vars_s,
	     char      *name,
	     int        name_s,
	     long int  *nerr);

void deletevlist(char      *vars,
		 int        vars_s,
		 char      *mode,
		 long int  *nerr);
void delims();
void depcor();
void depset();
void design();
void detnum();
void dewit();
void dfr();
void dif2();
void dif3();
void dif5();
void diff();
void dircor();
void dispatchevent1();
void dispatchevent2();
void dispatchevents();
void dispid();
void disppk();
void distaz();
void draw();
void draw1();
void draw2();
void dseis();
void dss();
void dtloc();
void dwwssn();
void echoAndShave () ;
void edecim();
void eigenanal();
void ekalp6();
void ekasp2();
void elmag();

void encodevdesc(long *descriptor,
		 long *deleteflag,
		 long *readonlyflag,
		 long *indirectflag,
		 long *sharedflag,
		 long *reservedflag,
		 long *applflag1,
		 long *applflag2,
		 long *valuetype,
		 long *desclength,
		 long *namelength,
		 long *valuelength);

long encodeventry();

void encodevnil(long int  node,
		long int  offset,
		long int *nerr);

void enddevice();
void enddevice1();
void enddevice2();
void endframe();
void endframe1();
void endframe2();
void endgraphics();
void erase();
void erase1();
void erase2();
double estpha();
void evallogical();
void executecommand();
void executemacro();
long existsnv();

long existsv(char  *vars,
	     int    vars_s,
	     char  *name,
	     int    name_s);

long existsvlist(char      *vars,
		 int        vars_s,
		 char      *mode,
		 long int  *node);

void expandwfdiscs();
void extrma();
void eyeomg();
void fastcontdata();
void fdbp();
void fdbr();
void fdhp();
void fdlp();
long int fdplot();
void fft();
long fieldsize();

void fill(float     array[],
	  long int  number,
	  double    value);

void fill2();
char **fill_colorbar();
char *fill_clrbar2();
char *fill_clrbar3();
char *fill_clrbar5();
char **fill_image();
char *fill_image2();
char *fill_image3();
char *fill_image5();
void put_image();
void put_image2();
void put_image3();
void filmsg();
double filtb();
void filterdesign();
double filtk();
void findch();
void findcommand();
void findtt();

void findvnil(long int  startindex,
	      long int *nilindex);

void firtrn();
void fitspl();
void fkevalp();
void fkevalr();
void flash();
double fli2();
void flipdata();
void flushbuffer();
void flushbuffer1();
void flushbuffer2();

#if MISSING_FUNC_FMIN
   double fmax(double a, double b);
#endif
#if MISSING_FUNC_FMAX
   double fmin(double a, double b);
#endif
#if MISSING_FUNC_LROUND
   long int lround(double z);
#endif

long fndelcl () ;
void formhv();
void formmarker();

char *fstrncpy(char *to,
	       int   tolen,
	       char *from,
	       int   fromlen);

complexf flttocmplx();
void g2tofloat();
void gauss();
void gbalp();
void gbasp();
void gc();
void general();
void gennames();
void getalphainfo();
void getalphainfo1();
void getalphainfo2();
char *getarg();
void getatw();

void getbbv(char     *kname,
	    char     *kvalue,
	    long int *nerr,
	    long int  kname_s,
	    long int  kvalue_s);

void getbfl();
void getclun();
void getcolor();
void getcomlist();
void getcontlabel();
void getcontlevelmode();
void getcontlinemode();
void getcontpoint();
void getcontrlink();
void getcontseg();
void getcontseglabel();
long getdata();
void getdeviceinfo1();
void getdeviceinfo2();
void getdevicename();
void getdevicerat1();
void getdevicerat2();
void getdeviceratio();
void getdir();
void getdolen();
void getembeddedargs();
void getepoch();

void getfhv(char     *kname,
	    float    *fvalue,
	    long int *nerr,
	    long int  kname_s);

void getfil();
void getfilenames();
void getvFILEptr();
void gethv();

void getihv(char      *kname,
	    char      *kvalue,
	    long int  *nerr,
	    long int   kname_s,
	    long int   kvalue_s);

void getins();

void getkhv(char      *kname,
	    char      *kvalue,
	    long int  *nerr,
	    long int   kname_s,
	    long int   kvalue_s);

void getlhv(char      *kname,
	    long      *lvalue,
	    long int  *nerr,
	    long int   kname_s);

void getlims();
void getlinestyle();
void getmacroinfo();
void getmaxdevices();

void getsmsg(long int  number,
	     char     *kmsg,
	     int       kmsg_s);

void getnadouble();
void getnainteger();
void getnareal();
void getnentry();
void getnfiles();

void getnhv(char      *kname,
	    long int  *nvalue,
	    long int  *nerr,
	    long int   kname_s);

void getniinteger();
void getnireal();
void getnumericargs();
void getnvdouble();
void getnvinteger();
void getnvlogical();
void getnvreal();
void getnvstring();
void getnvsymbol();
void getpicks();
void getpoint();
void getprefs();
void getran();
void getratio();
void getratio1();
void getratio2();
void getrng();
void getsgfsize();
void getstatus();
void getstringargs();
void getstringsize();
void getsymbolinfo();
void gettermlist();
void gettermtype();
void gettextangle();
void gettextfont();
void gettextjust();
void gettextsize();
void gettextwait();
void gettime();
void getvadouble();
void getvainteger();
void getvareal();
void getvdesc();

void getventry(char      *vars,
	       int        vars_s,
	       char      *name,
	       int        name_s,
	       long int   type,
	       long int  *length,
	       long int  *index,
	       long int  *nerr);

void getviinteger();
void getvireal();

void getvlist(char      *vars,
	      int        vars_s,
	      char      *name,
	      int        name_s,
	      char      *mode,
	      long int  *nerr);

void getvlist2(char      *vars,
	       int        vars_s,
	       char      *name,
	       int        name_s,
	       char      *mode,
	       long int  *nerr);

void getvport();
void getvportclip();
void getvspace();
void getvspaceclip();
void getvspacetype();
void getvvdouble();
void getvvinteger();
void getvvlogical();
void getvvreal();

void getvvstring(char      *vars,
		 int        vars_s,
		 char      *name,
		 int        name_s,
		 long int  *numchars,
		 char      *value,
		 int        value_s,
		 long int  *nerr);

void getvvsymbol();
void getwfdiscs();
void getwindowstat1();
void getwindowstat2();
void getwindowstatus();
void getworld();
void getxlm();
void getylm();
void getyw();
void get_geometry();
void get_geometry2();
void get_geometry3();
void gsref();
void hardcopyoff();
void hardcopyon();
void hardwaretext1();
void hardwaretext2();

void hdrfld(char      *kname,
	    int        kname_s,
	    long int  *icat,
	    long int  *item,
	    long      *lfound);

void hfslpwb();
void hs3();
void htribk();
void htridi();
void iaspcl();
void iaspmodel();
void ictok();
void idttm();
void idttmf();
long iequal();
void iirfilter();
void incat();
void incdat();

void increasenlist(long int   node,
		   long int   newlength,
		   long int  *nerr);

void inctim();
void inctimf();

long indexa(char    *string,
	    int      string_s,
	    long int kchar,
	    long     lfwd,
	    long     locc);

long indexb( char *string,
	     int   string_s);
long indexc();
long indexd();
long indexs();

void iniam(struct t_cmmem *memstruct);

void inibbs( void );

void inibom();
void inicol();
void inicom();
void inicpf();
void inicsf();
void inidbm();
void inidfm();
void inieam();
void iniexm();
void inifks();
void inigam();
void inigdm();
void inigem();
void inigtm();

void inihdr( void );

void iniicm();

void inilhf( void );

void inilin();

void inimsg( void );

void inisam();
void iniscm();
void inismm();
void inisnf();
void inispe();
void inissi();
void inisss();
void inisym();
void initblkdata();
void initcomlists();
void initcommon();
void initcontattr();
void initctable();
void initdevice1();
void initdevice2();

void initializevars( void );

void initnlist();
void initok();
void initpf();
void initrm();
void initsac();
void initsite();

void initvlist(char      *vars,
	       int        vars_s,
	       long int  *index,
	       long int  *nerr);

void iniuom();
void iniusr();

void inivars( void );

void iniwidth();
void inixyz();
void inqmsg();
void inquiremsg();
long inquirev();
void inqvsp();
void inspect();
void inter();

long ipow(long b,
	  long x);

long isign(long a,
	   long b);

int  isNaN();  /* This routine is DEPRECATED in favor of isnan */
void isorta();
long iupcor();
double j0();
void kadate();
void kadttm();
void katime();
void kidate();
void kijdat();
void label_cbar();
long labs();
long lbsrch();
long lcchar();
long lccl();
long lcdfl();
long lcentries();
long lcia();
long lcidi();
long lcint();
long lcirc();
long lcircp();
long lckey();
long lclist();
long lclog();
long lclog2();
long lclogc();
long lclogi();
long lclogr();
long lcmore();
long lcmsg();
long lcquot();
long lcra();
long lcreal();
long lcrest();
long lcrrc();
long lcrrcp();
long lcrtw();
long lctok();
long ldelcl();
long ldolist();
long ldttm();
long lequal();
void levin();
long lfilec();
long lfilesok();
long lgahdr();
long lgdf();
void lifite();
void lifitu();
void line();
void linear();
void linear_interp();
void linkcontsegs();
long linrng();
void listcontsegs();

void ljust(char  *ksym,
	   int    ksym_s);

long lkchar();
long lkcol();
long lkdfl();
long lkentries();
long lkia();
long lkint();
long lkirc();
long lkircp();
long lklist();
long lklog();
long lklog2();
long lklogc();
long lklogi();
long lklogr();
long lkquot();
long lkra();
long lkreal();
long lkrest();
long lkrrc();
long lkrrcp();
long lkrtw();
void lkt();
void lll();
void llsn();
long lnumcl();
long lnxtcl();
void loadxtmp();
void locdp();
void loclab();
void logdta();
void lp();
void lpdesign();
void lptbp();
void lptbr();
void lpthp();
void lrsmlp();
void lrsmsp();
long lwildc();
void macrokeyword();
long macroline();
void macroordered();
void macropreamble();
long macrostatus();
void makeuniq();

void map_chdr_in(float *memarray,
		 float *buffer);

void map_chdr_out(float *memarray,
		  float *buffer);

void map_hdr_in();
void map_hdr_out();
void mapit();
void markcontlabel();
void markhdr();
void markvert();
void markwf();

long max(long a,
	 long b);

void mem();
void mergecontsegs();

long min(long a,
	 long b);

void mlm();

void modcase(long      upflag,
	     char     *input,
	     long int  nchar,
	     char     *output);

void move();
void move1();
void move2();
long nccomp();
long ncpl1();

long nequal(char     *ksrch,
	    char     *klist,
	    int       klist_s,
	    long int  nlist);

void newcontlabel();
void newcontpoint();
void newcontseg();

void newhdr( void );

void newstn();
long next2();
long nextcontseg();
long nextinputfile();

long nextvlist(long int  *index,
	       char      *name,
	       int        name_s);

long nfndcl();
long non_num_com();
void noress();
void noresshf();
long nstrlensp();
void oldbb();
void oldkir();
void openmacro();
void ophelp();

void outmsg( void );

void overlp();
void pcimsg();
void pcmcur();
void pcmrpl();
void pcmsg();
void pcpmsg();
void pcrrpl();
void pcxcur();
void pcxop1();
void pcxop2();
void pcxope();
void pcxops();
void pcxrpl();
void pdecu();
void pdf();
void pds();
void pef();
void phaseadj();
void phaseshift();
void pkchar();
void pkdet();
void pkeval();
void pkfilt();
void pkfunc();
void pl2d();
void plalpha();
void plblank();
void plcalwvtrans();
void plclip();
void plcurv();
void pldta();
void plend();
void plenv();
void plgrid();
void plhome();
void plinit();
void plline();
void plmap();
void plnocl();
void plotcontdata();
void plotcontsegs();
void plotimage();
void plplab();
void plrest();
void plsave();
void pltext();
void pltmsg();
void pltplr();
void plview();
long pointsequal();
void polezero();
void polyfill();
void polygon();
void polyline();
void poptok();
void portable();

double powi(double b,
	    long   x);

void predfl();
void prefPicksToHeader() ;
void prependstring();
long previnputfile();
void prewit();
void printvlist();
void printvlist2();
void probe();
void processembedded();
void processfunction();
void processline();
void processnumeric();
void processstring();
void proerr();
void ptbllp();
void ptp();
void putchs();
void putcl();
void putcontlabel();
void putcontpoint();
void putcontrlink();
void putcontseg();
void putcontseglabel();
void putdentry();
void putdentrynew();
void putdentryold();
void putdreal();
void putfil();
void putnadouble();
void putnainteger();
void putnareal();
void putnentry();
void putnvdouble();
void putnvinteger();
void putnvlogical();
void putnvreal();
void putnvstring();
void putnvsymbol();
void putvadouble();
void putvainteger();
void putvareal();

void putventry(char      *vars,
	       int        vars_s,
	       char      *name,
	       int        name_s,
	       long int  *type,
	       long int   length,
	       long int  *index,
	       long int  *nerr);

void putvFILEptr();
void putvlist();
void putvvdouble();
void putvvinteger();
void putvvlogical();
void putvvreal();

void putvvstring(char      *vars,
		 int        vars_s,
		 char      *name,
		 int        name_s,
		 long int   numchars,
		 char      *value,
		 int        value_s,
		 long int  *nerr);

void putvvsymbol();
double pythag();
void qam();
void qapf();
void qcolor();
void qcut();
void qdevices();
void qfid();
void qgtext();
void qhpf();
void qline();
void qmtw();
void qpicks();
void qsymbol();
void qtitle();
void qwidth();
void qxlabl();
void qxlim();
void qylabl();
void qylim();
void r4sort();
double dbh_random();
void rdcblk_cvsc();
void rdcdta();
void rdchdr();
void rdci();
void rddta();
long rdhdr();
void rdsac();
void rdsdta();
void rdsegy();
void rdshdr();
void rdxdrdta();
void reaamb();

void readbbf(char     *kname,
	     long int *nerr,
	     long int  kname_s);

void readcfl();
void readctable();
void readfl();
void readhdr();
void readtermfile();

void readvfile(char      *fullvars,
	       int        fullvars_s,
	       long int  *node,
	       long int  *nerr);

void rectangle();
void redkir();
void reftek();

void relamb(float    **array, 
	    long int   index, 
	    long int  *nerr);

void relbfl();
void releaselabels();
void releasepoints();
void releasesegments();

void releasevnode(long int   node,
		  long int  *nerr);

void renamevlist();
void repav();
void reperr();
void repiv();
void repivl();
void repkv();
void replv();
void reprtw();
void reprv();
void resdfl();
void revers();
void reverseIndex ( ) ;
void rfir();
double rms();
void rollback();
void rotate();
void rs7();

void rsac1(char      *kname,
	   float      yarray[],
	   long int  *nlen,
	   float     *beg,
	   float     *del,
	   long int  *max_,
	   long int  *nerr,
	   long int   kname_s);

void rsac2(char      *kname,
	   float      yarray[],
	   long int  *nlen,
	   float      xarray[],
	   long int  *max_,
	   long int  *nerr,
	   long int   kname_s);

void rsach();
void rscursor();
void rsk();
void rsl();
void rsm();
void rstn();
void sac();
void sac_history_file_set(char *);
void sac_history_load(char *);
void sacToSeisMgr ();
void saccommands();
void sacinit();

void sacmsg(long int *nerr);

void sandia();
void savearg();
void savewstods();
void scaleimage();
void scallop();
void sctok();
void sduration();
void secord();
void sector();
void selectinputfiles();

void sendmesg(FILE  *unitnumber,
	      long   activate,
	      long   send_[]);

void setbbv(char     *kname,
	    char     *kvalue,
	    long int *nerr,
	    long int  kname_s,
	    long int  kvalue_s);

void setcolor();
void setcolor1();
void setcolor2();
void setcolorname();
void setcomlist();
void setcontcolorlist();
void setcontcolormode();
void setcontdatalim();
void setcontdatamode();
void setcontlabelattr();
void setcontlabellist();
void setcontlabelmode();
void setcontlabelsize();
void setcontlevelincr();
void setcontlevellist();
void setcontlevelmode();
void setcontlevelran();
void setcontlinelist();
void setcontlinemode();
void setcontlineregs();
void setcontlistmode();
void setcontnumlevels();
void setconttickattr();
void setcontticklist();
void setconttickmode();
void setctable();
void setctable1();
void setctable2();
void setctablename();

void setfhv(char      *kname,
	    float     *fvalue,
	    long int  *nerr,
	    long int   kname_s);

void setihv(char      *kname,
	    char      *kvalue,
	    long int  *nerr,
	    long int   kname_s,
	    long int   kvalue_s);

void setinputmode();

void setkhv(char      *kname,
	    char      *kvalue,
	    long int  *nerr,
	    long int   kname_s,
	    long int   kvalue_s);

void setlhv(char      *kname,
	    long int  *lvalue,
	    long int  *nerr,
	    long int   kname_s);

void setlinestyle();
void setlinestyle1();
void setlinestyle2();
void setlinewidth();
void setmacrolevel();
void setmacrostatus();

void setmsg(char     *ktype,
	    long int  number);

void setnfiles();

void setnhv(char      *kanme,
	    long int  *nvalue,
	    long int  *nerr,
	    long int   kname_s);

void setprompt();
void setpsctable();
void setrng();
void setsgfdir();
void setsgfnumber();
void setsgfprefix();
void setsgfsize();
void setsymbolgap();
void setsymbolnum();
void setsymbolsize();
void settextangle();
void settextangle2();
void settextfont();
void settextjust();
void settextsize();
void settextsize1();
void settextsize2();
void settexttype();
void settextwait();
void setvmodified();
void setvport();
void setvportborder();
void setvportclip();
void setvportratio();

void setvreadonly(char      *vars,
		  int        vars_s,
		  char      *name,
		  int        name_s,
		  long      *roflag,
		  long int  *nerr);

void setvspaceclip();
void setvspacetype();
void setwidth1();
void setwidth2();
void setwindowsize();
void setworld();
void shift();

double sign(double a,
	    double b);

void skipdo();
void skipif();
void smooth();
void snla3();
void softwaretext();
void sortcl();
void sortf();
void sorti();
void spcgrm();
void spcval();
void specplot();
void spectr();
long spectrogram();
void spfit();

void splitvname(char      *vars,
		int        vars_s,
		long int   ncvars,
		long int  *ic1,
		long int  *ic2);

void sro();
void srtndx();
void statln();
void statn();
void step();
char *strscpy();

char *subscpy(char *to,
	      int   start,
	      int   end,
	      int   len,
	      char *from);

void subtract();
void symbol();
void synch();
void tabin();
double taper();
void tauint();
void tauspl();
int  terminate();
void testio();
void testos();
void text();
void timeadj();
void timecheck() ;
float timecrossing( float * distArray , float distPoint , float * time , int nPoints );
void toamph();
void tokdel();
void tokenize();
void tokens();
void torlim();
double tosecs();
void tql2();
void tracereport();
void tracevariable();
void transfer();
void trtm();
void ttint();

void typmsg(char *ktype);

double umod();
long uniqueStaAndChan ( );
void unit();

void unsetbbv(char     *kname,
	      long int *nerr,
	      int       kname_s);

void unwrap();
void upcase();
void updatevdisk();
void updhdr();
void usrcom();
void vblist();
void vel();
void velocityadj();
void vfeven();
void vflist();
void vfmaxn();
void vfmax();
void vfminn();
void vfrng();
void vfspec();
void vftime();
void vfxyz();
void vmcalc();
void vmdly();
void vmline();
void vporttoworld();
void wa();
void wabn();
void wapf();
double warp();
void wavfrm();
void whpf1();
void wiech();
void wiener();
void wigint();
void wild();
void wildch();
void wildfl();
void wildgr();
void wildsm();
void window();
void window_data();
void winmov();
void worldcircle();
void worldcursor();
void worldcursortext();
void worlddraw();
void worldline();
void worldmove();
void worldpolygon();
void worldpolyline();
void worldrectangle();
void worldsector();
void worldsymbol();
void worldtovport();
void wrci();
void wrcom();
void wrcss();
void wrfile();
void wrhelp();
void wrindx();

void writebbf(char     *kname,
	      long int *nerr,
	      long int  kname_s);

void writenfile();

void writevfile(char     *vars,
		int       vars_s,
		char     *file,
		long int *nerr);

void writezdata();
void wrlist();
void wrsac();
void wrsdd();
void wrterm();
void wrtmsg();
void wrxdr();

void wsac0(char      *kname,
	   float     *xarray,
	   float     *yarray,
	   long int  *nerr,
	   long int   kname_s);

void wsac1(char      *kname,
	   float      yarray[],
	   long int  *nlen,
	   float     *beg,
	   float     *del,
	   long int  *nerr,
	   long int   kname_s);

void wsac2(char      *kname,
	   float      yarray[],
	   long int  *nlen,
	   float      xarray[],
	   long int  *nerr,
	   long int   kname_s);
void wwlpbn();
void wwsp();
void wwspbn();
void xabs();
void xabsgl();
void xadd();
void xaddf();
void xaddstack();
void xapiir();
void xapk();
void xaxes();
void xaxis();
void xbbfk();
void xbeam();
void xbegindevices();
void xbeginwindow();
void xbenioff();
void xboec();
void xbomc();
void xbp();
void xbr();
void xbreak();
void xcds();
void xch();
void xchangestack();
void xclip();
void xclog();
void xclogi();
void xclogr();
void xcndc();
void xcommit() ;
void xcolor();
void xcontour();
void xconv();
void xcopy();
void xcopyhdr();
void xcor();
void xconvolve();
void xcorrelate();
void xcrrcp();
void xcrtw();
void xcuter();
void xcutim();
void xdatagen();
void xdecimate();
void xdelete();
void xdeletestack();
void xdeltacheck();
void xdfmc();
void xdft();
void xdif();
void xdiff();
void xdistanceaxis();
void xdistancewind();
void xdiv();
void xdivf();
void xdivomega();
void xdo();
void xdrhdr();
void xeamc();
void xecho();
void xelse();
void xelseif();
void xenddevices();
void xenddo();
void xendif();
void xenvelope();
void xeval();
void xexmc();
void xexp();
void xexp10();
void xfg();
void xfid();
void xfir();
void xfitxy();
void xfksc();
void xgamc();
void xgcmc();
void xgemc();
void xgetbb();
void xglobalstack();
void xgrayscale();
void xgrid();
void xgroup();
void xgt();
void xhan();
void xhelp();
void xhilbert();
void xhp();
void xicmc();
void xidft();
void xif();
void ximage();
void xincrementsta();
void xinig();
void xinstallmacro();
void xint();
void xinterpolate();
void xkeepam();
void xkhronhite();
void xlds();
void xlh();
void xlinax();
void xlinaxis();
void xline();
void xliststack();
void xload();
void xlog();
void xlog10();
void xlogax();
void xlogaxis();
void xlp();
void xmacro();
void xmap();
void xmark();
void xmarkptp();
void xmarktimes();
void xmarkvalue();
void xmem();
void xmerge();
void xmlm();
void xmsg();
void xmul();
void xmulf();
void xmulomega();
void xnews();
void xnnmc();
void xoapf();
void xohpf();
void xp();
void xp1();
void xp2();
void xpause();
void xpc();
void xpcor();
void xpds();
void xphase();
void xpickauthor();
void xpickphase();
void xpicks();
void xplab();
void xplotalpha();
void xplotdy();
void xplotpm();
void xplotrecords();
void xplotstack();
void xplotxy();
void xpowgl();
void xppe();
void xppk();
void xprobe();
void xpsp();
void xpspe();
void xqdp();
void xquantize();
void xquitspe();
void xquitsss();
void xr();
void xra();
void xrcor();
void xrcss();
void xreadbbf();
void xreaddb () ;
void xrename();
void xreport();
void xrerr();
void xreverse();
void xrglitches();
void xrh();
void xrmean();
void xrms();
void xrollback () ;
void xrotate();
void xrq();
void xrsdd();
void xrsp();
void xrtm();
void xrtr();
void xsamc();
void xscallop();
void xscmc();
void xsetbb();
void xsetdevice();
void xsetmacro();
void xsetmat();
void xsgf();
void xsitecom();
void xsmmc();
void xsmooth();
void xspe();
void xspec();
void xspectrogram();
void xsqr();
void xsqrt();
void xsss();
void xsssc();
void xstretch();
void xsub();
void xsubf();
void xsumstack();
void xsym();
void xsynch();
void xsyntx();
void xsystemcommand();
void xtablname();
void xtaper();
void xtestsite();
void xticks();
void xtimeaxis();
void xtimewindow();
void xtitle();
void xtrace();
void xtranscript();
void xtransfer();
void xtraveltime();
void xtsize();
void xunsetbb();
void xunwr();
void xuomc();
void xvelocitymode();
void xvelocityrose();
void xversion();
void xvspac();
void xw();
void xwait();
void xwcss();
void xwcor();
void xwh();
void xwhile();
void xwhpf();
void xwidth();
void xwild();
void xwindow();
void xwnr();
void xwritebbf();
void xwritenn();
void xwritestack();
void xwsp();
void xwspe();
void xxdiv();
void xxgrid();
void xxlab();
void xxyzc();
void xydiv();
void xygrid();
void xylab();
void xylim();
void xyzcleanup();
void xzcolors();
void xzerostack();
void xzlabels();
void xzlevels();
void xzlines();
void xzticks();
void yaxis();
void ykalp();
void ykasp();
void ylinax();
void ylinaxis();
void ylogax();
void ylogaxis();
void zauxfile();

void zbasename(char  name[], 
	       long  name_len);

void zbegindevice1();
void zbeginframe1();
void zcatch();

void zclose(long int *nfu,
	    long int *nerr);

void zclosec(int *pfd);

void zcloses(FILE     **nfu,
	     long int  *nerr);

void zdest(char     *kname,
	   int       kname_s,
	   long int *nerr);
void zdestf();
void zenddevice1();
void zendframe1();
void zero();
void zexecute();
void zextcom();
void zflushbuffer1();

void zgetc(long  array[], 
	   char  str[], 
	   long  pnumc);

void zgimsg();
void zgtfun();

void zinquire(char  *kname,
	      long  *lexist);

void zirvfft();
void zistage();
void zload();

void zmemad(short        *pvar,
	    ADDRESS_TYPE *pvarloc);

void zmlm();
double zmod();
void znfile(long int *nfu,
	    char     *kname,
	    int       kname_s,
	    char     *ktype,
	    int       ktype_s,
	    long int *nerr);

void zopen_sac(long int *nfu, 
	       char     *kname, 
	       int       kname_s, 
	       char     *ktype, 
	       int       ktype_s, 
	       long int *nerr);

void zopenc(int  *pfd,
	    char  pfname[],
	    long *pnewfl,
	    long *pro,
	    int  *pnerr,
	    long  pfnlen);

void zopens(FILE     **nfu,
	    char      *kname,
	    int        kname_s,
	    char      *ktype,
	    int        ktype_s,
	    long int  *nerr);
void zpoeof();

void zputc(char  str[],
	   long  strlen,
	   long  array[],
	   long  pnumc);

void zquit();

void zrabs(int   *pfd,
	   char  array[],
	   int   pnwords,
	   int  *pswords,
	   int  *pnerr);

void zrun();
void zrunname();
void zruntext();
void zrvfft();
void zshft();

void zsysop(char *comstr,
	    long dummylen,
	    long *pnumc,
	    long *perr);

void zstage();
void zstart();
void zstep();
void zunit();
long zwindow();

void zwabs(int  *pfd,
	   char  array[],
	   int   pnwords,
	   int  *pswords,
	   int  *pnerr);

char* fstrdup(char     *s, 
	      long int  n);

void DEPRECATED(char *old_func, 
		char *new_func);


void sac_report_files_in_memory(long int *nerr);
void echo_switch(int value);
int  use_database(int setget);
int  display_copyright(int getset);
int  env_bool(char *env, int def);
void color_set(int value);
void color_increment_set(int value);
int  color_on();
int  color_foreground();
int  color_background();
int  color_foreground_default();
int  color_background_default();
void history_size_set(int value);
int  histoy_size();
void lh_columns_set(int value);
void lh_inclusive_set(int value);
void qdp_switch(int flag);
void qdp_points(int n);

void xdiv_power(int flag); 
void xdiv_number(int n); 
void xdiv_increment(float z); 
void xdiv_nice(int flag);

void ydiv_power(int flag);
void ydiv_number(int n);
void ydiv_increment(float z);
void ydiv_nice(int flag);

void xlabel_switch(int flag);
void xlabel_label(char *c);
void xlabel_location(char *c); 
void xlabel_size(char *c);

void ylabel_switch(int flag);
void ylabel_label(char *c);
void ylabel_location(char *c); 
void ylabel_size(char *c);


