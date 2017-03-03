#ifndef CSS_STRUCS_H
#define CSS_STRUCS_H

#ifndef NULL
#       define NULL 0
#endif 


#ifndef FALSE
#       define FALSE 0
#       define TRUE !FALSE
#endif 



/*                 +=======================================+                 */
/*=================|        affiliation structure          |=================*/
/*                 +=======================================+                 */

struct affiliation{
	char	net[9];	
	char	sta[7];
	char	lddate[18];
	long    wfid;       /*Used going from SAC to CSS when missing keys. */
};
struct affiliationList{
	struct affiliation *element;	
	struct affiliationList *prev;
	struct affiliationList *next;
        long index;
};

/* Null affiliation structure. */
static struct affiliation nullAffiliation =
{
	"-",				/* net     */
	"-",				/* sta     */
	"-",				/* lddate  */
	  0,                            /* wfid    */
}; 

/*===========================================================================*/






/*                 +=======================================+                 */
/*=================|          arrival  structure           |=================*/
/*                 +=======================================+                 */

struct arrival {
	char	sta[7];
	double	time;
	long	arid;
	long	jdate;
	long	stassid;
	long	chanid;
	char	chan[9];
	char	iphase[9];
	char	stype[2];
	double	deltim;
	double	azimuth;
	double	delaz;
	double	slow;
	double	delslo;
	double	ema;
	double	rect;
	double	amp;
	double	per;
	double	logat;
	char	clip[2];
	char	fm[3];
	double	snr;
	char	qual[2];
	char	auth[16];
	long	commid;
	char	lddate[18];
	char    SACfield[3];
	long    wfid;       /*Used going from SAC to CSS when missing keys. */
};

struct arrivalList{
        struct arrival *element;
        struct arrivalList *prev;
        struct arrivalList *next;
        long index;
};

/* Null arrival structure. */
static struct arrival nullArrival =
{
        "-",                            /* sta     */
        -9999999999.999,                /* time */
        -1,                             /* arid */
        -1,                             /* jdate */
        -1,                             /* stassid */
        -1,                             /* chanid */
        "-",                            /* chan */
        "-",                            /* iphase */
        "-",                            /* stype */
        -1,                             /* deltim */
        -1,                             /* azimuth */
        -1,                             /* delaz */
        -1,                             /* slow */
        -1,                             /* delslo */
        -1,                             /* ema */
        -1,                             /* rect */
        -1,                             /* amp */
        -1,                             /* per */
        -1,                             /* logat */
        "-",                            /* clip */
        "-",                            /* fm */
        -1,                             /* snr */
        "-",                            /* qual */
        "-",                            /* auth */
        -1,                             /*commid */
        "-",                            /* lddate  */
        "-",                            /* SACfield  */
	  0,                            /* wfid    */
}; 

/*===========================================================================*/






/*                 +=======================================+                 */
/*=================|           assoc  structure            |=================*/
/*                 +=======================================+                 */

struct assoc {
	long	arid;
	long	orid;
	char	sta[7];
	char	phase[9];
	double	belief;
	double	delta;
	double	seaz;
	double	esaz;
	double	timeres;
	char	timedef[2];
	double	azres;
	char	azdef[2];
	double	slores;
	char	slodef[2];
	double	emares;
	double	wgt;
	char	vmodel[16];
	long	commid;
	char	lddate[18];
	long    wfid;       /*Used going from SAC to CSS when missing keys. */
};

struct assocList{
        struct assoc *element;
        struct assocList *prev;
        struct assocList *next;
        long index;
};

static struct assoc nullAssoc =
{
        -1,	                              /* arid */
	-1,	                              /* orid */
	"-",	                              /* sta */
	"-",	                              /* phase */
	-1,	                              /* belief */
	-1,	                              /* delta */
	-999.0,	                              /* seaz */
	-999.0,	                              /* esaz */
	-999.0,	                              /* timeres */
	"-",	                              /* timedef */
	-999.0,	                              /* azres */
	"-",	                              /* azdef */
	-99999.0,	                      /* slores */
	"-",	                              /* slodef */
	-999.0,	                              /* emares */
	-1,	                              /* wgt */
	"-",	                              /* vmodel */
	-1,	                              /* commid */
	"-",	                              /* lddate */
	  0,                                  /* wfid    */
};


/*===========================================================================*/






/*                 +=======================================+                 */
/*=================|           event  structure            |=================*/
/*                 +=======================================+                 */

struct event {
	long	evid;
	char	evname[16];
	long	prefor;
	char	auth[16];
	long	commid;
	char	lddate[18];
	long    wfid;       /*Used going from SAC to CSS when missing keys. */
};

struct eventList{
        struct event *element;
        struct eventList *prev;
        struct eventList *next;
        long index;
};

static struct event nullEvent =
{
	-1,	                          /* evid */
	"-",	                          /* evname */
	-1,	                          /* prefor */
	"-",	                          /* auth */
	-1,	                          /* commid */
	"-",	                          /* lddate */
	  0,                              /* wfid    */
};
/*===========================================================================*/






/*                 +=======================================+                 */
/*=================|          gregion  structure           |=================*/
/*                 +=======================================+                 */

struct gregion {
	long	grn;
	char	grname[41];
	char	lddate[18];
	long    wfid;       /*Used going from SAC to CSS when missing keys. */
};

struct gregionList{
        struct gregion *element;
        struct gregionList *prev;
        struct gregionList *next;
        long index;
};

static struct gregion nullGregion =
{
	-1,	                         /* grn */
	"-",	                         /* grname */
	"-",	                         /* lddate */
	  0,                             /* wfid    */
};

/*===========================================================================*/






/*                 +=======================================+                 */
/*=================|        instrument  structure          |=================*/
/*                 +=======================================+                 */

struct instrument {
	long	inid;
	char	insname[51];
	char	instype[7];
	char	band[2];
	char	digital[2];
	double	samprate;
	double	ncalib;
	double	ncalper;
	char	dir[65];
	char	dfile[33];
	char	rsptype[7];
	char	lddate[18];
	long    wfid;       /*Used going from SAC to CSS when missing keys. */
};

struct instrumentList{
        struct instrument *element;
        struct instrumentList *prev;
        struct instrumentList *next;
        long index;
};

static struct instrument nullInstrument =
{
	-1,	                         /* inid */
	"-",	                         /* insname */
	"-",	                         /* instype */
	"-",	                         /* band */
	"-",	                         /* digital */
	-1,	                         /* samprate */
	-1,	                         /* ncalib */
	-1,	                         /* ncalper */
	"-",	                         /* dir */
	"-",    	                 /* dfile */
	"-",	                         /* rsptype */
	"-",	                         /* lddate */
	  0,                             /* wfid    */
};

/*===========================================================================*/






/*                 +=======================================+                 */
/*=================|          origerr  structure           |=================*/
/*                 +=======================================+                 */

struct origerr {
	long	orid;
	double	sxx;
	double	syy;
	double	szz;
	double	stt;
	double	sxy;
	double	sxz;
	double	syz;
	double	stx;
	double	sty;
	double	stz;
	double	sdobs;
	double	smajax;
	double	sminax;
	double	strike;
	double	sdepth;
	double	stime;
	double	conf;
	long	commid;
	char	lddate[18];
	long    wfid;       /*Used going from SAC to CSS when missing keys. */
};

struct origerrList{
        struct origerr *element;
        struct origerrList *prev;
        struct origerrList *next;
        long index;
};

static struct origerr nullOrigerr =
{
	-1,	                       /* orid */
	-1,	                       /* sxx */
	-1,	                       /* syy */
	-1,	                       /* szz */
	-1,	                       /* stt */
	-1,	                       /* sxy */
	-1,	                       /* sxz */
	-1,	                       /* syz */
	-1,	                       /* stx */
	-1,	                       /* sty */
	-1,	                       /* stz */
	-1,	                       /* sdobs */
	-1,	                       /* smajax */
	-1,	                       /* sminax */
	-1,	                       /* strike */
	-1,	                       /* sdepth */
	-1,	                       /* stime */
	-1,	                       /* conf */
	-1,	                       /* commid */
	"-",	                       /* lddate */
	  0,                           /* wfid    */
};

/*===========================================================================*/






/*                 +=======================================+                 */
/*=================|           origin  structure           |=================*/
/*                 +=======================================+                 */

struct origin {
	double	lat;
	double	lon;
	double	depth;
	double	time;
	long	orid;
	long	evid;
	long	jdate;
	long	nass;
	long	ndef;
	long	ndp;
	long	grn;
	long	srn;
	char	etype[8];
	double	depdp;
	char	dtype[2];
	double	mb;
	long	mbid;
	double	ms;
	long	msid;
	double	ml;
	long	mlid;
	char	algorithm[16];
	char	auth[16];
	long	commid;
	char	lddate[18];
	long    wfid;       /*Used going from SAC to CSS when missing keys. */
};

struct originList{
        struct origin *element;
        struct originList *prev;
        struct originList *next;
        long index;
};

/* Null origin structure. */
static struct origin nullOrigin =
{
        -999.0,                 /* lat       */
        -999.0,                 /* lon       */
        -999.0,                 /* depth     */
        -9999999999.999,        /* time      */
        -1,                     /* orid      */
        -1,                     /* evid      */
        -1,                     /* jdate     */
        -1,                     /* nass      */
        -1,                     /* ndef      */
        -1,                     /* ndp       */
        -1,                     /* grn       */
        -1,                     /* srn       */
        "-",                    /* etype     */
        -999.0,                 /* depdp     */
        "-",                    /* dtype     */
        -999.0,                 /* mb        */
        -1,                     /* mbid      */
        -999.0,                 /* ms        */
        -1,                     /* msid      */
        -999.0,                 /* ml        */
        -1,                     /* mlid      */
        "-",                    /* algorithm */
        "-",                    /* auth      */
        -1,                     /* commid    */
        "-",                    /* lddate    */
	  0,                    /* wfid    */
};

/*===========================================================================*/






/*                 +=======================================+                 */
/*=================|            remark  structure          |=================*/
/*                 +=======================================+                 */

struct remark {
	long	commid;
	long	lineno;
	char	remark[81];
	char	lddate[18];
};

struct remarkList{
        struct remark *element;
        struct remarkList *prev;
        struct remarkList *next;
        long index;
};

static struct remark nullRemark =
{
	-1,	                   /* commid */
	-1,	                   /* lineno */
	"-",	                   /* remark */
	"-",	                   /* lddate */
};
/*===========================================================================*/






/*                 +=======================================+                 */
/*=================|           sensor  structure           |=================*/
/*                 +=======================================+                 */

struct sensor {
	char	sta[7];
	char	chan[9];
	double	time;
	double	endtime;
	long	inid;
	long	chanid;
	long	jdate;
	double	calratio;
	double	calper;
	double	tshift;
	char	instant[2];
	char	lddate[18];
	long    wfid;       /*Used going from SAC to CSS when missing keys. */
};

struct sensorList{
        struct sensor *element;
        struct sensorList *prev;
        struct sensorList *next;
        long index;
};

static struct sensor nullSensor =
{
	"-",	                     /* sta */
	"-",	                     /* chan */
        -9999999999.999,             /* time      */
	9999999999.999,              /* endtime */
	-1,	                     /* inid */
	-1,	                     /* chanid */
	-1, 	                     /* jdate */
	-1,	                     /* calratio */
	-1,	                     /* calper */
	-1, 	                     /* tshift */
	"-",	                     /* instant */
	"-",	                     /* lddate */
	  0,                         /* wfid    */
};

/*===========================================================================*/






/*                 +=======================================+                 */
/*=================|             site  structure           |=================*/
/*                 +=======================================+                 */


struct site {
	char	sta[7];
	long	ondate;
	long	offdate;
	double	lat;
	double	lon;
	double	elev;
	char	staname[51];
	char	statype[5];
	char	refsta[7];
	double	dnorth;
	double	deast;
	char	lddate[18];
	long    wfid;       /*Used going from SAC to CSS when missing keys. */
};

struct siteList{
        struct site *element;
        struct siteList *prev;
        struct siteList *next;
        long index;
};

static struct site nullSite =
{
	"-",	                       /* sta */
	-1,	                       /* ondate */
	-1,	                       /* offdate */
	-999.0,	                       /* lat */
	-999.0,	                       /* lon */
	-999.0,	                       /* elev */
	"-", 	                       /* staname */
	"-", 	                       /* statype */
	"-",	                       /* refsta */
	0.0, 	                       /* dnorth */
	0.0,	                       /* deast */
	"-",	                       /* lddate */
	  0,                           /* wfid    */
};

/*===========================================================================*/






/*                 +=======================================+                 */
/*=================|           sitechan  structure         |=================*/
/*                 +=======================================+                 */

struct sitechan {
	char	sta[7];
	char	chan[9];
	long	ondate;
	long	chanid;
	long	offdate;
	char	ctype[5];
	double	edepth;
	double	hang;
	double	vang;
	char	descrip[51];
	char	lddate[18];
	long    wfid;       /*Used going from SAC to CSS when missing keys. */
};

struct sitechanList{
        struct sitechan *element;
        struct sitechanList *prev;
        struct sitechanList *next;
        long index;
};

static struct sitechan nullSitechan =
{
	"-",	                   /* sta */
	"-",	                   /* chan */
	-1,	                   /* ondate */
	-1,	                   /* chanid */
	-1,	                   /* offdate */
	"-",	                   /* ctype */
	0.0,	                   /* edepth */
	-9999999999.999,	   /* hang */
	-9999999999.999,	   /* vang */
	"-",	                   /* descrip */
	"-",	                   /* lddate */
	  0,                       /* wfid    */
};
/*===========================================================================*/






/*                 +=======================================+                 */
/*=================|            stassoc  structure         |=================*/
/*                 +=======================================+                 */

struct stassoc {
	long	stassid;
	char	sta[7];
	char	etype[8];
	char	location[33];
	double	dist;
	double	azimuth;
	double	lat;
	double	lon;
	double	depth;
	double	time;
	double	imb;
	double	ims;
	double	iml;
	char	auth[16];
	long	commid;
	char	lddate[18];
	long    wfid;       /*Used going from SAC to CSS when missing keys. */
};

struct stassocList{
        struct stassoc *element;
        struct stassocList *prev;
        struct stassocList *next;
        long index;
};

static struct stassoc nullStassoc =
{
	-1,	                       /* stassid */
	"-",	                       /* sta */
	"-",	                       /* etype */
	"-",	                       /* location */
	-1,	                       /* dist */
	-1,	                       /* azimuth */
	-999.0,	                       /* lat */
	-999.0,	                       /* lon */
	-999.0,	                       /* depth */
	-9999999999.999,	       /* time */
	-999.0, 	                       /* imb */
	-999.0,	                       /* ims */
	-999.0,	                       /* iml */
	"-",	                       /* auth */
	-1,	                       /* commid */
	"-",	                       /* lddate */
	  0,                           /* wfid    */
};

/*===========================================================================*/






/*                 +=======================================+                 */
/*=================|             wfdisc  structure         |=================*/
/*                 +=======================================+                 */


struct wfdisc
{
        char    sta[8];
        char    chan[9];
        double  time;
        long    wfid;
        long    chanid;
        long    jdate;
        double  endtime;
        long    nsamp;
        float   samprate;
        float   calib;
        float   calper;
        char    instype[7];
        char    segtype[2];
        char    dattype[3];
        char    clip[2];
        char    dir[65];
        char    dfile[33];
        long    foff;
        long    commid;
        char    lddate[18];
};

struct trace{
   float *r;
   float *i;
   int Cmplx;
};
	
struct wfdiscList{
        struct wfdisc *element;
        struct wfdiscList *prev;
        struct wfdiscList *next;
        struct trace *seis;
        long index;
};

/* Null wfdisc structure. */
static struct wfdisc wfdisc_null =
{
        "-",                            /* sta     */
        "-",                            /* chan    */
        -9999999999.999,                /* time    */
        -1,                             /* wfid    */
        -1,                             /* chanid  */
        -1,                             /* jdate   */
        9999999999.999,                 /* endtime */
        -1,                             /* nsamp   */
        -1,                             /* samprate*/
        1,                              /* calib   */
        1,                              /* calper  */
        "-",                            /* instype */
        "-",                            /* segtype */
        "-",                            /* dattype */
        "-",                            /* clip    */
        ".",                            /* dir     */
        "-",                            /* dfile   */
        -1,                             /* foff    */
        -1,                             /* commid  */
        "-",                            /* lddate  */
}; 



/*                 +=======================================+                 */
/*=================|             wftag  structure          |=================*/
/*                 +=======================================+                 */

struct wftag {
	char	tagname[9];
	long	tagid;
	long	wfid;
	char	lddate[18];
};

struct wftagList{
        struct wftag *element;
        struct wftagList *prev;
        struct wftagList *next;
        long index;
};

static struct wftag nullWftag =
{
	"-",	                   /* key (arid, orid, evid) */
	-1,	                   /* tagname value */
	-1,	                   /* waveform ID */
	"-",	                   /* lddate */
};
/*===========================================================================*/











/*                 +=======================================+                 */
/*=================|            sacdata  structure         |=================*/
/*                 +=======================================+                 */


struct UD{
   float value[10];
   char  label[3][9];
};   

struct sacdata
{
   struct UD userdata;
   long synthflag;
   long lpspol;
   long iztype;
   long idep;
   long iftype;
   long wfid;
   long nsnpts;
   long nxsize;
   long nysize;
   long leven;
   float fmt;
   float sb;
   float sdelta;
   float xminimum;
   float xmaximum;
   float yminimum;
   float ymaximum;
   
   float odelta;
   float resp[10];
   float evel;
   float az;
   float baz;
   float gcarc;
   float dist;
   long iinst;
   long istreg;
   long iqual;
   long lovrok;
   long lcalda;
   char khole[9];
   char ko[9];
   char kdatrd[9];
};

	
struct sacdataList{
        struct sacdata *element;
        struct sacdataList *prev;
        struct sacdataList *next;
        long index;
};


 
/* Null sacdata structure. */
static struct sacdata nullSacdata =
{
   { {-12345, -12345, -12345, -12345, -12345, -12345, -12345, -12345, -12345, -12345},
     {"-12345", "-12345", "-12345"} },
    -12345,
    -12345,
    -12345,
    -12345,
    -12345,
    0,
    -12345,
    -12345,
    -12345,
    -12345,
    -12345.0,
    -12345.0,
    -12345.0,
    -12345.0,
    -12345.0,
    -12345.0,
    -12345.0,
    -12345.0,
    {-12345.0, -12345.0, -12345.0, -12345.0, -12345.0, -12345.0, -12345.0, -12345.0,
     -12345.0, -12345.0},
    -12345.0,
    -12345.0,
    -12345.0,
    -12345.0,
    -12345.0,
    -12345,
    -12345,
    -12345,
    -12345,
    -12345,
    "-12345",
    "-12345",
    "-12345"
}; 




#endif 
