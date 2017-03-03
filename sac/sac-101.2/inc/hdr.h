/* ../../inc/hdr.h */

#define	MCMHDR	(MFHDR + MNHDR + MIHDR + MLHDR)
#define	MFHDR	70
#define	MHDR	(MCMHDR + MKMHDR)
#define MHDRFILE (MCMHDR + FILEMKMHDR)
#define	MIHDR	20
#define	MIV	830
#define	MKHDR	24
#define FOURBYTEHDRS 164

/* the old character header (kmhdr) size (fortran version) was
   2*MKHDR 32bit words, due to the fact that each entry was
   8 characters (2 32bit words) long.

#define MKMHDR  (2*MKHDR)
  the new character header (kmhdr) size (c version) in 32bit
  words is (9*MKHDR)/4.  Which for MKHDR=24 is 54.  If MKHDR
  is changed then this will have to be looked at, because
  (9*MKHDR)/4 may have a remainder.
*/
/*
  For purposes of maintaining backward compatibility, the
  null terminated strings comprising the character header
  data in memory are written into the file concatenated
  without the terminating nulls.  Read and write operations
  map these strings into/out of the in-memory struct.
*/
#define MKMHDR  54 /* in-memory size of character header struct */
#define FILEMKMHDR (2*MKHDR) /* size of file character header data block */
#define	MLHDR	5
#define	MNHDR	15
#define	MVHDRC	6


#define SAC_VERSION_LOCATION              76
#define SAC_FLOAT_UNDEFINED              (-12345.0)

#define SAC_HEADER_FLOATS                 70   /* 4 bytes  (real or float)     */
#define SAC_HEADER_INTEGERS               15   /* 4 bytes  (integer or int)    */
#define SAC_HEADER_ENUMS                  20   /* 4 bytes  (integer or int)    */
#define SAC_HEADER_LOGICALS               5   /* 4 bytes  (integer or int)    */
#define SAC_HEADER_STRINGS                24   /* 9 bytes  (character or char), actually 23 + 1 */
#define SAC_HEADER_NUMBERS                ( SAC_HEADER_FLOATS + \
					    SAC_HEADER_INTEGERS +	\
					    SAC_HEADER_ENUMS +		\
					    SAC_HEADER_LOGICALS )
#define SAC_HEADER_SIZEOF_NUMBER          4
#define SAC_HEADER_STRING_LENGTH_FILE     8
#define SAC_HEADER_STRING_LENGTH          ( SAC_HEADER_STRING_LENGTH_FILE + 1 )
#define SAC_HEADER_MAJOR_VERSION          6
#define SAC_FIRST_DATA_POINT_WORD         ( SAC_HEADER_NUMBERS + ( SAC_HEADER_STRINGS * 2 ) )
#define SAC_FIRST_COMPONENT               1
#define SAC_SECOND_COMPONENT              2

#define SAC_OK                             0
#define SAC_ERROR_FILE_NOT_EVENLY_SPACED   801
#define SAC_ERROR_FILE_NOT_UNEVENLY_SPACED 802
#define SAC_ERROR_DATA_TRUNCATED_ON_READ   803
#define SAC_ERROR_NOT_A_SAC_FILE          1317




struct t_cmhdr {
	float fhdr[MFHDR];
	long int nhdr[MNHDR], ihdr[MIHDR];
	long lhdr[MLHDR];
	long int niv[MIV];
	float fundef;
	long int iundef, nundef, nvhdrc;
	float exthdr[20];
	long  linc ,	/* TRUE if INC option is set on lh. maf 961212 */
	      llh ;	/* TRUE during the execution of xlh(). maf 961212 */
	}	cmhdr;
struct t_kmhdr {
	char khdr[MKHDR][9], kundef[9];
	}	kmhdr;


/* 	Note:  in the following list, ninf, nhst, and nsn were 
	changed to norid, nevid, and nwfid respectively.  maf 961031 

	mag, imagtyp, and imagsrc added to provide magnitude, 
	magnitude type (mb, ms, ml, etc.) and magnitude source ( ie
	what institution measured the magnitude).  maf 970205 */

#ifdef DOINITS
	float *const a = (float*)((char*)cmhdr.fhdr + 32);
	float *const arrivl = (float*)((char*)cmhdr.fhdr + 32);
	float *const az = (float*)((char*)cmhdr.fhdr + 204);
	float *const b = (float*)((char*)cmhdr.fhdr + 20);
	float *const baz = (float*)((char*)cmhdr.fhdr + 208);
	float *const begin = (float*)((char*)cmhdr.fhdr + 20);
	float *const cmpaz = (float*)((char*)cmhdr.fhdr + 228);
	float *const cmpinc = (float*)((char*)cmhdr.fhdr + 232);
	float *const delta = (float*)cmhdr.fhdr;
	float *const depmax = (float*)((char*)cmhdr.fhdr + 8);
	float *const depmen = (float*)((char*)cmhdr.fhdr + 224);
	float *const depmin = (float*)((char*)cmhdr.fhdr + 4);
	float *const depmn = (float*)((char*)cmhdr.fhdr + 4);
	float *const depmx = (float*)((char*)cmhdr.fhdr + 8);
	float *const dist = (float*)((char*)cmhdr.fhdr + 200);
	float *const e = (float*)((char*)cmhdr.fhdr + 24);
	float *const ennd = (float*)((char*)cmhdr.fhdr + 24);
	float *const evdp = (float*)((char*)cmhdr.fhdr + 152);
	float *const evel = (float*)((char*)cmhdr.fhdr + 148);
	float *const evla = (float*)((char*)cmhdr.fhdr + 140);
	float *const evlo = (float*)((char*)cmhdr.fhdr + 144);
	float *const f = (float*)((char*)cmhdr.fhdr + 80);
	float *const mag = (float*)((char*)cmhdr.fhdr + 156);	/* magnitude. maf 970205 */
	float *const fhdr64 = (float*)((char*)cmhdr.fhdr + 252);
	float *const fhdr65 = (float*)((char*)cmhdr.fhdr + 256);
	float *const fhdr66 = (float*)((char*)cmhdr.fhdr + 260);
	float *const fhdr67 = (float*)((char*)cmhdr.fhdr + 264);
	float *const fhdr68 = (float*)((char*)cmhdr.fhdr + 268);
	float *const fhdr69 = (float*)((char*)cmhdr.fhdr + 272);
	float *const fhdr70 = (float*)((char*)cmhdr.fhdr + 276);
	float *const fini = (float*)((char*)cmhdr.fhdr + 80);
	float *const fmean = (float*)((char*)cmhdr.fhdr + 224);
	float *const fmt = (float*)((char*)cmhdr.fhdr + 36);
	float *const gcarc = (float*)((char*)cmhdr.fhdr + 212);
	long int *const ia = (long*)((char*)cmhdr.niv + 44);
	long int *const iacc = (long*)((char*)cmhdr.niv + 28);
	long int *const iamph = (long*)((char*)cmhdr.niv + 8);
	long int *const ib = (long*)((char*)cmhdr.niv + 32);
	long int *const ichem = (long*)((char*)cmhdr.niv + 168);
	long int *const iday = (long*)((char*)cmhdr.niv + 36);
	long int *const idep = (long*)((char*)cmhdr.ihdr + 4);
	long int *const idisp = (long*)((char*)cmhdr.niv + 20);
	long int *const idown = (long*)((char*)cmhdr.niv + 116);
	long int *const idrop = (long*)((char*)cmhdr.niv + 184);
	long int *const ieast = (long*)((char*)cmhdr.niv + 108);
	long int *const ievreg = (long*)((char*)cmhdr.ihdr + 24);
	long int *const ievtyp = (long*)((char*)cmhdr.ihdr + 28);
	long int *const iftype = (long*)cmhdr.ihdr;
	long int *const iglch = (long*)((char*)cmhdr.niv + 180);
	long int *const igood = (long*)((char*)cmhdr.niv + 176);
	long int *const imagtyp = (long*)((char*)cmhdr.ihdr + 40);/* magnitude type. maf */
	long int *const imagsrc = (long*)((char*)cmhdr.ihdr + 44);/* magnitude source. 970205 */
	long int *const ihdr13 = (long*)((char*)cmhdr.ihdr + 48);
	long int *const ihdr14 = (long*)((char*)cmhdr.ihdr + 52);
	long int *const ihdr15 = (long*)((char*)cmhdr.ihdr + 56);
	long int *const ihdr16 = (long*)((char*)cmhdr.ihdr + 60);
	long int *const ihdr17 = (long*)((char*)cmhdr.ihdr + 64);
	long int *const ihdr18 = (long*)((char*)cmhdr.ihdr + 68);
	long int *const ihdr19 = (long*)((char*)cmhdr.ihdr + 72);
	long int *const ihdr20 = (long*)((char*)cmhdr.ihdr + 76);
	long int *const ihdr4 = (long*)((char*)cmhdr.ihdr + 12);
	long int *const ihglp = (long*)((char*)cmhdr.niv + 136);
	long int *const ihorza = (long*)((char*)cmhdr.niv + 112);
	long int *const iinst = (long*)((char*)cmhdr.ihdr + 16);
	long int *const illlbb = (long*)((char*)cmhdr.niv + 124);
	long int *const ilowsn = (long*)((char*)cmhdr.niv + 188);
        /* these 18 added for magnitude info. maf 970205 */
        long int *const imb = (long*)((char*)cmhdr.niv + 204);
        long int *const ims = (long*)((char*)cmhdr.niv + 208);
        long int *const iml = (long*)((char*)cmhdr.niv + 212);
        long int *const imw = (long*)((char*)cmhdr.niv + 216);
        long int *const imd = (long*)((char*)cmhdr.niv + 220);
        long int *const imx = (long*)((char*)cmhdr.niv + 224);
        long int *const ineic = (long*)((char*)cmhdr.niv + 228);
        long int *const ipdeq = (long*)((char*)cmhdr.niv + 232);
	long int *const ipdew = (long*)((char*)cmhdr.niv + 236);
	long int *const ipde = (long*)((char*)cmhdr.niv + 240);
        long int *const iisc = (long*)((char*)cmhdr.niv + 244);
        long int *const ireb = (long*)((char*)cmhdr.niv + 248);
        long int *const iusgs = (long*)((char*)cmhdr.niv + 252);
        long int *const ibrk = (long*)((char*)cmhdr.niv + 256);
        long int *const icaltech = (long*)((char*)cmhdr.niv + 260);
        long int *const illnl = (long*)((char*)cmhdr.niv + 264);
        long int *const ievloc = (long*)((char*)cmhdr.niv + 268);
        long int *const ijsop = (long*)((char*)cmhdr.niv + 272);
        long int *const iuser = (long*)((char*)cmhdr.niv + 276);
        long int *const iunknown = (long*)((char*)cmhdr.niv + 280);
	/* These 17 lines added for ievtyp.  maf 970325 */
	long int *const iqb = (long*)((char*)cmhdr.niv + 284);
	long int *const iqb1 = (long*)((char*)cmhdr.niv + 288);
	long int *const iqb2 = (long*)((char*)cmhdr.niv + 292);
        long int *const iqbx = (long*)((char*)cmhdr.niv + 296);
        long int *const iqmt = (long*)((char*)cmhdr.niv + 300);
        long int *const ieq = (long*)((char*)cmhdr.niv + 304);
        long int *const ieq1 = (long*)((char*)cmhdr.niv + 308);
        long int *const ieq2 = (long*)((char*)cmhdr.niv + 312);
        long int *const ime = (long*)((char*)cmhdr.niv + 316);
        long int *const iex = (long*)((char*)cmhdr.niv + 320);
        long int *const inu = (long*)((char*)cmhdr.niv + 324);
        long int *const inc = (long*)((char*)cmhdr.niv + 328);
        long int *const io_ = (long*)((char*)cmhdr.niv + 332);
        long int *const il = (long*)((char*)cmhdr.niv + 336);
        long int *const ir = (long*)((char*)cmhdr.niv + 340);
        long int *const it = (long*)((char*)cmhdr.niv + 344);
        long int *const iu = (long*)((char*)cmhdr.niv + 348);
        /* These 9 added for ievtyp, to keep up with database. maf 000530 */
        long int *const ieq3 = (long*)((char*)cmhdr.niv + 352);
        long int *const ieq0 = (long*)((char*)cmhdr.niv + 356);
        long int *const iex0 = (long*)((char*)cmhdr.niv + 360);
        long int *const iqc = (long*)((char*)cmhdr.niv + 364);
        long int *const iqb0 = (long*)((char*)cmhdr.niv + 368);
        long int *const igey = (long*)((char*)cmhdr.niv + 372);
        long int *const ilit = (long*)((char*)cmhdr.niv + 376);
        long int *const imet = (long*)((char*)cmhdr.niv + 380);
        long int *const iodor = (long*)((char*)cmhdr.niv + 384);

	long int *const inorth = (long*)((char*)cmhdr.niv + 104);
	long int *const inucl = (long*)((char*)cmhdr.niv + 144);
	long int *const io = (long*)((char*)cmhdr.niv + 40);
	long int *const iother = (long*)((char*)cmhdr.niv + 172);
	long int *const ipostn = (long*)((char*)cmhdr.niv + 152);
	long int *const ipostq = (long*)((char*)cmhdr.niv + 164);
	long int *const ipren = (long*)((char*)cmhdr.niv + 148);
	long int *const ipreq = (long*)((char*)cmhdr.niv + 160);
	long int *const iquake = (long*)((char*)cmhdr.niv + 156);
	long int *const iqual = (long*)((char*)cmhdr.ihdr + 32);
	long int *const iradev = (long*)((char*)cmhdr.niv + 96);
	long int *const iradnv = (long*)((char*)cmhdr.niv + 88);
	long int *const irldta = (long*)((char*)cmhdr.niv + 192);
	long int *const irlim = (long*)((char*)cmhdr.niv + 4);
	long int *const isro = (long*)((char*)cmhdr.niv + 140);
	long int *const istreg = (long*)((char*)cmhdr.ihdr + 20);
	long int *const isynth = (long*)((char*)cmhdr.ihdr + 36);
	long int *const it0 = (long*)((char*)cmhdr.niv + 48);
	long int *const it1 = (long*)((char*)cmhdr.niv + 52);
	long int *const it2 = (long*)((char*)cmhdr.niv + 56);
	long int *const it3 = (long*)((char*)cmhdr.niv + 60);
	long int *const it4 = (long*)((char*)cmhdr.niv + 64);
	long int *const it5 = (long*)((char*)cmhdr.niv + 68);
	long int *const it6 = (long*)((char*)cmhdr.niv + 72);
	long int *const it7 = (long*)((char*)cmhdr.niv + 76);
	long int *const it8 = (long*)((char*)cmhdr.niv + 80);
	long int *const it9 = (long*)((char*)cmhdr.niv + 84);
	long int *const itanev = (long*)((char*)cmhdr.niv + 100);
	long int *const itannv = (long*)((char*)cmhdr.niv + 92);
	long int *const itime = (long*)cmhdr.niv;
	long int *const iunkn = (long*)((char*)cmhdr.niv + 16);
	long int *const iup = (long*)((char*)cmhdr.niv + 120);
	long int *const ivel = (long*)((char*)cmhdr.niv + 24);
	long int *const ivolts = (long*)((char*)cmhdr.niv + 196);
	long int *const iwwsn1 = (long*)((char*)cmhdr.niv + 128);
	long int *const iwwsn2 = (long*)((char*)cmhdr.niv + 132);
	long int *const ixy = (long*)((char*)cmhdr.niv + 12);
	long int *const ixyz = (long*)((char*)cmhdr.niv + 200);
	long int *const iztype = (long*)((char*)cmhdr.ihdr + 8);

	char *const kstnm = (char*)kmhdr.khdr;
	char *const kevnm = (char*)((char*)kmhdr.khdr + 9);
	char *const khole = (char*)((char*)kmhdr.khdr + 27);
	char *const ko = (char*)((char*)kmhdr.khdr + 36);
	char *const ka = (char*)((char*)kmhdr.khdr + 45);
	char *const kt0 = (char*)((char*)kmhdr.khdr + 54);
	char *const kt1 = (char*)((char*)kmhdr.khdr + 63);
	char *const kt2 = (char*)((char*)kmhdr.khdr + 72);
	char *const kt3 = (char*)((char*)kmhdr.khdr + 81);
	char *const kt4 = (char*)((char*)kmhdr.khdr + 90);
	char *const kt5 = (char*)((char*)kmhdr.khdr + 99);
	char *const kt6 = (char*)((char*)kmhdr.khdr + 108);
	char *const kt7 = (char*)((char*)kmhdr.khdr + 117);
	char *const kt8 = (char*)((char*)kmhdr.khdr + 126);
	char *const kt9 = (char*)((char*)kmhdr.khdr + 135);
	char *const kf = (char*)((char*)kmhdr.khdr + 144);
	char *const kuser0 = (char*)((char*)kmhdr.khdr + 153);
	char *const kuser1 = (char*)((char*)kmhdr.khdr + 162);
	char *const kuser2 = (char*)((char*)kmhdr.khdr + 171);
	char *const kcmpnm = (char*)((char*)kmhdr.khdr + 180);
	char *const knetwk = (char*)((char*)kmhdr.khdr + 189);
	char *const kdatrd = (char*)((char*)kmhdr.khdr + 198);
	char *const kinst = (char*)((char*)kmhdr.khdr + 207);

	long *const lcalda = (long*)((char*)cmhdr.lhdr + 12);
	long *const leven = (long*)cmhdr.lhdr;
	long *const lhdr5 = (long*)((char*)cmhdr.lhdr + 16);
	long *const lovrok = (long*)((char*)cmhdr.lhdr + 8);
	long *const lpspol = (long*)((char*)cmhdr.lhdr + 4);
        long int *const nevid = (long*)((char*)cmhdr.nhdr + 32);
	long int *const nhdr15 = (long*)((char*)cmhdr.nhdr + 56);
	long int *const norid = (long*)((char*)cmhdr.nhdr + 28);
	long int *const npts = (long*)((char*)cmhdr.nhdr + 36);
	long int *const nsnpts = (long*)((char*)cmhdr.nhdr + 40);
	long int *const nvhdr = (long*)((char*)cmhdr.nhdr + 24);
        long int *const nwfid = (long*)((char*)cmhdr.nhdr + 44);
	long int *const nxsize = (long*)((char*)cmhdr.nhdr + 48);
	long int *const nysize = (long*)((char*)cmhdr.nhdr + 52);
	long int *const nzdttm = (long*)cmhdr.nhdr;
	long int *const nzhour = (long*)((char*)cmhdr.nhdr + 8);
	long int *const nzjday = (long*)((char*)cmhdr.nhdr + 4);
	long int *const nzmin = (long*)((char*)cmhdr.nhdr + 12);
	long int *const nzmsec = (long*)((char*)cmhdr.nhdr + 20);
	long int *const nzsec = (long*)((char*)cmhdr.nhdr + 16);
	long int *const nzyear = (long*)cmhdr.nhdr;
	float *const o = (float*)((char*)cmhdr.fhdr + 28);
	float *const odelta = (float*)((char*)cmhdr.fhdr + 16);
	float *const origin = (float*)((char*)cmhdr.fhdr + 28);
	float *const resp0 = (float*)((char*)cmhdr.fhdr + 84);
	float *const resp1 = (float*)((char*)cmhdr.fhdr + 88);
	float *const resp2 = (float*)((char*)cmhdr.fhdr + 92);
	float *const resp3 = (float*)((char*)cmhdr.fhdr + 96);
	float *const resp4 = (float*)((char*)cmhdr.fhdr + 100);
	float *const resp5 = (float*)((char*)cmhdr.fhdr + 104);
	float *const resp6 = (float*)((char*)cmhdr.fhdr + 108);
	float *const resp7 = (float*)((char*)cmhdr.fhdr + 112);
	float *const resp8 = (float*)((char*)cmhdr.fhdr + 116);
	float *const resp9 = (float*)((char*)cmhdr.fhdr + 120);
	float *const sb = (float*)((char*)cmhdr.fhdr + 216);
	float *const scale = (float*)((char*)cmhdr.fhdr + 12);
	float *const sdelta = (float*)((char*)cmhdr.fhdr + 220);
	float *const stdp = (float*)((char*)cmhdr.fhdr + 136);
	float *const stel = (float*)((char*)cmhdr.fhdr + 132);
	float *const stla = (float*)((char*)cmhdr.fhdr + 124);
	float *const stlo = (float*)((char*)cmhdr.fhdr + 128);
	float *const t0 = (float*)((char*)cmhdr.fhdr + 40);
	float *const t1 = (float*)((char*)cmhdr.fhdr + 44);
	float *const t2 = (float*)((char*)cmhdr.fhdr + 48);
	float *const t3 = (float*)((char*)cmhdr.fhdr + 52);
	float *const t4 = (float*)((char*)cmhdr.fhdr + 56);
	float *const t5 = (float*)((char*)cmhdr.fhdr + 60);
	float *const t6 = (float*)((char*)cmhdr.fhdr + 64);
	float *const t7 = (float*)((char*)cmhdr.fhdr + 68);
	float *const t8 = (float*)((char*)cmhdr.fhdr + 72);
	float *const t9 = (float*)((char*)cmhdr.fhdr + 76);
	float *const time0 = (float*)((char*)cmhdr.fhdr + 40);
	float *const time1 = (float*)((char*)cmhdr.fhdr + 44);
	float *const time2 = (float*)((char*)cmhdr.fhdr + 48);
	float *const time3 = (float*)((char*)cmhdr.fhdr + 52);
	float *const time4 = (float*)((char*)cmhdr.fhdr + 56);
	float *const time5 = (float*)((char*)cmhdr.fhdr + 60);
	float *const time6 = (float*)((char*)cmhdr.fhdr + 64);
	float *const time7 = (float*)((char*)cmhdr.fhdr + 68);
	float *const time8 = (float*)((char*)cmhdr.fhdr + 72);
	float *const time9 = (float*)((char*)cmhdr.fhdr + 76);
	float *const user0 = (float*)((char*)cmhdr.fhdr + 160);
	float *const user1 = (float*)((char*)cmhdr.fhdr + 164);
	float *const user2 = (float*)((char*)cmhdr.fhdr + 168);
	float *const user3 = (float*)((char*)cmhdr.fhdr + 172);
	float *const user4 = (float*)((char*)cmhdr.fhdr + 176);
	float *const user5 = (float*)((char*)cmhdr.fhdr + 180);
	float *const user6 = (float*)((char*)cmhdr.fhdr + 184);
	float *const user7 = (float*)((char*)cmhdr.fhdr + 188);
	float *const user8 = (float*)((char*)cmhdr.fhdr + 192);
	float *const user9 = (float*)((char*)cmhdr.fhdr + 196);
	float *const xmaximum = (float*)((char*)cmhdr.fhdr + 240);
	float *const xminimum = (float*)((char*)cmhdr.fhdr + 236);
	float *const ymaximum = (float*)((char*)cmhdr.fhdr + 248);
	float *const yminimum = (float*)((char*)cmhdr.fhdr + 244);
		/* OFFSET Vectors w/subscript range: 1 to dimension */
	float *const Exthdr = &cmhdr.exthdr[0] - 1;
	float *const Fhdr = &cmhdr.fhdr[0] - 1;
	long *const Ihdr = &cmhdr.ihdr[0] - 1;
	long *const Lhdr = &cmhdr.lhdr[0] - 1;
	long *const Nhdr = &cmhdr.nhdr[0] - 1;
	long *const Niv = &cmhdr.niv[0] - 1;

/*	long *const Nzdttm = &nzdttm[0] - 1; */
	long *const Nzdttm = (long*)&cmhdr.nhdr[0] - 1;
#else
extern float *const a;
extern float *const arrivl;
extern float *const az;
extern float *const b;
extern float *const baz;
extern float *const begin;
extern float *const cmpaz;
extern float *const cmpinc;
extern float *const delta;
extern float *const depmax;
extern float *const depmen;
extern float *const depmin;
extern float *const depmn;
extern float *const depmx;
extern float *const dist;
extern float *const e;
extern float *const ennd;
extern float *const evdp;
extern float *const evel;
extern float *const evla;
extern float *const evlo;
extern float *const f;
extern float *const mag;	/* magnitude.  maf 970205 */
extern float *const fhdr64;
extern float *const fhdr65;
extern float *const fhdr66;
extern float *const fhdr67;
extern float *const fhdr68;
extern float *const fhdr69;
extern float *const fhdr70;
extern float *const fini;
extern float *const fmean;
extern float *const fmt;
extern float *const gcarc;
extern long int *const ia;
extern long int *const iacc;
extern long int *const iamph;
extern long int *const ib;
extern long int *const ichem;
extern long int *const iday;
extern long int *const idep;
extern long int *const idisp;
extern long int *const idown;
extern long int *const idrop;
extern long int *const ieast;
extern long int *const ievreg;
extern long int *const ievtyp;
extern long int *const iftype;
extern long int *const iglch;
extern long int *const igood;
extern long int *const imagtyp;		/* magnitude type. maf 970205 */
extern long int *const imagsrc;		/* magnitude source. maf 970205 */
extern long int *const ihdr13;
extern long int *const ihdr14;
extern long int *const ihdr15;
extern long int *const ihdr16;
extern long int *const ihdr17;
extern long int *const ihdr18;
extern long int *const ihdr19;
extern long int *const ihdr20;
extern long int *const ihdr4;
extern long int *const ihglp;
extern long int *const ihorza;
extern long int *const iinst;
extern long int *const illlbb;
extern long int *const ilowsn;
extern long int *const imb;		/* these 20 added to support */
extern long int *const ims;		/* magnitude. maf 970205 */
extern long int *const iml;
extern long int *const imw;
extern long int *const imd;
extern long int *const imx;
extern long int *const ineic;
extern long int *const ipdeq;
extern long int *const ipdew;
extern long int *const ipde;
extern long int *const iisc;
extern long int *const ireb;
extern long int *const iusgs;
extern long int *const ibrk;
extern long int *const icaltech;
extern long int *const illnl;
extern long int *const ievloc;
extern long int *const ijsop;
extern long int *const iuser;
extern long int *const iunknown;
extern long int *const inorth;
extern long int *const inucl;
extern long int *const io;
extern long int *const iother;
extern long int *const ipostn;
extern long int *const ipostq;
extern long int *const ipren;
extern long int *const ipreq;
extern long int *const iquake;
extern long int *const iqual;
extern long int *const iradev;
extern long int *const iradnv;
extern long int *const irldta;
extern long int *const irlim;
extern long int *const isro;
extern long int *const istreg;
extern long int *const isynth;
extern long int *const it0;
extern long int *const it1;
extern long int *const it2;
extern long int *const it3;
extern long int *const it4;
extern long int *const it5;
extern long int *const it6;
extern long int *const it7;
extern long int *const it8;
extern long int *const it9;
extern long int *const itanev;
extern long int *const itannv;
extern long int *const itime;
extern long int *const iunkn;
extern long int *const iup;
extern long int *const ivel;
extern long int *const ivolts;
extern long int *const iwwsn1;
extern long int *const iwwsn2;
extern long int *const ixy;
extern long int *const ixyz;
extern long int *const iztype;
extern long int *const iqb;		/* These 17 for */
extern long int *const iqb1;		/* ievtyp.  maf 970325 */
extern long int *const iqb2;
extern long int *const iqbx;
extern long int *const iqmt;
extern long int *const ieq;
extern long int *const ieq1;
extern long int *const ieq2;
extern long int *const ime;
extern long int *const iex;
extern long int *const inu;
extern long int *const inc;
extern long int *const io_;
extern long int *const il;
extern long int *const ir;
extern long int *const it;
extern long int *const iu;
extern long int *const ieq3;           /* These 9 for ievtype to keep */
extern long int *const ieq0;           /* up with database.  maf 970325 */
extern long int *const iex0;
extern long int *const iqc;
extern long int *const iqb0;
extern long int *const igey;
extern long int *const ilit;
extern long int *const imet;
extern long int *const iodor;

extern char *const kstnm;
extern char *const kevnm;
extern char *const khole;
extern char *const ko;
extern char *const ka;
extern char *const kt0;
extern char *const kt1;
extern char *const kt2;
extern char *const kt3;
extern char *const kt4;
extern char *const kt5;
extern char *const kt6;
extern char *const kt7;
extern char *const kt8;
extern char *const kt9;
extern char *const kf;
extern char *const kuser0;
extern char *const kuser1;
extern char *const kuser2;
extern char *const kcmpnm;
extern char *const knetwk;
extern char *const kdatrd;
extern char *const kinst;

extern long *const lcalda;
extern long *const leven;
extern long *const lhdr5;
extern long *const lovrok;
extern long *const lpspol;
extern long int *const nevid;
extern long int *const nhdr15;
extern long int *const norid;
extern long int *const npts;
extern long int *const nsnpts;
extern long int *const nvhdr;
extern long int *const nwfid;
extern long int *const nxsize;
extern long int *const nysize;
extern long int *const nzdttm;
extern long int *const nzhour;
extern long int *const nzjday;
extern long int *const nzmin;
extern long int *const nzmsec;
extern long int *const nzsec;
extern long int *const nzyear;
extern float *const o;
extern float *const odelta;
extern float *const origin;
extern float *const resp0;
extern float *const resp1;
extern float *const resp2;
extern float *const resp3;
extern float *const resp4;
extern float *const resp5;
extern float *const resp6;
extern float *const resp7;
extern float *const resp8;
extern float *const resp9;
extern float *const sb;
extern float *const scale;
extern float *const sdelta;
extern float *const stdp;
extern float *const stel;
extern float *const stla;
extern float *const stlo;
extern float *const t0;
extern float *const t1;
extern float *const t2;
extern float *const t3;
extern float *const t4;
extern float *const t5;
extern float *const t6;
extern float *const t7;
extern float *const t8;
extern float *const t9;
extern float *const time0;
extern float *const time1;
extern float *const time2;
extern float *const time3;
extern float *const time4;
extern float *const time5;
extern float *const time6;
extern float *const time7;
extern float *const time8;
extern float *const time9;
extern float *const user0;
extern float *const user1;
extern float *const user2;
extern float *const user3;
extern float *const user4;
extern float *const user5;
extern float *const user6;
extern float *const user7;
extern float *const user8;
extern float *const user9;
extern float *const xmaximum;
extern float *const xminimum;
extern float *const ymaximum;
extern float *const yminimum;
extern float *const Exthdr;
extern float *const Fhdr;
extern long *const Ihdr;
extern long *const Lhdr;
extern long *const Nhdr;
extern long *const Niv;
extern long *const Nzdttm;
#endif

