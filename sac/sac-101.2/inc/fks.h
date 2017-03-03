/* ../../inc/fks.h */

#define	MXLENP	MDFL

#define MOFFSETOPTS	5
#define OCASCADE	0
#define OREFERENCE	1
#define OUSER		2
#define OSTATION	3
#define OEVENT		4

#define IUSR		1
#define	ISTATION	2
#define IEVENT		4

struct t_cmfks {
	char kmaptype[9];
	long lkfilter, lknorm, leps;
	float eps;
	char kfkmethod[5];
	long int neigenmlm, neigenmus;
	long lexp;
	long int iexp;
	float rkhor;
	long int iazs, iwvs, nlcontour;
        float fkmax, fkmax_azimuth;
        float fkmax_wavenum;
	char scalng[9], ktitle[81], kofbbfk[MCPFN+1];
	long int idimsq;
	long lwr;
	float bear, veloc, anginc, survel, ctrx, ctry, ctrz;
	long int nctr;
	char kofbeam[MCPFN+1];

	long int flagOffset ;	/* added for OFFSET option. maf 970306 */
	char koffset [ MOFFSETOPTS ] [ 9 ] ;	/* " */

	long  lReference ;	/* 1 if reference option is set. maf 970207 */
	int   nReference ;	/* number of numbers set in reference option. */
	float rReference[ 3 ] ;	/* array of numbers for reference option. maf */
	}	cmfks;


