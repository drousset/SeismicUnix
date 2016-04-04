/*
 * suqtile - get a any quantile value
 */

#include <stdio.h>
#include <math.h>
#include "../include/su.h"

extern char *sdoc;
extern int xargc;
extern char **xargv;
extern bool verbose;
extern bool hisout,bhout;

static bool absv;
static float qtile;
static int iq;
static char *lsdoc = 
"suqtile [OPTIONS PARAMETERS] <stdin >stdout				\n\
									\n\
OPTIONS:								\n\
	-v turn verbose on to print maximum and minimum of every trace	\n\
	-a use absolute values						\n\
PARAMETERS:								\n\
	qtile=100   for minimum, 100 for maximum 50 for median		\n\
									\n";

/* INITIALIZE SELF DOCUMENTATION */
initsdoc()
{
	 sdoc = lsdoc;
}

/* GET OPTIONS AND PARAMETERS */
optpars()
{
	int c;

	absv = false;
	verbose = false;

	while( (c=getopt(xargc,xargv,"v"))!=EOF) {
		switch(c) {
		case 'a':
			absv = true;
			break;
		case 'v':
			verbose = true;
			break;
		case '?':
			warn("getopt returned '?'");
		}
	}

	qtile = 100.0;	fgetpar("qtile",&qtile);
	iq = qtile/100.0*abh->ns - 0.5;


	/* SET HISTORY AND BINARY OUTPUT OFF FOR MAIN */
	if(verbose) hisout = true;
	else hisout = false;
	bhout = false;
}

/* ADD HISTORY TO ASCII HEADER */
addhis(outfd)
int outfd;
{
	hislog(outfd);
}

static int gitr;		/* trace with global max */
static float gmax= -1.0;	/* global absolute maximum */

/* TRACE SEQUENTIAL MAX FINDING */
trseq(itr,atr,abh)
int itr;
Sutrace *atr;
Subhed *abh;
{
	float qval;		/* quantile value on trace */

	if(absv) copyabs(atr->data,atr->data,abh->ns);

	quant(iq,atr->data,abh->ns);

	qval = atr->data[iq];

	if(verbose) printf("%d %e\n", itr, qval);

	if(absmax > gmax) {
		gmax = absmax;
		gitr = itr;
	}
	return(0);
}

postp()
{
	printf("Global max=%e at trace %d\n",gmax,gitr);
}
