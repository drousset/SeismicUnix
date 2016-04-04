/*
 * agc
 */

#include <stdio.h>
#include <math.h>
#include "../include/su.h"

extern char *sdoc;
extern int xargc;
extern char **xargv;
extern bool verbose;
extern bool hisout,bhout;

static float wagc;
static bool gainall;

static char *lsdoc = 
"suagc [OPTIONS wagc=40] <stdin >stdout			\n\
							\n\
OPTIONS:						\n\
	-m	gain only marked traces see sumark	\n\
	-v	verbose					\n\
							\n\
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

	/* GET OPTIONS */
	verbose = false;	bgetpar("v",&verbose);
/* 	if(option("v")>0) verbose = true; */
/* 	else verbose=false; */

	if(verbose) fprintf(stderr,"verbose is on\n");

	while( (c=getopt(xargc,xargv,"mv"))!=EOF) {
		switch(c) {
		case 'm':
			gainall = false;
			break;
		case 'v':
			verbose = true;
			break;
		case '?':
			warn("getopt returned '?'");
		}
	}

	hisout = true;
	bhout = true;

	/* GET PARAMETERS */
	wagc  = 0.0;	fgetpar("wagc",&wagc);
}

/* ADD HISTORY TO ASCII HEADER */
addhis(outfd)
int outfd;
{
	hislog(outfd);
	hispr(outfd, "\twagc=%f\n",  wagc);
	hisclose(outfd);
}

/* TRACE SEQUENTIAL ROUTINE */
trseq(itr,atr,abh)
int itr;
Sutrace *atr;
Subhed *abh;
{
	if( (gainall || atr->mark) ) {
		doagc ( wagc,  abh->ns, atr->data);
	}
	return(1);
}

/* POST PROCESSING */
postp(){}
