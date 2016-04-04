/*
 * gain
 */

#include <stdio.h>
#include <math.h>
#include "../include/su.h"

extern char *sdoc;
extern int xargc;
extern char **xargv;
extern bool verbose;
extern bool hisout,bhout;

static float scale,tpow,epow,wagc,gpow,trap,clip,qbal,pbal;
static bool gainall;
static char *lsdoc = 
"sugain [OPTIONS PARAMETERS] <stdin >stdout			\n\
								\n\
OPTIONS:							\n\
	-m	gain only marked traces see sumark		\n\
	-v	verbose						\n\
								\n\
PARAMETERS:							\n\
								\n\
	out(t) = CLIP{AGC{(t**tpow*exp(epow*t)*in(t))**gpow}} <- NOT UP TO DATE	\n\
								\n\
	scale=1	uniform scale					\n\
	tpow=0.0						\n\
	epow=0.0						\n\
	qbal=0.0						\n\
	trap=0.0						\n\
	clip=0.0						\n\
	qclip=100.0						\n\
	dclip=1.0						\n\
	wagc=0.0						\n\
	gpow=1.0						\n\
								\n";

/* INITIALIZE SELF DOCUMENTATION */
initsdoc()
{
	 sdoc = lsdoc;
}

/* GET OPTIONS AND PARAMETERS */
optpars()
{
	int c,j;

	/* GET OPTIONS */
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
	scale = 1.0;	j  = fgetpar("scale",&scale);
	tpow  = 0.0;	j += fgetpar("tpow",&tpow);
	epow  = 0.0;	j += fgetpar("epow",&epow);
	wagc  = 0.0;	j += fgetpar("wagc",&wagc);
	gpow  = 1.0;	j += fgetpar("gpow",&gpow);
	trap  = 0.0;	j += fgetpar("trap",&trap);
	clip  = 0.0;	j += fgetpar("clip",&clip);
	qbal  = 0.0;	j += fgetpar("qbal",&qbal);
	pbal  = 0.0;	j += fgetpar("pbal",&pbal);

	if(!j) err("%smust specify at least one argument!\n",sdoc);
}

/* ADD HISTORY TO ASCII HEADER */
addhis(outfd)
int outfd;
{
	hislog(outfd);
	if(trap >0.0)	hispr(outfd, "\ttrap=%f\n",  trap);
	if(scale!=1.0)	hispr(outfd, "\tscale=%f\n", scale);
	if(tpow !=1.0)	hispr(outfd, "\ttpow=%f\n",  tpow);
	if(epow !=0.0)	hispr(outfd, "\tepow=%f\n",  epow);
	if(gpow !=1.0)	hispr(outfd, "\tgpow=%f\n",  gpow);
	if(qbal >0.0)	hispr(outfd, "\tqbal=%f\n",  qbal);
	if(pbal >0.0)	hispr(outfd, "\tpbal=%f\n",  pbal);
	if(wagc !=0.0)	hispr(outfd, "\twagc=%f\n",  wagc);
	if(clip >0.0)	hispr(outfd, "\tclip=%f\n",  clip);
	hisclose(outfd);
}

trseq(itr,atr,abh)
int itr;
Sutrace *atr;
Subhed *abh;
{
	if( (gainall || atr->mark) ) {
		if(trap >0.0)  dotrap( trap,  atr->ns, atr->data);
		if(scale!=1.0) doscale(scale, atr->ns, atr->data);
		if(tpow !=1.0) dotpow( tpow,  atr->ns, atr->data);
		if(epow !=0.0) doepow( epow,  atr->ns, atr->data);
		if(gpow !=1.0) dogpow( gpow,  atr->ns, atr->data);
		if(qbal >0.0)  doqbal( qbal,  atr->ns, atr->data);
		if(pbal >0.0)  dopbal( pbal,  atr->ns, atr->data);
		if(wagc !=0.0) doagc ( wagc,  atr->ns, atr->data);
		if(clip >0.0)  doclip( clip,  atr->ns, atr->data);
	}
	return(1);
}

postp(){}
