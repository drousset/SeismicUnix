h39096
s 00126/00000/00000
d D 1.1 88/05/19 07:02:58 shuki 1 0
c date and time created 88/05/19 07:02:58 by shuki
e
u
U
f e 0
t
T
I 1
/*
 * sutwind - time window
 *
 * Examples:
 *	sutwind <DATA tmin=0.5 tmax=2 | sugain ...
 *
*/
#include <stdio.h>
#include <math.h>
#include "../include/su.h"

int xargc;
char **xargv;
bool verbose;
char *sdoc = 
"									\n\
suwind <stdin >stdout [OPTIONS PARAMETERS]				\n\
									\n\
OPTIONS:								\n\
	-v	turn verbose on						\n\
									\n\
PARAMETERS:								\n\
	tmin=0.0	min time to pass				\n\
	tmax=		max time to pass (default= from header)		\n\
	itmin=0		min time sample to pass				\n\
	itmax=		max time sample to pass (default= from header)	\n\
	nt=		number of time samples to pass. (nt=itmax-itmin+1)\n\
									\n\
";

main(ac,av)
int ac; char **av;
{
	int infd,outfd,i;
	Sutrace tr;
	Subhed bh;
	float tmin,tmax;
	float dt;
	int itmin,itmax,nt;
	int nzeros;
	char *pzeros;

	xargc = ac; xargv = av;

	/* GET OPTIONS */
	verbose = false;
	while( (i=getopt(xargc,xargv,"v"))!=EOF) {
		switch(i) {
		case 'v':
			verbose = true;
			break;
		case '?':
			warn(__FILE__,__LINE__,"getopt returned '?'");
		}
	}

	/* OPEN FILES	*/
	infd = input();
	outfd = output();

	/* PASS ASCII HEADER	*/
	apass(infd,outfd);

	/* GET BINARY HEADER */
	getbh(infd,&bh);
	tr.data = (float*) malloc(bh.ns*bh.esize);

	/* GET PARAMETERS */
	dt = 0.000001*bh.dt;
	itmin=0;		igetpar("itmin"   , &itmin);
	if(fgetpar("tmin",&tmin)) {
		itmin=tmin/dt;
	} else {
		tmin = itmin*dt;
	}

	itmax=bh.ns-1;		igetpar("itmax"   , &itmax);
	if(fgetpar("tmax",&tmax)) {
		itmax=tmax/dt-1;
	} else {
		tmax =itmax*dt;
	}

	if(igetpar("nt",&nt)) {
		itmax = itmin+nt-1;
		tmax =itmax*dt;
	} else {
		nt =itmax-itmin+1;
	}

	if(itmin<0 || itmin>itmax)
		err(__FILE__,__LINE__,"Illegal itmin=%d or itmax=%d",itmin,itmax);

	hislog(outfd);
	if(verbose)
		fprintf(stderr,"timewindow: itmin=%d itmax=%d nt=%d\n",itmin,itmax,nt);


	nzeros = (nt-bh.ns)*sizeof(float);

	pzeros = (char*) (tr.data + bh.ns - itmin);

	bh.ns = nt;
/* 	suchns(outfd,nt); */
	putbh(outfd,&bh);


	while(gettr(infd,&tr)) {

		if(itmin>0) {
			for(i=itmin;i<=itmax;i++) {
				tr.data[i-itmin] = tr.data[i];
			}
		}

		if(nzeros>0) bzero(pzeros,nzeros);

		tr.ns = nt;

		puttr(outfd,&tr);

	}

	exit(0);
}

E 1
