h11075
s 00005/00004/00067
d D 1.2 88/05/23 10:19:46 shuki 2 1
c umainseq
e
s 00071/00000/00000
d D 1.1 88/05/23 06:22:05 shuki 1 0
c date and time created 88/05/23 06:22:05 by shuki
e
u
U
f e 0
t
T
I 1
/*
 * mainseq - Generic main for trace-sequential application programs
 */

#include <stdio.h>
#include "../include/su.h"

int xargc;				/* THIS EXTERNALS ARE ESSENTIAL	*/
char *sdoc,**xargv;			/* TO LINK THIS MODULE TO THE	*/
bool verbose;				/* SU LIBRARY			*/
I 2
bool suout=true;
E 2

main(ac,av)
int ac; char **av;
{
	int infd,outfd,itr,nr,nbpt;
	Sutrace *atrin,*atrout;
	Subhed bh;

	/* INITIALIZATIONS	*/
	xargc = ac; xargv = av;
	inits();

	/* OPEN FILES	*/
	infd = input();	
D 2
	outfd = output();
E 2
I 2
	if(suout) outfd = output();
	else outfd = -1;
E 2

D 2
	/* PASS ASCII HEADER	*/
E 2
I 2
	/* ASCII HEADER	*/
E 2
	apass(infd,outfd);
D 2

E 2
	hislog(outfd);

	/* GET BINARY HEADER	*/
	getbh(infd,&bh);

	/* MEMORY ALLOCATION */
	atrin  = (Sutrace*) malloc(sizeof(Sutrace));
	if(atrin==NULL) err(__FILE__,__LINE__,"Can't malloc atrin");
	atrin->data  = (float*) malloc(bh.ns*bh.esize);
	if(atrin->data==NULL) err(__FILE__,__LINE__,"Can't malloc atrin.data");

	/* FIRST GETTR */
	nr = gettr(infd,atrin);
	nbpt = NTRHB+bh.ns*sizeof(float);
	if(nr!=nbpt)
	  err(__FILE__,__LINE__,"First Gettr() returned %d, should be %d",nr,nbpt);

	/* PRE PROCESSING */
	prep(&bh,atrin,&atrout,infd,outfd);
	if(atrout==NULL) {
		atrout = (Sutrace*) malloc(sizeof(Sutrace));
		if(atrout==NULL) err(__FILE__,__LINE__,"Can't malloc atrout");
		atrout->data = (float*) malloc(bh.ns*bh.esize);
		if(atrout->data==NULL) err(__FILE__,__LINE__,"Can't malloc atrout.data");
	}

	/* PUT BINARY HEADER */
D 2
	putbh(outfd,&bh);
E 2
I 2
	if(suout) putbh(outfd,&bh);
E 2

	/* MAIN PROCESSING LOOP */
	itr = 0;
	do {
		if(trseq(itr,atrin,atrout,&bh))
			puttr(outfd,atrout);
		itr++;
	} while( gettr(infd,atrin) );

	/* POST PROCESSING */
	postp();

	exit(0);
}
E 1
