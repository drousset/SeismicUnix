h64642
s 00006/00005/00046
d D 1.3 88/05/18 16:22:27 valer 3 2
c 
e
s 00009/00009/00042
d D 1.2 88/04/28 17:12:23 valer 2 1
c outtrace independent
e
s 00051/00000/00000
d D 1.1 88/04/14 13:52:31 shuki 1 0
c date and time created 88/04/14 13:52:31 by shuki
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

#include "../include/su.h"

int xargc;				/* THIS EXTERNALS ARE ESSENTIAL	*/
char *sdoc,**xargv;			/* TO LINK THIS MODULE TO THE	*/
bool verbose;				/* SU LIBRARY			*/

main(ac,av)
int ac; char **av;
{
	int infd,outfd,itr;
D 3
	float *indata,*outdata;
E 3
D 2
	Sutrace tr;
E 2
I 2
	Sutrace trin,trout;
E 2
	Subhed bh;

	xargc = ac; xargv = av;			/* INITIALIZATIONS	*/
	inits(ac,av);

	infd = input();					/* OPEN FILES	*/
	outfd = output();

	apass(infd,outfd);			/* PASS ASCII HEADER	*/

D 2
	bpass(infd,outfd,&bh);			/* PASS BINARY HEADER	*/
E 2
I 2
	getbh(infd,&bh);			/* GET BINARY HEADER	*/
E 2

				/* DYNAMIC TRACE MEMORY ALLOCATION	*/
D 2
	indata = (float*) malloc(bh.ns*bh.esize);
E 2
I 2
	trin.data = (float*) malloc(bh.ns*bh.esize);
E 2

D 3
	outdata = (float*) malloc(bh.ns*bh.esize);
E 3
I 3
	trout.data = (float*) malloc(bh.ns*bh.esize);
E 3

D 2
	tr.data = indata;
E 2

D 2
	for(itr=0;gettr(infd,&tr)!=0;itr++) {		/* MAIN LOOP	*/
E 2
I 2
	for(itr=0;gettr(infd,&trin)!=0;itr++) {		/* MAIN LOOP	*/
E 2

D 2
		if(trseq(itr,&tr,&bh,outdata)) {
E 2
I 2
D 3
		bcopy((char*)&trin,(char*)&trout,sizeof(Sutrace));	
		trout.data = outdata;
E 3
I 3
		bcopy((char*)&trin,(char*)&trout,sizeof(Sutrace)-sizeof(float*));	
E 3
E 2

D 2
			tr.data = outdata;
E 2
I 2
		if(trseq(itr,&trin,&bh,&trout)) {
E 2

D 2
    			puttr(outfd,&tr);
E 2
I 2
D 3
			if(itr==0) putbh(outfd,&bh);
E 3
I 3
			if(itr==0) {
				hislog(outfd);
				putbh(outfd,&bh);
			}
E 3
E 2

D 2
			tr.data = indata;
E 2
I 2
    			puttr(outfd,&trout);
E 2

		}

	}

	exit(0);
}
E 1
