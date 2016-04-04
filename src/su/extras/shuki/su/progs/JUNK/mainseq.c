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
	Sutrace tr;
	Subhed bh;

	xargc = ac; xargv = av;			/* INITIALIZATIONS	*/
	inits(ac,av);

	infd = input();					/* OPEN FILES	*/
	outfd = output();

	apass(infd,outfd);			/* PASS ASCII HEADER	*/

	bpass(infd,outfd,&bh);			/* PASS BINARY HEADER	*/

				/* DYNAMIC TRACE MEMORY ALLOCATION	*/
	tr.data = (float*) malloc(bh.ns*bh.esize);

	for(itr=0;gettr(infd,&tr)!=0;itr++) {		/* MAIN LOOP	*/

		if(trseq(itr,&tr,&bh)) puttr(outfd,&tr);
	}

	exit(0);
}
