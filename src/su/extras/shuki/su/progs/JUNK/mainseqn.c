/*
 * mainseqn - Generic main for trace-sequential application programs
 *		when the output is not SU data
 */

#include "../include/su.h"

int xargc;				/* THIS EXTERNALS ARE ESSENTIAL */
char *sdoc,**xargv;			/* TO LINK THIS MODULE TO THE */
bool verbose;				/* SU LIBRARY */

main(ac,av)
int ac; char **av;
{
	int infd,itr;
	Sutrace tr;
	Subhed bh;

	xargc = ac; xargv = av;			/* INITIALIZATIONS	*/
	inits(ac,av);

	infd = input();			/* OPEN FILES */

	apass(infd,-1);			/* READ ASCII HEADER */

	getbh(infd,&bh);		/* READ BINARY HEADER */

					/* DYNAMIC TRACE MEMORY ALLOCATION */
	tr.data = (float*) malloc(bh.ns*bh.esize);


	for(itr=0;gettr(infd,&tr)!=0;itr++) {		/* MAIN LOOP	*/

		trseqn(itr,&tr,&bh);	/* TRACE PROCESSING */
	}

	postp();			/* POST PROCESSING */

	exit(0);
}
