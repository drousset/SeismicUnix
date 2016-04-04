/*
 * sutrseq - Generic main for trace-sequential application programs
 */

#include "../include/su.h"

int xargc;
char *sdoc,**xargv;
bool verbose,hisout,bhout;

main(ac,av)
int ac; char **av;
{
	int infd,outfd,itr;
	Sutrace tr;
	Subhed bh;

	xargc = ac; xargv = av;	/* INITIALIZE COMMAND LINE EXTERNALS */

	initsdoc();		/* INITIALIZE SELF DOCUMENTATION */

	optpars();		/* OPTIONS AND PARAMETERS */

	/* OPEN FILES */
	infd = input();
	outfd = output();

	apass(infd,outfd,hisout);	/* PASS ASCII HEADER */

	getbh(infd,&bh);		/* READ BINARY HEADER */

	/* READ THE FIRST TRACE */
	if(!gettr(infd,&tr)) err("Can't read the first trace\n");

	/* ADD HISTORY TO ASCII HEADER */
	if(hisout) addhis(outfd);
	if(bhout) hisclose(outfd);

	if(bhout) putbh(outfd,&bh);	/* WRITE BINARY HEADER */

	/* MAIN LOOP */
	itr = 0;
	do {
		if(trseq(itr++,&tr,&bh)) puttr(outfd,&tr);

	} while (gettr(infd,&tr));

	postp();			/* POST PROCESSING */

	exit(0);
}
