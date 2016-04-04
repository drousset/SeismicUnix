/*
 * sucat - concate two data sets
 */

#include "../include/su.h"

int xargc;
char **xargv;

char *sdoc =
"					\n\
sucat data1 data2 > stdout		\n\
					\n\
output is data1data2			\n\
					\n";
bool verbose,hisout,bhout;

main(ac,av)
int ac; char **av;
{
	int infd1,infd2,outfd,nt;
	Sutrace tr;
	Subhed bh1,bh2;

	xargc = ac; xargv = av;

	if(ac!=3) {
		warn("incorrect number of command line args");
		selfdoc();
	}

	/* OPEN FILES */
	infd1 = suopen(av[1]);
	infd2 = suopen(av[2]);
	outfd = output();

	/* ASCII HEADERS */
	hispr(outfd,"History of %s:\n",av[1]);
	apass(infd1,outfd);
	hispr(outfd,"\n");
	hispr(outfd,"History of %s:\n",av[2]);
	apass(infd2,outfd);

	/* BINARY HEADER */
	getbh(infd1,&bh1);
	getbh(infd2,&bh2);
	if(bh1.ns!=bh2.ns) err("incompatible input files");

	tr.data = (float*)malloc(bh1.ns*bh1.esize);

	/* ADD HISTORY TO ASCII HEADER */
	hislog(outfd);
	hispr(outfd, "\tconcating %s and %s\n",av[1],av[2]);

	/* WRITE BINARY HEADER */
	putbh(outfd,&bh2);

	/* MAIN LOOP */
	while(gettr(infd1,&tr)) puttr(outfd,&tr);

	while(gettr(infd2,&tr)) puttr(outfd,&tr);

	exit(0);
}
