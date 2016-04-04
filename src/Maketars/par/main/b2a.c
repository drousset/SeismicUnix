/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* B2A: $Revision: 1.15 $ ; $Date: 2011/11/16 16:42:16 $	*/
#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" B2A - convert binary floats to ascii				",
" 								",
" b2a <stdin >stdout 						",
" 								",
" Required parameters:						",
" 	none							",
" 								",
" Optional parameters:						",
" 	n1=2		floats per line in output file 		",
" 								",
" 	outpar=/dev/tty output parameter file, contains the	",
"			number of lines (n=)			",
"                       other choices for outpar are: /dev/tty, ",
"                       /dev/stderr, or a name of a disk file   ",
" 								",
NULL};

/* Credits:
 *	CWP: Jack K. Cohen
 */
/**************** end self doc ***********************************/


int
main(int argc, char **argv)
{
	char *outpar;		/* name of file holding output parfile	*/
	FILE *outparfp;		/* ... its file pointer			*/
	int n1;			/* number of floats per line		*/
	size_t n1read;		/* number of items read			*/
	int n2 = 0;		/* number of lines in input file 	*/
	float *x;		/* binary floats			*/


	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(1);


	/* Get parameters and do set up */
	if (!getparstring("outpar", &outpar))	outpar = "/dev/tty" ;
	outparfp = efopen(outpar, "w");
	if (!getparint("n1", &n1))		n1 = 2;
	x = ealloc1float(n1);


        checkpars();

	/* Loop over data converting to ascii */
	while ((n1read = efread(x, FSIZE, n1, stdin))) {
		register int i1;

		if (n1read != n1)
			err("out of data in forming line #%d", n2+1);
		for (i1 = 0; i1 < n1; ++i1)  printf("%15.10e ", x[i1]);
		putchar('\n');
		++n2;
	}


	/* Make par file */
	fprintf(outparfp, "n=%d\n", n2);

	return(CWP_Exit());
}
