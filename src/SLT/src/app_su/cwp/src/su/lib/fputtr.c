/* FPUTTR: $Revision: 1.3 $; $Date: 2004/11/30 17:46:52 $	*/

/*----------------------------------------------------------------------
 * Copyright (c) Colorado School of Mines, 1989.
 * All rights reserved.
 *
 * This code is part of SU.  SU stands for Seismic Unix, a processing line
 * developed at the Colorado School of Mines, partially based on Stanford
 * Exploration Project (SEP) software.  Inquiries should be addressed to:
 *
 *  Jack K. Cohen, Center for Wave Phenomena, Colorado School of Mines,
 *  Golden, CO 80401  (jkc@dix.mines.colorado.edu)
 *----------------------------------------------------------------------
 */

#include "su.h"
#include "segy.h"
#include "header.h"

/* fputtr - put a segy trace on a file by descriptor
 * puttr  - put a segy trace on stdout
 *
 * Returns:
 *	void
 *
 * Synopsis:
 *	void fputtr(FILE *fp, segy *tp)
 *
 *	puttr(tp) is a macro for fputtr(stdout, tp)
 *
 * Example:
 *	segy tr;
 *	...
 *	while (gettr(&tr)) {
 *		tr.offset = abs(tr.offset);
 *		puttr(&tr);
 *	}
 *
 * Credits:
 *	SEP: Einar
 *	CWP: Jack, Shuki
 *            :  Zhiming Li and J. Dulac,     allowed id headers passed
 *					      among su programs
 *
 */


void fputtr(FILE *fp, segy *tp)
{
	static int nsegy;		/* number of bytes output	*/
        static int first = 1;

	extern char SU_chdr[];
	extern char SU_bhdr[];



	if ( (first && fp == stdout ) || ftello(fp) == 0 ) { /* First entry */
		unsigned short bytesper;/* bytes per datum (packed?)	*/
		filetype ftype = filestat(fileno(fp));

		switch (ftype) {
		case DIRECTORY:
			err("fputtr: stdout can't be a directory");
		case TTY:
			err("fputtr: stdout can't be tty");
		default: /* OK */
		break;
		}

                if( first && fp == stdout ){
                   first = 0;
                }

		if      (tp->trid == CHARPACK)	bytesper = sizeof(char);
		else if (tp->trid == SHORTPACK) bytesper = sizeof(short);
		else				bytesper = sizeof(float);

		nsegy = HDRBYTES + bytesper * tp->ns;
		fflush(stderr);

                /* make sure that the binary header matches the data */
                *(short*)(&SU_bhdr[16]) = tp->dt;
                *(short*)(&SU_bhdr[20]) = tp->ns;

		if (strncmp(SU_chdr, "C 1 CLIENT",10)==0) {
			 efwrite(SU_chdr, 1, EBCBYTES, fp);
			 efwrite(SU_bhdr, 1, BNYBYTES, fp);
		}

	}

	efwrite(tp, 1, nsegy, fp);
	return;
}



#ifdef TEST

/*********************** self documentation **********************/
char *sdoc = "\
								\n\
tputtr <stdin >stdout						\n\
								\n\
	Test harness for puttr.c				\n\
	Changes cdp to abs(cdp)					\n\
	Contrast the following results:	 			\n\
	suwind <DATA count=10 | sugethw cdp 			\n\
	suwind <DATA count=10 | tputtr | sugethw cdp 		\n\
								\n\
";
/*****************************************************************/

segy tr;

main(int argc, char **argv)
{
	initargs(argc, argv);
	askdoc(1);

 	while (gettr(&tr)) {
 		tr.offset = abs(tr.offset);
 		puttr(&tr);
 	}

	return EXIT_SUCCESS;
}
#endif
