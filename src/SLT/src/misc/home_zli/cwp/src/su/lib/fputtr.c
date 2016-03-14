/* FPUTTR: $Revision: 1.8 $; $Date: 91/01/10 09:35:49 $	*/

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
	static bool first = true;	/* to check if first entry	*/
	extern char SU_chdr[];
	extern char SU_bhdr[];



	if (first) {	/* First entry */
		unsigned short bytesper;/* bytes per datum (packed?)	*/
		filetype ftype = filestat(fileno(fp));

		first = false;


		switch (ftype) {
		case DIRECTORY:
			err("fputtr: stdout can't be a directory");
		case TTY:
			err("fputtr: stdout can't be tty");
		default: /* OK */
		break;
		}

		if      (tp->trid == CHARPACK)	bytesper = sizeof(char);
		else if (tp->trid == SHORTPACK) bytesper = sizeof(short);
		else				bytesper = sizeof(float);

		nsegy = HDRBYTES + bytesper * tp->ns;
		fflush(stderr);
		if (strncmp(SU_chdr, "C 1 CLIENT",10)==0) {
			 efwrite(SU_chdr, 1, EBCBYTES, fp);
			 efwrite(SU_bhdr, 1, BNYBYTES, fp);
		/* zero out the SU_chdr and SU_bhdr buffers */
			 bzero(SU_chdr, EBCBYTES);
			 bzero(SU_bhdr, BNYBYTES);
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
