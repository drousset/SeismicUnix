/* FGETTRA: $Revision: 1.6 $; $Date: 90/12/23 17:03:09 $	*/

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

/* fgettra - get disk trace by trace number
 *
 * Returns:
 *	integer number of traces in the disk file
 *
 * Synopsis:
 *	#include "segy.h"
 *	fgettra(FILE *fp, segy *traceptr, int itr)
 *
 *	gettra(tp, itr) is a macro for fgettra(stdin, tp, itr)
 *
 * Credits:
 *	SEP: Einar
 *	CWP: Jack
 *
 */


int fgettra(FILE *fp, segy *tp, int itr)
{
	static int nsfirst;		/* samples on first trace	*/
	static int nsegy;		/* size of trace in bytes	*/
	static int ntr;			/* number of traces in file	*/
	static bool first=true;		/* flag for first entry		*/
	int nread;			/* bytes read			*/
	static char card[EBCBYTES];
	static char bhdr[BNYBYTES];
	static int hdbytes;

	if (first) {	/* first entry; get number of traces */
		unsigned short bytesper;/* bytes per float (packed?)	*/
		long length;		/* length of file in bytes	*/
		filetype ftype = filestat(fileno(fp));

		first = false;

		switch(ftype) {
		case TTY:
			warn("stdin not redirected");
			selfdoc();
		break;
		case DISK:	/* correct */
		break;
		default:
			err("%s: input must be disk file", __FILE__);
		break;
		}
		
		efread(card, 1, 10, fp);
                if (strncmp(card, "C 1 CLIENT",10)==0 ) {
			efseek(fp, EBCBYTES+BNYBYTES, 0);
			hdbytes = EBCBYTES + BNYBYTES;
		}
		else {
			efseek(fp, 0, 0);
			hdbytes = 0;
			
		}
		   
		
		if (HDRBYTES != (nread = efread(tp, 1, HDRBYTES, fp))) {
			err("%s: read only %d bytes of header",
							__FILE__, nread);
		}

		nsfirst = tp->ns;
		if (nsfirst > SU_NFLTS) {
			err("%s: trace too long: nsfirst=%d>SU_NFLTS=%d",
				__FILE__, nsfirst, SU_NFLTS);
		}

		if      (tp->trid == CHARPACK)  bytesper = sizeof(char);
		else if (tp->trid == SHORTPACK) bytesper = sizeof(short);
		else                            bytesper = sizeof(float);

		nsegy = HDRBYTES + nsfirst * bytesper;
		efseek(fp, 0, SEEK_END);
		length = eftell(fp);
		ntr = (length-hdbytes) / nsegy;

	} /* end first entry */


	/* Check on requested trace number */
	if (itr >= ntr)  err("%s, trying to read off end of file", __FILE__);


	/* Position file pointer at start of requested trace */
	efseek(fp, itr*nsegy+hdbytes, SEEK_SET);

	nread = efread(tp, 1, nsegy, fp);
	if (nread != nsegy)
		err("%s: read only %d of %d bytes in trace",
						__FILE__, nread, nsegy);

	if (tp->ns != nsfirst)
	    warn("%s: header ns field = %d differs from first trace = %d",
						__FILE__, tp->ns, nsfirst);

	return ntr;
}
