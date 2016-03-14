#include "su.h"
#include "segy.h"
#include "header.h"

/* fgtra64 - get disk trace by trace number when file size > 2G
 *
 * Returns:
 *	integer number of traces in the disk file
 *
 * Synopsis:
 *	#include "segy.h"
 *	fgtra64(FILE *fp, segy *traceptr, int itr)
 *
 *	gtra64(tp, itr) is a macro for fgtra64(stdin, tp, itr)
 *
 * Credits:
 *      zhiming li 	      
 *
 */


int fgtra64(FILE *fp, segy *tp, int itr)
{
	static int nsfirst;		/* samples on first trace	*/
	static int nsegy;		/* size of trace in bytes	*/
	static int ntr;			/* number of traces in file	*/
	static bool first=true;		/* flag for first entry		*/
	int nread;			/* bytes read			*/
	static char card[EBCBYTES];
	static char bhdr[BNYBYTES];
	static int hdbytes;

	off_t lofset;
	long long ilonglong;

	if (first) {	/* first entry; get number of traces */
		unsigned short bytesper;/* bytes per float (packed?)	*/
		long long length;	/* length of file in bytes	*/

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
/*
			err("%s: input must be disk file", __FILE__);
*/
		break;
		}
		
		efread(card, 1, 10, fp);
                if (strncmp(card, "C 1 CLIENT",10)==0 ) {
			ilonglong = EBCBYTES+BNYBYTES;
			bcopy(&ilonglong,&lofset,8);
			fseek64(fp, lofset, 0);
			hdbytes = EBCBYTES + BNYBYTES;
		} else {
			ilonglong = 0;
			bcopy(&ilonglong,&lofset,8);
			fseek64(fp, lofset, 0);
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
		ilonglong = 0;
		bcopy(&ilonglong,&lofset,8);
		fseek64(fp, lofset, SEEK_END);
		lofset = ftell64(fp);
		bcopy(&lofset,&length,8);
		length = (length-hdbytes)/nsegy;
		ntr = length;

	} /* end first entry */


	/* Check on requested trace number */
	if (itr >= ntr)  err("%s, trying to read off end of file", __FILE__);


	/* Position file pointer at start of requested trace */
	ilonglong = itr;
	ilonglong = ilonglong*nsegy + hdbytes;
	bcopy(&ilonglong,&lofset,8);
	fseek64(fp, lofset, SEEK_SET);

	nread = efread(tp, 1, nsegy, fp);
	if (nread != nsegy)
		err("%s: read only %d of %d bytes in trace",
						__FILE__, nread, nsegy);

	if (tp->ns != nsfirst)
	    warn("%s: header ns field = %d differs from first trace = %d",
						__FILE__, tp->ns, nsfirst);

	return ntr;
}
