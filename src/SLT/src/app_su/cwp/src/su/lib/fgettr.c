/* FGETTR: $Revision: 1.1 $; $Date: 2004/11/10 19:44:00 $	*/

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

/* fgettr - get a segy trace from a file by file pointer
 * gettr  - get a segy trace from stdin
 *
 * Returns:
 *	int: number of bytes read on current trace (0 after last trace)
 *
 * Synopsis:
 *	int fgettr(FILE *fp, segy *tp)
 *
 *	gettr(tp) is a macro for fgettr(stdin, tp)
 *
 * Example:
 *	segy tr;
 *	...
 *	while (gettr(&tr)) {
 *		tr.offset = abs(tr.offset);
 *		puttr(&tr);
 *	}
 *	...
 *
 * Credits:
 *	SEP: Einar, Stew
 *	CWP: Shuki, Jack
 *
 *            : Zhiming Li and J. Dulac,   allowed id headers passed among
 *					   su programs
 */
char SU_chdr[EBCBYTES];
char SU_bhdr[BNYBYTES];

static unsigned short fubar[120];

int fgettr(FILE *fp, segy *tp)
{
	static unsigned long itr = 0;	/* number of traces read	*/
	static bool first = true;	/* to check if first entry	*/
	static int nsfirst;		/* samples from 1st header	*/
	static int nsegy; 		/* segy bytes from nsfirst	*/
	static filetype ftype;		/* file type			*/
	int nread;			/* bytes read			*/
	int n10;
	extern char SU_chdr[];
	extern char SU_bhdr[];
	


	if (first) {


		unsigned short bytesper; /* bytes per datum (packed?)	*/
		unsigned int databytes;	/* bytes from nsfirst		*/

		first = false;

		switch (ftype = filestat(fileno(fp))) {
		case DIRECTORY:
			err("fgettr: stdin can't be a directory");
		case TTY:
			err("fgettr: stdin can't be tty");
		default:
			/* Get tape header if any */
			nread = efread(SU_chdr, 1, 10, fp);
			if (strncmp(SU_chdr, "C 1 CLIENT",10)==0 ) {
			   	nread=efread(&SU_chdr[10], 1, EBCBYTES-10, fp);
			   	nread=efread(SU_bhdr, 1, BNYBYTES, fp);
				n10 = 0;
			}
			else {
				memcpy((char*)tp,SU_chdr,10);
				n10 = 10;
			}
			/* read in trace */
			/* Get the header */
			switch ( 
			   nread=efread(((char *)tp)+n10,1,HDRBYTES-n10,fp) ){
			case 0:   err("fgettr: no traces");
			default:  if (nread+n10 != HDRBYTES)
					err("fgettr: bad first header");
			break;
			}

                        memcpy( (char*)fubar ,(char*)tp ,240 );

			/* Have the header, now for the data */
			nsfirst = tp->ns;
			if (nsfirst > SU_NFLTS)
				err("fgettr: unable to handle %d > %d "
				    "samples per trace", nsfirst, SU_NFLTS);

			if      (tp->trid==CHARPACK)   bytesper=sizeof(char);
			else if (tp->trid==SHORTPACK)  bytesper=sizeof(short);
			else			       bytesper=sizeof(float);

			databytes = bytesper * nsfirst;
			nsegy = HDRBYTES + databytes;

			switch (nread = efread(tp->data, 1, databytes, fp)) {
			case 0:   err("fgettr: no data on first trace");
			default:  if (nread != databytes)
					 err("fgettr: first disktrace: "
					     "read only %d bytes of %u",
					      nread, databytes);
			break;
			}
		break;
		}

	} else { /* Not first entry */

		switch (nread = efread(tp, 1, nsegy, fp)) {
		case 0:   return 0; /* finished */
		default:  if (nread != nsegy)
			      err("fgettr: disk read trace #%ld", itr);
		break;
		}

		if (tp->ns != nsfirst)
			err("fgettr: on trace #%ld, "
			    "number of samples in header (%d) %s "
			    "differs from number for first trace (%d)", 
			     itr, tp->ns, nsfirst);
	}

	++itr;
	return nsegy;
}

void dmphdr( segy* tr ){

   FILE* dbg;

   dbg=fopen( "dbg.dat" ,"w" );
   fwrite( tr ,1 ,240 ,dbg );
   fclose(dbg);
}


#ifdef TEST

/*********************** self documentation **********************/
string sdoc = "\
								\n\
tgettr <stdin >stdout						\n\
								\n\
	Test harness for gettr.c				\n\
	Changes cdp to abs(cdp)					\n\
	Contrast the following results:	 			\n\
	suplane offset=-100 | sugethw offset 			\n\
	suplane offset=-100 | tgettr | sugethw offset		\n\
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
