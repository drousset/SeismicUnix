/* Copyright (c) Colorado School of Mines, 1995.*/
/* All rights reserved.                       */

/* DSUFGETTR: $Revision: 1.00 $; $Date: 95/08/01 13:36:22 $	*/


/*********************** self documentation **********************/
/****************************************************************************
DSUFGETTR - Routines to get an SU trace from a file 

fgettr		get a segy trace from a file by file pointer (nt constant)
gettr		macro using fgettr to get a trace from stdin (nt constant)
fvgettr		get a segy trace from a file by file pointer (nt may vary)
vgettr		macro using fgettr to get a trace from stdin (nt may vary_
 
*****************************************************************************
Function Prototype:
int dsufgettr(FILE *fp, segy *tp);
int dsufvgettr(FILE *fp, segy *tp);

*****************************************************************************
Returns:
int: number of bytes read on current trace (0 after last trace)
 
*****************************************************************************
Authors: CWP: Alejandro E. Murillo
Authors: SEP: Einar Kjartansson, Stew Levin CWP: Shuki Ronen, Jack Cohen
****************************************************************************/
/**************** end self doc ********************************/

#include "su.h"
#include "segy.h"
#include "header.h"


int dsufgettr(FILE *fp, segy *tp)
{
  static unsigned long itr = 0;	/* number of traces read	*/
  static cwp_Bool first = true;	/* to check if first entry	*/
  static int nsfirst;		/* samples from 1st header	*/
  static int nsegy; 		/* segy bytes from nsfirst	*/
  static FileType ftype;		/* file type of input *fp	*/
  int nread;			/* bytes seen by fread calls	*/
  extern	DsuTask ThisDsuTask;


  if (first) {

    unsigned short bytesper; /* bytes per datum (packed?)	*/
    unsigned int databytes;	/* bytes from nsfirst		*/

/*
    fprintf(stderr, "\t\tDSUFGETTR (%d) first trace\n", itr);
    fflush(stderr);
*/

    first = false;

    /* Get the header */
    switch (nread =  RecvOneInt(ThisDsuTask.parent_tid, MsgHdrSize)) {
      case MSGERR:   
      case MSGEOF:   

	if (nread == MSGERR)
          fprintf( stderr, "Error signal received. Aborting execution \n");
	else
          fprintf( stderr, "EOF signal received.\n");
        return 0; /* no more traces */

      default:  
        if (nread != HDRBYTES) 
	  err("%s: bad first header", __FILE__);
        nread = RecvBBytes((void *)tp, HDRBYTES, -1, MsgHdr);
	break;
    }

    /* Have the header, now for the data */

    nsfirst = tp->ns;
    if (nsfirst > SU_NFLTS)
      err("%s: unable to handle %d > %d samples per trace",
	__FILE__, nsfirst, SU_NFLTS);

    if      (tp->trid==CHARPACK)   bytesper=sizeof(char);
    else if (tp->trid==SHORTPACK)  bytesper=sizeof(short);
    else			   bytesper=sizeof(float);

    databytes = bytesper * nsfirst;
    nsegy = HDRBYTES + databytes;

    switch (nread = RecvOneInt(ThisDsuTask.parent_tid, MsgDataSize)) {

      case MSGERR:
      case MSGEOF:   err("%s: no data on first trace", __FILE__);
      default:  
	if (nread != databytes)
      	  err("%s: first trace: read only %d bytes of %u",
             __FILE__, nread, databytes);
        nread = RecvBBytes((void *)tp->data, databytes, -1, MsgData);
        break;

    }

  } else { /* Not first entry */

    switch (nread =  RecvOneInt(ThisDsuTask.parent_tid, MsgHdrSize)) {

      case MSGERR:
      case MSGEOF:

	if (nread == MSGERR)
          fprintf( stderr, "Error signal received. Aborting execution \n");
	else
          fprintf( stderr, "EOF signal received.\n");
        return 0; /* no more traces */

      default:  
        if (nread != HDRBYTES)
          err("%s: read trace #%ld", __FILE__, itr);
        nread = RecvBBytes((void *)tp, nread, -1, MsgHdr);
        nread = RecvOneInt(ThisDsuTask.parent_tid, MsgDataSize);
        nread = RecvBBytes((void *)tp->data, nread,-1, MsgData);
        break;
    }

    if (tp->ns != nsfirst)
      err("%s: on trace #%ld, "
          "number of samples in header (%d) "
          "differs from number for first trace (%d)", 
          __FILE__, itr, tp->ns, nsfirst);
  }

  ++itr;

/*
  if ( !(itr%10) ) {
    fprintf(stderr, "\t\tDSUFGETTR (%d) received\n", itr);
    fflush(stderr);
  }
*/

  return nsegy;
}

int dsufvgettr(FILE *fp, segy *tp)
/************************************************************************* 
  fvgettr - GET a segy TRace from a file by File pointer (nt can Vary)
************************************************************************** 
Input:
fp	input file pointer

Output:
tp	pointer to a type segy

Returns:
	int: number of bytes read on current trace (0 after last trace)

************************************************************************** 
Notes: Cloned from .../su/lib/fgettr.c   
Macro defined in segy.h :	#define vgettr(x)	fgettr(stdin, (x))
************************************************************************** 
Credits: CWP: Jack K. Cohen, Michel Dietrich 
**************************************************************************/
{
	static unsigned long itr = 0;	/* number of traces read	*/
	static cwp_Bool first = true;	/* to check if first entry	*/
	static FileType ftype;		/* file type of input *fp	*/
	static unsigned short bytesper;	/* bytes per datum (packed?)	*/
	int databytes;			/* data bytes on current segy	*/
	int nt;				/* samples on trace (can vary)	*/
	int nsegy; 			/* total bytes on current segy	*/
	int nread;			/* bytes seen by fread calls	*/
	extern	DsuTask ThisDsuTask;



	if (first) {
		first = false;

		switch (ftype = filestat(fileno(fp))) {
		case DIRECTORY:
			err("%s: stdin can't be a directory", __FILE__);
		case TTY:
			err("%s: stdin can't be tty", __FILE__);
		default:
			if      (tp->trid==CHARPACK)   bytesper=sizeof(char);
			else if (tp->trid==SHORTPACK)  bytesper=sizeof(short);
			else			       bytesper=sizeof(float);
		}
	}

	/* Get the header */
	switch (nread = RecvOneInt(ThisDsuTask.parent_tid, MsgHdrSize)) {
	  case MSGERR:
	  case MSGEOF: return 0; /* no more traces */
	  default:  
	    if (nread != HDRBYTES)
	      err("%s: bad header, trace #%ld (%d)", __FILE__, itr, nread);
	    nread = RecvBBytes((void *)tp, HDRBYTES, -1, -1);
	    break;
	}

	/* Have the header, now for the data */
	nt = tp->ns;
	if (nt > SU_NFLTS)
		err("%s: unable to handle %d > %d "
			"samples per trace", __FILE__, nt, SU_NFLTS);

	databytes = bytesper * nt;
	nsegy = HDRBYTES + databytes;

	switch (nread = RecvOneInt(ThisDsuTask.parent_tid, MsgHdrSize)) {
	case MSGERR:
	case MSGEOF:   err("%s: no data on trace #%ld, itr", __FILE__);
	default:  if (nread != databytes)
			 err("%s: trace #%ld, read only %d bytes of %u",
				__FILE__, itr, nread, databytes);
		  nread = RecvBBytes((void *)tp->data, databytes, -1, -1);
	break;
	}

	++itr;
/*
	if ( !(itr%100) ) {
	  fprintf(stderr, "\t\tDSUVgettr(%d) received\n", itr);
	  fflush(stderr);
	}
*/
	return nsegy;
}
