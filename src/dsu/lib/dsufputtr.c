/* Copyright (c) Colorado School of Mines, 1995.*/
/* All rights reserved.                       */

/* FPUTTR: $Revision: 1.00 $; $Date: 95/08/01 14:19:40 $	*/

/*********************** self documentation **********************/
/************************************************************************
DSUFPUTTR - Functions to put an SU trace on a file by descriptor

dsufputtr		put a segy trace on a file by descriptor
dsuputtr		put a segy trace on stdout

*************************************************************************
Function prototype:
void dsufputtr(FILE *fp, segy *tp);

*************************************************************************
Returns:
	void

*************************************************************************
Notes:
The macro "puttr" is defined in .../include/su.h as:
#define puttr(x)	fputtr(stdout, (x))


*************************************************************************
Authors: CWP: Alejandro E. Murillo
Authors: SEP: Einar Kjartansson CWP: Jack K. Cohen, Shuki Ronen
************************************************************************/
/**************** end self doc ********************************/

#include "su.h"
#include "segy.h"
#include "header.h"

void dsufputtr(FILE *fp, segy *tp)
{
  static int itr = 0;		/* number of traces sent	*/
  static int nsegy;		/* number of bytes output	*/
  static cwp_Bool first = true;	/* to check if first entry	*/
  int	i, HdrSize, DataSize;
  extern  DsuTask ThisDsuTask;


  if (first) {	/* First entry */

    unsigned short bytesper; /* bytes per datum (packed?)	*/
    FileType ftype = filestat(fileno(fp));

/* DEBUG
    fprintf(stderr, "\t\tDSUfputtr (%d) first trace\n", itr);
    fflush(stderr);
*/

    first = false;

    if      (tp->trid == CHARPACK)	bytesper = sizeof(char);
    else if (tp->trid == SHORTPACK) bytesper = sizeof(short);
    else				bytesper = sizeof(float);

    nsegy = HDRBYTES + bytesper * tp->ns;
    HdrSize  = HDRBYTES;
    DataSize = nsegy - HDRBYTES;

    for (i = 0; i < MAX_LINK; i++)

      if (ThisDsuTask.branch_tid[i] > 0) {

	SendInt(&HdrSize, 1, ThisDsuTask.branch_tid[i], MsgHdrSize);
	SendBBytes((void *)tp,HDRBYTES, ThisDsuTask.branch_tid[i], MsgHdr);
	SendInt(&DataSize, 1, ThisDsuTask.branch_tid[i], MsgDataSize);
	SendBBytes((void *)tp -> data,DataSize, 
		ThisDsuTask.branch_tid[i], MsgData);
      }

    return;
  }

  HdrSize  = HDRBYTES;
  DataSize = nsegy - HDRBYTES;

  for (i = 0; i < MAX_LINK; i++)

    if (ThisDsuTask.branch_tid[i] > 0) {

      SendInt(&HdrSize, 1, ThisDsuTask.branch_tid[i], MsgHdrSize);
      SendBBytes((void *)tp, HDRBYTES , ThisDsuTask.branch_tid[i], MsgHdr);
      SendInt(&DataSize, 1, ThisDsuTask.branch_tid[i], MsgDataSize);
      SendBBytes((void *)tp -> data, DataSize, 
		ThisDsuTask.branch_tid[i], MsgData);

    }

  itr++;

/* DEBUG
  if ( !(itr%10) ) {
    fprintf(stderr, "\t\tDSUfputtr (%d) called\n", itr);
    fflush(stderr);
  }
*/

  return;
}
