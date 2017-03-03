/*
CDOC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 *** Function:	zsleep_(pmsec)					   D. Trimmer
 
 *** Purpose:	Sleep specified number of milliseconds
 
 *** Inputs:	pmsec	pointer to long containing # milliseconds to
 
 *** Returns:	none
 
 *** Notes:	This function is to be called by a FORTRAN routine.  The
		'_' is appended to the name for compatibility with FORTRAN.
 
		BSD 4.2 UNIX offers a sleep function in increments of one
		second, and is only accurate to +/- 1 second.  Therefore,
		this software sleep was written to provide smaller sleeps.
 
                It is used by the graphics library for delays when
                performing certain graphics operations to the terminal.

 *** Bugs:	If the system is busy, the actual sleep may be much longer
		than that specified, since it will take longer to complete
		the loop.  The loop needs to be tuned for machines with
		different speeds.
 
 *** History:	07/26/84	Under development--D. Trimmer
		07/26/84	Tested--D. Trimmer
		12/19/84        Tuned for ROS 3.2/RIDGE 32--D. Trimmer
	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CEND
*/

#include "config.h"
 
#ifndef READLINE
zsleep(pmsec)
long *pmsec;
 
{
	long i;		/* index */
	long j;		/* index */
 
	

	for (i=0;i<*pmsec;++i)
		for (j=0;j<364;++j);		/* 1 millisecond loop */
	return;
}
 
#else 

#include <sys/time.h>
#include <stdio.h>
#include "../../inc/select.h"

#define TIMER_SET          0
#define TIMER_GET          1
#define TIMER_SEC_TO_USEC  1000000
#define TIMER_MSEC_TO_USEC 1000
#define ZSLEEP_PRMTLEN     2
#define ZSLEEP_MSGLEN      9


static 
void process_line(char *p) { 
  select_loop_continue(SELECT_OFF);
  select_loop_message(p, SELECT_MSG_SET);
  return; 
}

int
zsleep(long timeout) {

  int left;
  struct timeval time;
  char prmt[ZSLEEP_PRMTLEN];
  char msg[ZSLEEP_MSGLEN];

  prmt[0] = '$';
  prmt[1] = '\0';

  /* Convert milliseconds to microseconds */
  timeout = timeout * TIMER_MSEC_TO_USEC; 
  if((left = timer(TIMER_SET, timeout)) == 0) {
    return(0);
  }
  time.tv_sec = 0;
  time.tv_usec = left;
  
  while((left = timer(TIMER_GET, timeout)) > 0) {
    time.tv_sec = 0;
    time.tv_usec = left;
    select_loop(prmt, ZSLEEP_PRMTLEN, 
		msg, ZSLEEP_MSGLEN, 
		&time, process_line);
  }
  
  fprintf(stdout,"\n");
  rl_callback_handler_remove(); /* Returns Prompt */
  
}


int 
timer(int set_get, int usec) {
  static struct timeval start;
  struct timeval now;
  int delta;

  if(set_get == TIMER_SET) {
    if(gettimeofday(&start, NULL) != 0) {
      perror("gettimeofday");
    } 
  }

  if(gettimeofday(&now, NULL) != 0) {
    perror("gettimeofday");
  } 
  delta = (now.tv_sec - start.tv_sec) * TIMER_SEC_TO_USEC + 
    (now.tv_usec - start.tv_usec);

  return(usec - delta);
}

#endif /* READLINE */
