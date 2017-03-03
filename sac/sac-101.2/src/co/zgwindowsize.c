/*
CDOC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 *** Function:	zgwindowsize_(number_rows, number_columns, error_flag)
 
 *** Purpose:	Get size of window being used for standard input.
 
 *** Inputs:	
		
 
 *** Returns: 
 
 *** Notes:	This function is to be called by a FORTRAN routine.  The
		'_' is appended to the name for compatibility with FORTRAN.
 
 *** History:	08/01/84	Under development--D. Trimmer
		08/01/84	Tested--D. Trimmer
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CEND
*/
 
#include <stdio.h>
#include "config.h"


#if GWINSZ_IN_SYS_IOCTL
# include <sys/ioctl.h>
#else 
# include <termios.h>
#endif

zgwindowsize_(number_rows, number_columns, error_flag)
 

long *number_rows, *number_columns, *error_flag;
 
{
  struct winsize ws;    /* system structure containing window info. */
 
  if( (*error_flag = ioctl(fileno(stdin),TIOCGWINSZ,&ws) ) == 0)  {
    *number_rows = (long) ws.ws_row;
    *number_columns = (long) ws.ws_col;
  }
  else
    *error_flag = 1;
  return;
}
 
 
