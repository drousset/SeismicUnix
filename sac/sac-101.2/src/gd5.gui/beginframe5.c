/*******************************************************************************
** PURPOSE:
*    To begin a frame to the GUI device.
*
** OUTPUT ARGUMENTS:
*    nerr:  Error flag.  Set to 0 if no error occurs. (Pointer)
*
** SUBROUTINES CALLED:
*    erase3_
*******************************************************************************/


beginframe5(nerr)
  int *nerr;
{
  *nerr = 0;
  
/* Erase the window */

  erase5();

/* Check for asynchronous events, in case of resize, etc. */

/*  dispatchevent3(nerr); */

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    102695:  Original Version
*******************************************************************************/
