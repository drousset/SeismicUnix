/*******************************************************************************
** PURPOSE:
*    To end plotting to a frame.
*
** OUTPUT ARGUMENTS:
*    nerr:  Error flag.  Set to 0 if no error occurs. (Pointer)
*
** SUBROUTINES CALLED:
*    flushbuffer5
*******************************************************************************/


endframe5(nerr)
  int *nerr;
{
  *nerr = 0;
  flushbuffer5(*nerr);

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    102695:  Original Version
*******************************************************************************/
