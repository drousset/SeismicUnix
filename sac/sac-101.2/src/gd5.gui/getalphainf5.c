/*******************************************************************************
** PURPOSE:
*    To get alphanumeric information.
*
** OUTPUT ARGUMENTS:
*    num_lines:     Number of lines of text in window. (Pointer)
*    erase:         Erase character.
*    erase_length:  Length of erase.
*
** SUBROUTINES CALLED:
*    zgwindowsize
*
** LOCAL VARIABLES:
*    pname:  Device name of window. (Pointer)
*    npix:   Height of window. (pixels)
*    fd:     File descriptor for window device.
*******************************************************************************/


getalphainfo5(num_lines, erase, erase_length)
  long *num_lines;
  char erase[];
  long erase_length;
{ 
  long num_columns, error_flag;

  erase[0] = ' ';

/* Get size of window. */

  zgwindowsize_( num_lines, &num_columns, &error_flag );
  if( error_flag != 0) {
    *num_lines = 20;
  }

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    890828:  Modified to use new co/zgwindowsize routine. (J. Tull)
*    861205:  Original version. (B. Hickman)
*******************************************************************************/
