#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
void /*FUNCTION*/ wsac2(kname, yarray, nlen, xarray, nerr, kname_s)
char *kname;   long int kname_s;
float yarray[];
long int *nlen;
float xarray[];
long int *nerr;
{

	float *const Xarray = &xarray[0] - 1;
	float *const Yarray = &yarray[0] - 1;


	/*=====================================================================
	 * PURPOSE: To write an unevenly spaced or spectral SAC file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *     KNAME:  Name of disk file to write. [c]
	 *             The name should be blank filled.
	 *    YARRAY:  Array containing the dependent variable. [fa]
	 *      NLEN:  Length of YARRAY and XARRAY. [i]
	 *    XARRAY:  Array containing the independent variable. [fa]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      NERR:  Error return flag, 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL: DFM/4
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    MACH:    MUNOUT
	 *    HDR:     NPTS, B, E, LEVEN
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INIHDR, INILHF, INIMSG, NEWHDR, WSAC0, WRTMSG
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870902:  Added calls to INILHF and INIMSG as part of initialization.
	 *    870513:  Changed call to wrtxtd to wrtmsg.
	 *    800820:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850307
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Initialize some common blocks if not already done. */

	if( cmhdr.fundef != -12345. ){
	    inihdr();
	    inilhf();
	    inimsg();
	}

	/* - Initialize all header fields to their default values. */

	newhdr();

	/* - Set up the header fields passed by the calling program. */

	*npts = *nlen;
	*begin = Xarray[1];
	*ennd = Xarray[*nlen];
	*leven = FALSE;

	/* - Write the file to disk. */

	wsac0( kname, xarray, yarray, nerr, kname_s );

L_8888:
	if( *nerr != 0 )
	    outmsg();
	return;

} /* end of function */




/* Wrapper to make the function more convenient for FORTRAN programmers. */

void wsac2_ (char      *kname, 
	     float      yarray[], 
	     long int  *nlen, 
	     float      xarray[], 
	     long int  *nerr, 
	     long int   kname_s) {
  wsac2 ( kname , yarray , nlen , xarray , nerr , kname_s ) ;
}
void wsac2__ (char      *kname, 
	      float      yarray[], 
	      long int  *nlen, 
	      float      xarray[], 
	      long int  *nerr, 
	      long int   kname_s) {
  wsac2 ( kname , yarray , nlen , xarray , nerr , kname_s ) ;
}
