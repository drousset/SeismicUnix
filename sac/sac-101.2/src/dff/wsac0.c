#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "hdr.h"
void /*FUNCTION*/ wsac0(kname, xarray, yarray, nerr, kname_s)
char *kname;   long int kname_s;
float *xarray, *yarray;
long int *nerr;
{
	long int ncerr, nderr, nlcdsk, nun;
	float temp[MKMHDR], temp2[FILEMKMHDR];

	char *kname_c;
	kname_c = fstrdup(kname, kname_s);
	kname_s = strlen(kname_c) + 1;


	/*=====================================================================
	 * PURPOSE: To write a SAC file to disk using current header values.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *     KNAME:  Name of disk file to write. [c]
	 *             The name should be blank filled.
	 *    XARRAY:  Array containing independent variable. [fa]
	 *             This is not used if the data is evenly spaced.
	 *    YARRAY:  Array containing dependent variable. [fa]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      NERR:  Error return flag 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MUNOUT
	 *    HDR:     FHDR
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  ZGTFUN, ZNFILE, ZWABS, ZCLOSE, ZPUTC, WRTMSG
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    TEMP:    Array used while writing header character data. [fa]
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Create the requested file and open it. */
	zdest( kname_c,kname_s, &nderr );
	znfile( &nun, kname_c,kname_s, "DATA",5, nerr );

	if( *nerr != 0 )
	    goto L_8888;

	/* - Write the header to disk starting at word 0. */

	nlcdsk = 0;
	zwabs((int*) &nun, (char *)cmhdr.fhdr, MCMHDR, (int*)&nlcdsk, (int*)nerr );
	if( *nerr != 0 )
	    goto L_8888;
	nlcdsk = nlcdsk + MCMHDR;
	zputc( (char *)kmhdr.khdr, 9, (long int *)temp, (MCPW+1)*MKHDR );

        map_chdr_out(temp,temp2);

	zwabs( (int*)&nun, (char *)temp2, FILEMKMHDR, (int*)&nlcdsk, (int*)nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Write the array containing the dependent variable to disk
	 *   starting after the end of the header. */

	nlcdsk = nlcdsk + FILEMKMHDR;
	zwabs( (int*)&nun, (char *)yarray, *npts, (int*)&nlcdsk, (int *)nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - If the data is not evenly spaced, write the array
	 *   containing the independent variable. */

	if( !*leven ){
	    nlcdsk = nlcdsk + *npts;
	    zwabs( (int *)&nun, (char *)xarray, *npts, (int *)&nlcdsk, (int *)nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}

	/* - Write any error message to terminal, close the disk file, and return. */

L_8888:
	if( *nerr != 0 )
	    outmsg();
	zclose( &nun, &ncerr );
	free(kname_c);
	kname_c = NULL;

	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    961031:  ninf and nhst were changed to norid and nevid for
	 *             compatability with the CSS format.  maf 961031
	 *    870513:  Changed call to wrtxtd to wrtmsg.
	 *    840118:  Deleted call to ZTRUNC.
	 *    830125:  Changes due to modified header common block.
	 *    820118:  Added logic to truncate file before closing.
	 *    810120:  Changed to output message retrieval from disk.
	 *    800821:  Original version [Prime].
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850307
	 *===================================================================== */

} /* end of function */




/* Wrapper to make the function more convenient for FORTRAN programmers. */

void wsac0_ (char      *kname, 
	     float     *xarray,
	     float     *yarray, 
	     long int  *nerr, 
	     long int   kname_s) {
  wsac0 ( kname , xarray , yarray , nerr , kname_s ) ;
}
void wsac0__ (char      *kname, 
	      float     *xarray,
	      float     *yarray, 
	      long int  *nerr, 
	      long int   kname_s) {
  wsac0 ( kname , xarray , yarray , nerr , kname_s ) ;
}
