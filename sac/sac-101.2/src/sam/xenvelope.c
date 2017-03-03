/**
 * @file   xenvelope.c
 * 
 * @brief  Envelope Function
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#define	MINDATALEN	201
#define	MLENSCRATCH	4297

#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/sam.h"


/** 
 * 
 *  To parse and execute the action command ENVELOPE.
 *     This command computes the envelope of a function.
 * 
 *  @param nerr
 *     Error return flag
 *     - 0 on Success
 *     - Non-Zero on Error
 *
 * @note Local Variables
 *    - MLENSCRATCH:  Size of scratch space needed for transform. [ip]
 *    - ndxscratch:   Index in SACMEM array for scratch space. [i]
 *    - ndxhilbert:   Index in SACMEM array for Hilbert transform. [i]
 *    - ndxy:         Index in SACMEM array for current signal. [i]
 *
 * @date  890223:  Original version.
 * @date  890223:  Documented/Reviewed
 *
 * 
 */

void 
xenvelope(long int *nerr) {
	long int j, j_, jdfl, jdfl_, ndxhilbert, ndxscratch, ndxsignal, 
	 nlenmn, nlenmx, nlnsignal, notused, ntused;

        float *Sacmem1, *Sacmem2;

	*nerr = 0;

	/* - Test for a non-null data file list. */
	vflist( nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Make sure each file is an evenly spaced time series file. */
	vfeven( nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Determine minimum and maximum signal size.
	 *   Make sure minimum is not too small for fir filter subroutine.
	 *   Use maximum to allocate space for the Hilbert transform. */

	nlenmn = MLARGE;
	nlenmx = 0;
	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		jdfl_ = jdfl - 1;
		getfil( jdfl, FALSE, &ntused, &ntused, &ntused, nerr );
		if( *nerr != 0 )
			goto L_8888;
		nlenmn = min( nlenmn, *npts );
		nlenmx = max( nlenmx, *npts );
		}

	if( nlenmn < MINDATALEN ){
		*nerr = 1613;
		setmsg( "ERROR", *nerr );
		apimsg( MINDATALEN );
		//		outmsg();
		goto L_8888;
		}

	/* - EXECUTION PHASE: */

	/* - Allocate temporary blocks for output Hilbert transform. */

	allamb( &cmmem, nlenmx, &ndxhilbert, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Perform the requested function on each file in DFL. */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		jdfl_ = jdfl - 1;

		/* -- Get next file from the memory manager.
		 *    (Header is moved into common blocks CMHDR and KMHDR.) */
		getfil( jdfl, TRUE, &nlnsignal, &ndxsignal, &notused, nerr );
		if( *nerr != 0 )
			goto L_8888;

		envelope(nlnsignal,
			 cmmem.sacmem[ndxsignal],    /* Input  */
			 cmmem.sacmem[ndxhilbert]);  /* Output */

		/* Copy output signal back onto input */
		for(j = 0; j < nlnsignal; j++) {
		  cmmem.sacmem[ndxsignal][j] = cmmem.sacmem[ndxhilbert][j]; 
		}

		/* -- Update any header fields that may have changed. */
		extrma( cmmem.sacmem[ndxsignal], 1, nlnsignal, 
			depmin, depmax, depmen );

		/* -- Return file to memory manager. */
		putfil( jdfl, nerr );
		if( *nerr != 0 )
			goto L_8888;

		}

	/* - Release scratch space. */

	relamb( cmmem.sacmem, ndxhilbert, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Calculate and set new range of dependent variable. */
	setrng();

L_8888:
	return;

}

