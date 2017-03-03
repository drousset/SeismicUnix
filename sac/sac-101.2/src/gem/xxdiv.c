/** 
 * @file   xxdiv.c
 * 
 * @brief  Control the X Divisions
 * 
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "gem.h"

void
xdiv_nice(int flag) {
  cmgem.lxdiv  = (flag == TRUE) ? FALSE : TRUE; /* X Division "Increment"; spacing between divisons: cmgem.xdiv  */
  cmgem.lnxdiv = (flag == TRUE) ? FALSE : TRUE; /* X Division "Number"; number of divisions: cmgem.lnxdiv        */
}

void 
xdiv_increment(float z) {
  cmgem.lxdiv  = TRUE;
  cmgem.lnxdiv = FALSE;
  cmgem.xdiv   = z;    /* Spacing between Divisions */
}

void
xdiv_number(int n) {
  cmgem.lxdiv = FALSE;
  cmgem.lnxdiv = TRUE;
  cmgem.nxdiv = n;  /* Number of Divisions */
}

void
xdiv_power(int flag) {
  cmgem.lxpowr = flag; /* Division displayed as a power */
}

/** 
 * Parse the command "xdiv" and set the X Divisions
 * 
 * @param nerr 
 *   Error Return Flag
 *   - 0 on Success
 *   - Non-Zero on Error
 *
 * @date   840531:  Added power labeling flag option.
 * @date   820610:  Original version (from GEMCOM.)
 *
 */
void 
xxdiv(long int *nerr) {


	*nerr = 0;

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- Set up nice numbering. */
		if( lckey( "NI$",4 ) ){
                  xdiv_nice(TRUE);
                  /* -- Set up fixed division spacings. */
                }
		else if( lkreal( "I$",3, &cmgem.xdiv ) ){
			cmgem.lxdiv = TRUE;
			cmgem.lnxdiv = FALSE;

			/* -- Set up a fixed number of divisions. */
			}
		else if( lkirc( "NU$",4, 1, 100, &cmgem.nxdiv ) ){
			cmgem.lxdiv = FALSE;
			cmgem.lnxdiv = TRUE;

			/* -- Turn power labeling on/off. */
			}
		else if( lklog( "P$",3, &cmgem.lxpowr ) ){

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;
		}

L_8888:
	return;

}
