/** 
 * @file   xydiv.c
 * 
 * @brief  Control the Y Divisions 
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
ydiv_nice(int flag) {
  cmgem.lydiv  = (flag == TRUE) ? FALSE : TRUE; /* Y Division "Increment"; spacing between divisons: cmgem.ydiv  */
  cmgem.lnydiv = (flag == TRUE) ? FALSE : TRUE; /* Y Division "Number"; number of divisions: cmgem.lnydiv        */
}

void 
ydiv_increment(float z) {
  cmgem.lydiv  = TRUE;
  cmgem.lnydiv = FALSE;
  cmgem.ydiv   = z;    /* Spacing between Divisions */
}

void
ydiv_number(int n) {
  cmgem.lydiv = FALSE;
  cmgem.lnydiv = TRUE;
  cmgem.nydiv = n;  /* Number of Divisions */
}

void
ydiv_power(int flag) {
  cmgem.lypowr = flag; /* Division displayed as a power */
}


/** 
 * Parse the command "ydiv" and set the Y Divisions
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
xydiv(long int *nerr) {

	*nerr = 0;

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- Set up nice numbering. */
		if( lckey( "NI$",4 ) ){
                  ydiv_nice(TRUE);
                  /* -- Set up fixed division spacings. */
                }
		else if( lkreal( "I$",3, &cmgem.ydiv ) ){
			cmgem.lydiv = TRUE;
			cmgem.lnydiv = FALSE;

			/* -- Set up a fixed number of divisions. */
			}
		else if( lkirc( "NU$",4, 1, 100, &cmgem.nydiv ) ){
			cmgem.lydiv = FALSE;
			cmgem.lnydiv = TRUE;

			/* -- Turn power labeling on/off. */
			}
		else if( lklog( "P$",3, &cmgem.lypowr ) ){

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

