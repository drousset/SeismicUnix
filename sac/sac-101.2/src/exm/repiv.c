#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ repiv(ktext, ktext_s, iv)
char *ktext;   int ktext_s;
long int iv;
{
	char kline[MCMSG+1];
	long int nct;
        char *strtemp;

	/*=====================================================================
	 * PURPOSE: To report the value of a integer variable.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    TEXT:    Text to accompany value of variable. [c]
	 *    IV:      Value of integer variable to report. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MUNOUT
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INDEXC
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890104:  Changed from direct terminal output to message subsystem.
	 *    820315:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890104
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine length of text. */
	nct = indexc( ktext,ktext_s, '$' );

	/* - Write text and value of variable to message system. */

        strtemp = malloc(nct+1);
        strncpy(strtemp,ktext,nct);
        strtemp[nct] = '\0';

        sprintf(kline,"   %s%s%5ld", strtemp, " is ", iv );
	aplmsg( kline,MCMSG+1 );

        free(strtemp);
L_8888:
	return;

} /* end of function */

