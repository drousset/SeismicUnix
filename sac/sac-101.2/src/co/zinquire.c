#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ zinquire(kname, lexist)
char *kname;
long *lexist;
{
        char *nkname, *tok;

	/*=====================================================================
	 * PURPOSE: To inquire about the existence of a disk file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kname:   Name of disk file. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    lexist:  Set to .TRUE. if file exists, .FALSE. otherwise. [l]
	 *=====================================================================
	 * MODULE/LEVEL:  co/5
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    871222:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  871222
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Check existance of file. */

        strcpy((nkname=malloc(strlen(kname)+1)),kname);
        tok = strtok(nkname," \0");
	*lexist = !access(tok, 0 );

        free(nkname);

L_8888:
	return;

} /* end of function */

