#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/bbs.h"
void /*FUNCTION*/ writebbf(kname, nerr, kname_s)
char *kname;    long int kname_s ;
long int *nerr;
{
	int nc ;

	char *kname_c;

	kname_c = fstrdup(kname, kname_s);
	kname_s = strlen(kname_c) + 1;

	/*=====================================================================
	 * PURPOSE:  To write a blackboard variable file from a program.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kname:   Name of blackboard variable file. [c]
	 *             Set to blanks to use previous blackboard file name.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag. Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  bbs/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    bbs:     knmbbs
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  writevfile
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    871012:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  871012
	 *===================================================================== */
	/* - Write file to disk using the blackboard reserved name. */

	writevfile( kmbbs.knmbbs,MCPFN+1, kname_c, nerr );

	free(kname_c);

L_8888:
	return;

} /* end of function */





/* Added for FORTRAN friendliness */
void writebbf_ (char      *kname, 
		long int  *nerr, 
		long int   kname_s) {
  writebbf(kname, nerr, kname_s) ;
}
void writebbf__ (char      *kname, 
		 long int  *nerr, 
		 long int   kname_s) {
  writebbf(kname, nerr, kname_s) ;
}
