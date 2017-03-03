#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/bbs.h"
void /*FUNCTION*/ unsetbbv(kname, nerr, kname_s)
char *kname;   int kname_s;
long int *nerr;
{
	char ktemp[33];
	long int nc;

	char *kname_c;
	
	kname_c = fstrdup(kname, kname_s);
	kname_s = strlen(kname_c) + 1;
	
	/*=====================================================================
	 * PURPOSE:  To unset (delete) a blackboard variable.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kname:   The name of the blackboard variable. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag.  Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL: bbs/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    
	 *    bbs:     knmbbs
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  indexb, modcase, deletev
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    880412:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  880412
	 *===================================================================== */
	/* PROCEDURE: */
	nc = min( indexb( kname_c,kname_s ), 32 );
	strcpy( ktemp, "                                " );
	modcase( TRUE, kname_c, nc, ktemp );
	deletev( kmbbs.knmbbs,MCPFN+1, ktemp,33, nerr );

	free(kname_c);
L_8888:
	return;

} /* end of function */





/* Added for FORTRAN friendleness */
void unsetbbv_ (char      *kname, 
		long int  *nerr, 
		long int   kname_s) {
  unsetbbv(kname, nerr, kname_s) ;
}
void unsetbbv__ (char      *kname, 
		long int  *nerr, 
		long int   kname_s) {
  unsetbbv(kname, nerr, kname_s) ;
}
