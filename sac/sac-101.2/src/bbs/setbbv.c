#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/bbs.h"
void /*FUNCTION*/ setbbv(kname, kvalue, nerr, kname_s, kvalue_s)
char *kname;   long int kname_s;
char *kvalue;   long int kvalue_s;
long int *nerr;
{
	char ktemp[33] = "                                " ;
	long int nc;

	char *kname_c;
	char *kvalue_c;
	int i;

	kname_c  = fstrdup(kname, kname_s);
	kvalue_c = fstrdup(kvalue, kvalue_s);

	kname_s  = strlen(kname_c) + 1;
	kvalue_s = strlen(kvalue_c) + 1;

	/*=====================================================================
	 * PURPOSE:  To set (define) a blackboard variable.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kname:   The name of the blackboard variable. [c]
	 *    kvalue:  The value of the blackboard variable. [c]
	 *    nerr:    Error return flag.  Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL: bbs/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    
	 *    bbs:     knmbbs
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  indexb, modcase, putvvstring
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870301:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870301
	 *===================================================================== */
	/* PROCEDURE: */
 	nc = min( indexb( kname_c,kname_s ), 32 ); 
 	modcase( TRUE, kname_c, nc, ktemp ); 
	nc = indexb( kvalue_c,kvalue_s );
	putvvstring( kmbbs.knmbbs,MCPFN+1, ktemp,33, nc, kvalue_c,kvalue_s, 
	 nerr );

	free(kname_c);
	free(kvalue_c);

L_8888:
	return;

} /* end of function */





/* Added for FORTRAN friendliness */
void setbbv_ (char      *kname, 
	      char      *kvalue, 
	      long int  *nerr, 
	      long int   kname_s,
	      long int   kvalue_s) {
  setbbv(kname, kvalue, nerr, kname_s, kvalue_s) ;
}
void setbbv__ (char      *kname, 
	      char      *kvalue, 
	      long int  *nerr, 
	      long int   kname_s,
	      long int   kvalue_s) {
  setbbv(kname, kvalue, nerr, kname_s, kvalue_s) ;
}
