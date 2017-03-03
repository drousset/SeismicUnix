#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/mem.h"
#include "../../inc/vars.h"
#include "../../inc/bbs.h"

char *fstrdup(char *s, long int n);

void /*FUNCTION*/ readbbf(kname, nerr, kname_s)
char *kname;    long int kname_s ;
long int *nerr;
{

	char *kname_c;
	kname_c = fstrdup(kname, kname_s);
	kname_s = strlen(kname_c) + 1;

	/*=====================================================================
	 * PURPOSE:  To read a blackboard variable file into a program.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kname:   Name of blackboard variable file. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag. Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  bbs/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    mem:     sacmem, mmem, mepsl
	 *    bbs:     kbbsinit
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    bbs:     knmbbs
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  initializevars, inibbs, getvlist
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890309:  Changed from call to readvfile to getvlist.
	 *    871012:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  871012
	 *===================================================================== */
	/* - Initialize blackboard store and sacmem array if needed. */
	if( strcmp(kmbbs.kbbsinit,"INITDONE") != 0 ){
		initializevars();
		inibbs();
		inivars () ;
		strcpy ( kmvars.varsidcode , "VARS" ) ;
		}

	/* - Read file from disk using the blackboard reserved name. */

	strcpy( kmbbs.knmbbs, kname_c);
	getvlist( kmbbs.knmbbs,MCPFN+1, " ",2, "SINGLE", nerr );

	free(kname_c);
L_8888:
	return;

} /* end of function */





/* Added for FORTRAN friendliness */
void readbbf_ (char      *kname, 
	       long int  *nerr, 
	       long int   kname_s) {
  readbbf(kname, nerr, kname_s) ;
}
void readbbf__ (char      *kname, 
		long int  *nerr, 
		long int   kname_s) {
  readbbf(kname, nerr, kname_s) ;
}
