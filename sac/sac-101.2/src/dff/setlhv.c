#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
#include "../../inc/lhf.h"
void /*FUNCTION*/ setlhv(kname, lvalue, nerr, kname_s)
char *kname;   long int kname_s;
long int *lvalue;
long int *nerr;
{
	char ktest[9];
	long int index, ntest;

	char *kname_c;

/* the following line was added to help the function work the same whether
   it's called from C or FORTRAN.  From FORTRAN, don't pass in the last
   parameter, the compiler does it for you.  From C, don't count the
   terminator.  maf 970917 */
/*         kname_s++ ; */

	kname_c = fstrdup(kname, kname_s);
	kname_s = strlen(kname_c) + 1;

	/*=====================================================================
	 * PURPOSE: To set a logical header value in the current SAC file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KNAME:   Name of header field to set. [c]
	 *    LVALUE:  New value of header field. [l]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred. [i]
	 *             = 1337 Header field does not exist.
	 *=====================================================================
	 * MODULE/LEVEL: DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    HDR:     LHDR, MLHDR
	 *    LHF:     KLHDR
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INDEXB, MODCASE, NEQUAL, SETMSG, APCMSG, WRTMSG
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NCNAME:  Number of characters in KNAME. [i]
	 *    KTEST:   Local storage for KNAME [k]
	 *    INDEX:   Index in LHDR array of requested variable. [i]
	 *=====================================================================
	 * ASSUMPTIONS:
	 * - The data file will be written by WSAC  OR
	 * - The header will be stored in working memory by PUTFIL.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870902:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870902
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Convert input name to uppercase and check versus list of legal names. */

	ntest = min( indexb( kname_c,kname_s ), MCPW );
  	strncpy( ktest, "         ", 9); 
 	modcase( TRUE, kname_c, ntest, ktest );
	ktest[8] = 0;
/*  	fprintf(stderr, "kname_c[%d]: <%s> <%s>\n", kname_s, kname_c, ktest); */
	index = nequal( ktest, (char*)kmlhf.klhdr,9, MLHDR );

	/* - Store value in appropriate header field. */

	if( index > 0 ){
	    Lhdr[index] = *lvalue;
	}
	else{
	    *nerr = 1337;
	    Lhdr[index] = FALSE;
	}

	/* - Create error message and write to terminal. */

	if( *nerr != 0 ){
	    setmsg( "ERROR", *nerr );
	    apcmsg( kname_c,kname_s );
	    outmsg();
	}

 	free(kname_c); 

L_8888:
	return;

} /* end of function */




/* Wrapper to make the function more convenient for FORTRAN programmers. */

void setlhv_ (char      *kname, 
	      long      *lvalue, 
	      long int  *nerr, 
	      long int   kname_s) {
  setlhv ( kname , lvalue , nerr , kname_s ) ;
}
void setlhv__ (char      *kname, 
	       long      *lvalue, 
	       long int  *nerr, 
	       long int   kname_s) {
  setlhv ( kname , lvalue , nerr , kname_s ) ;
}
