#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
#include "../../inc/lhf.h"
void /*FUNCTION*/ getnhv(kname, nvalue, nerr, kname_s)
char *kname;   long int kname_s;
long int *nvalue, *nerr;
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
	 * PURPOSE: To get an integer header value from the current SAC file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KNAME:   Name of header field to get. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NVALUE:  Value of header field from current SAC data file. [i]
	 *    NERR:    Error flag. Set to 0 if no error occurred. [i]
	 *             = 1336 Header variable is undefined.
	 *             = 1337 Header variable does not exist.
	 *=====================================================================
	 * MODULE/LEVEL: DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    HDR:     NHDR, MNHDR, NUNDEF
	 *    LHF:     KNHDR
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INDEXB, MODCASE, NEQUAL, SETMSG, APCMSG, WRTMSG
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NCNAME:  Number of characters in KNAME. [i]
	 *    KTEST:   Local storage for KNAME [k]
	 *    INDEX:   Index in NHDR array of requested variable. [i]
	 *=====================================================================
	 * ASSUMPTIONS:
	 * - A data file has been read by RSAC1 or RSAC2  OR
	 * - A header has been retrieved from working memory by GETFIL
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
	strcpy( ktest, "        " );
	modcase( TRUE, kname_c, ntest, ktest );
	index = nequal( ktest, (char*)kmlhf.knhdr,9, MNHDR );

	/* - If legal name, return current value.
	 *   Otherwise, set error condition. */

	if( index > 0 ){
	    *nvalue = Nhdr[index];
	    if( *nvalue == cmhdr.nundef )
		*nerr = 1336;
	}
	else{
	    *nerr = 1337;
	    *nvalue = cmhdr.nundef;
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




/* Wrapper to make code more convenient for FORTRAN programmers.  */

void getnhv_ (char      *kname, 
	      long int  *nvalue, 
	      long int  *nerr, 
	      long int   kname_s) {
  getnhv ( kname , nvalue , nerr , kname_s ) ;
}
void getnhv__ (char      *kname, 
	       long int  *nvalue, 
	       long int  *nerr, 
	       long int   kname_s) {
  getnhv ( kname , nvalue , nerr , kname_s ) ;
}
