#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
#include "../../inc/lhf.h"
void /*FUNCTION*/ getkhv(kname, kvalue, nerr, kname_s, kvalue_s)
char *kname;   long int kname_s;
char *kvalue;   long int kvalue_s;
long int *nerr;
{
	char ktest[9];
	long int index, ntest;
	
	char *kname_c;
	int callFromC = 0;

/* the following lines were added to help the function work the same whether
   it's called from C or FORTRAN.  From FORTRAN, don't pass in the last
   two parameters, the compiler does it for you.  From C, don't count the
   terminator.  maf 970917 */
/*         kname_s++ ; */
/* kvalue_s++; */

	if(kname_s < 0) {
	  callFromC = 1;
	}

	kname_c = fstrdup(kname, kname_s);
	kname_s = strlen(kname_c) + 1;

	/*=====================================================================
	 * PURPOSE: To get a character header value from the current SAC file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KNAME:   Name of header field to get. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KVALUE:  Value of header field from current SAC data file. [c]
	 *             KEVNM is 16 characters long.  All others are 8.
	 *    NERR:    Error flag. Set to 0 if no error occurred. [i]
	 *             = 1336 Header variable is undefined.
	 *             = 1337 Header variable does not exist.
	 *=====================================================================
	 * MODULE/LEVEL: DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    HDR:     KHDR, MKHDR, KUNDEF
	 *    LHF:     KKHDR
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INDEXB, MODCASE, NEQUAL, SETMSG, APCMSG, WRTMSG
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NCNAME:  Number of characters in KNAME. [i]
	 *    KTEST:   Local storage for KNAME [k]
	 *    INDEX:   Index in KHDR array of requested variable. [i]
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
	index = nequal( ktest, (char*)kmlhf.kkhdr,9, MKHDR );

	/* - If legal name, return current value.
	 *   Otherwise, set error condition. */

	if( index > 0 ){
	    fstrncpy( kvalue, kvalue_s-1, kmhdr.khdr[index - 1],
                      strlen(kmhdr.khdr[index - 1]) );
	    if( memcmp(kvalue,kmhdr.kundef,min(strlen(kvalue),
                       strlen(kmhdr.kundef))) == 0 ){
		*nerr = 1336;
	    }
	    else if( index == 2 ){
		subscpy( kvalue, 8, -1, kvalue_s - 1, kmhdr.khdr[2] );
	    }
	}
	else{
	    *nerr = 1337;
	    fstrncpy( kvalue, kvalue_s-1, kmhdr.kundef, strlen(kmhdr.kundef) );
	    index = 1;
	}

	/* - Create error message and write to terminal. */

	if( *nerr != 0 ){
	    setmsg( "ERROR", *nerr );
	    apcmsg( kname_c,kname_s );
            outmsg();
	}
	if(callFromC) {
	  kvalue[(index == 2) ? 16 : 8] = 0; /* Null Terminate the String at the approproiate Length */
	} else {
      if(index == 2 && kvalue_s > 16) {
        memset(kvalue + 16, ' ', kvalue_s-16);
      }
      if(index != 2 && kvalue_s > 8) {
        memset(kvalue + 8, ' ', kvalue_s - 8);
      }
    }

	free(kname_c);

L_8888:
	return;

} /* end of function */




/* Wrapper to make fuction more convenient for FORTRAN programmers. */

void getkhv_ (char      *kname, 
	      char      *kvalue, 
	      long int  *nerr, 
	      long int   kname_s,
	      long int   kvalue_s) {
  getkhv ( kname , kvalue , nerr , kname_s , kvalue_s ) ;
}

void getkhv__ (char      *kname, 
	      char      *kvalue, 
	      long int  *nerr, 
	      long int   kname_s,
	      long int   kvalue_s) {
  getkhv ( kname , kvalue , nerr , kname_s , kvalue_s ) ;
}
