#include <stdio.h>
/*#include <stdlib.h> */
/*#include "../../inc/complex.h" */
/*#include "../../inc/proto.h" */
/*#include "../../inc/mach.h" */
#include "../../inc/dfm.h"

void /* FUNCTION */ xpickprefs ( nerr )
long int *nerr;
{

    /*=====================================================================
     * PURPOSE:  By default, the pickPreferences file is not used in 
     *           reading picks from CSS data nor in passing data from
     *           SeisMgr to the SAC data buffers.
     *           This command controls the use of the preferences file.
     *=====================================================================
     * OUTPUT ARGUMENTS:
     *            NERR passes back an error number if an error occured.
     *=====================================================================
     * MODULE/LEVEL:  
     *=====================================================================
     * GLOBAL INPUT:
     *=====================================================================
     * GLOBAL OUTPUT:
     *    DFM:     cmdfm.lpref
     *=====================================================================
     * SUBROUTINES CALLED:
     *=====================================================================
     * MODIFICATION HISTORY:
     *    000606:  Original version.  maf
     *===================================================================== */
    /* PROCEDURE: */
    *nerr = 0;


    if( lcmore( nerr ) ) {
	if( lckeyExact( "ON#$", 5 ) ) {
	    cmdfm.lpref = TRUE ;
	}

	else if( lckeyExact( "OFF#$", 6 ) ) {
	    cmdfm.lpref = FALSE ;
	}

	else{
	    *nerr = 1394 ;
	    setmsg( "ERROR" , *nerr ) ;
	    outmsg() ;
	}
    }
    else
	cmdfm.lpref = !cmdfm.lpref ;
}
