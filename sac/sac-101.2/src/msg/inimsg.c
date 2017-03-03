
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "msg.h"

/** 
 * Initialize MSG Common Block
 * 
 * @date   August 4 2008 - Changed Errors to appear on 
 *                         Standard Error instead of Standard Output
 *                         B. Savage < savage _AT_ uri _DOT_ edu >
 * @date   881230:  Added activation of standard output for messages.
 * @date   860203:  Added options for message storage in common.
 * @date   841005:  Original version.
 *
 * @date 881230 Documented/Reviewed
 *
 */
void 
inimsg() {
	long send_[MTPMSG];
	long int nerr;
        long i;
	long *const Send_ = &send_[0] - 1;


	strcpy( kmmsg.ktpmsg[MERRORS - 1], "ERRORS  " );
	strcpy( kmmsg.ktpmsg[MWARNINGS - 1], "WARNINGS" );
	strcpy( kmmsg.ktpmsg[MOUTPUT - 1], "OUTPUT  " );
	strcpy( kmmsg.ktpmsg[MCOMMANDS - 1], "COMMANDS" );
	strcpy( kmmsg.ktpmsg[MMACROS - 1], "MACROS  " );
	strcpy( kmmsg.ktpmsg[MPROCESSED - 1], "PROCESSE" );

	cmmsg.nummsg = 0;
	cmmsg.itpmsg = 0;
	cmmsg.autoout = FALSE;

	cmmsg.nfmsg = 0;
	sacmsg( &nerr );
	if( nerr != 0 )
		outmsg();

	cmmsg.nunits = 0;

        for (i=0; i<MUNITS; i++){
          cmmsg.iunits[i] = NULL;
	}

        /* Send warnings and output to standard output */
	Send_[MERRORS]    = FALSE;
	Send_[MWARNINGS]  = TRUE;
	Send_[MOUTPUT]    = TRUE;
	Send_[MCOMMANDS]  = FALSE;
	Send_[MMACROS]    = FALSE;
	Send_[MPROCESSED] = FALSE;
	sendmesg( MUNOUT, TRUE, send_ );

        /* Send the error output to Standard Error */
	Send_[MERRORS]    = TRUE;
	Send_[MWARNINGS]  = FALSE;
	Send_[MOUTPUT]    = FALSE;
	Send_[MCOMMANDS]  = FALSE;
	Send_[MMACROS]    = FALSE;
	Send_[MPROCESSED] = FALSE;
        sendmesg( stderr, TRUE, send_ );

L_8888:
	return;

} /* end of function */

