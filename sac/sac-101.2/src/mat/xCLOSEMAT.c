
#include <config.h>

#ifdef HAVE_MATLAB

#include <stdio.h>

/* Matlab Specific Header File */
#include <engine.h>

#include "matFuncExternal.h"


/** 
 * Execute the command "closemat" which closes the Matlab Engine
 *
 * @param nerr 
 *    Error Return Flag
 *    - 0 on Success ( Always Successful )
 *    - Non-Zero on Error
 *
 * @date 970902:  Original version.
 *
 */
void 
xCLOSEMAT(long int *nerr)
{
#	include "matFuncInternal.h" 

	*nerr = 0;

	if ( linkedAndRunning ) {
	   matDisconnect () ;
	}

        return;
        
} 

#endif /* HAVE_MATLAB */

#ifndef HAVE_MATLAB

void xCLOSEMAT(long int *nerr) { matlab_unavailable(); }

#endif 
