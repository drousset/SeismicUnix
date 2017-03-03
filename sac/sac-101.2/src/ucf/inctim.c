#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "config.h"
#include "../../inc/complex.h"
#include "../../inc/proto.h"

#define TOLERANCE 1e-4
#define TOLERANCE_EXCEEDED "Resulting error from shift exceeded tolerance (1e-6): "
#define BUG_REPORT "Please report bug to: "

void /*FUNCTION*/ inctim(nhrold, nmnold, nscold, nmsold, secinc, nhrnew, 
	 nmnnew, nscnew, nmsnew, nexday)
long int nhrold, nmnold, nscold, nmsold;
double secinc;
long int *nhrnew, *nmnnew, *nscnew, *nmsnew, *nexday;
{
	long int nhradd, nmnadd, nmsinc, nscadd, nscinc;
	double shift_error;
	/* Ind
	 *=====================================================================
	 * PURPOSE:  To calculate a new time from an old time and an increment.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    NHROLD:  Old hour.
	 *    NMNOLD:  Old minute.
	 *    NSCOLD:  Old second.
	 *    NMSOLD:  Old millisecond.
	 *    SECINC:  Time increment in seconds [floating].
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NHRNEW:  New hour.
	 *    NMNNEW:  New minute.
	 *    NSCNEW:  New second.
	 *    NMSNEW:  New millisecond.
	 *    NEXDAY:  Number of extra days in new time; =0 if same day.
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Set new values equal to old values. */
	*nexday = 0;
	*nhrnew = nhrold;
	*nmnnew = nmnold;

	/* Convert the floating point seconds to integer seconds and milliseconds. */

	
	nscinc = floor(secinc);
	nmsinc = lround( 1000.0 * (secinc - (float)( nscinc )) );
	*nmsnew = nmsold + nmsinc;
	*nscnew = nscold + nscinc;

	shift_error = fabs(secinc - ((double)(*nscnew - nscold) + 
				     (double)(*nmsnew - nmsold)/1000.0));
	if(shift_error >= TOLERANCE) {
	  setmsg("ERROR", 0);
	  apcmsg(TOLERANCE_EXCEEDED, strlen(TOLERANCE_EXCEEDED));
	  apfmsg(shift_error);
	  apcmsg("\n", strlen("\n"));
	  apcmsg(BUG_REPORT, strlen(BUG_REPORT));
	  apcmsg(PACKAGE_BUGREPORT, strlen(PACKAGE_BUGREPORT)+1);
	  outmsg();
	}
	/* - Make sure each output field is within it's correct range. */

	/* -- milliseconds. */
	if( *nmsnew > 999 ){
		nscadd = *nmsnew/1000;
		*nscnew = *nscnew + nscadd;
		*nmsnew = *nmsnew - 1000*nscadd;
	}
	else if( *nmsnew < 0 ){
		nscadd = *nmsnew/1000 - 1;
		*nscnew = *nscnew + nscadd;
		*nmsnew = *nmsnew - 1000*nscadd;
	}

	/* -- seconds. */
	if( *nscnew > 59 ){
		nmnadd = *nscnew/60;
		*nmnnew = *nmnnew + nmnadd;
		*nscnew = *nscnew - 60*nmnadd;
	}
	else if( *nscnew < 0 ){
		nmnadd = *nscnew/60 - 1;
		*nmnnew = *nmnnew + nmnadd;
		*nscnew = *nscnew - 60*nmnadd;
		if ( *nscnew == 60 ) {	/* if number of seconds is 60 ... (maf 970328) */
		    (*nmnnew)++ ;	/* increment number of minutes, ... */
		    *nscnew = 0 ;	/* and set number of seconds to 0 */
		}
	}

	/* -- minutes. */
	if( *nmnnew > 59 ){
		nhradd = *nmnnew/60;
		*nmnnew = *nmnnew - 60*nhradd;
		*nhrnew = *nhrnew + nhradd;
	}
	else if( *nmnnew < 0 ){
		nhradd = *nmnnew/60 - 1;
		*nmnnew = *nmnnew - 60*nhradd;
		*nhrnew = *nhrnew + nhradd;
                if ( *nmnnew == 60 ) {  /* if number of minutes is 60 ... (maf 970328) */
                    (*nhrnew)++ ;       /* increment number of hours, ... */
                    *nmnnew = 0 ;       /* and set number of minutes to 0 */
                }
	}

	/* -- hours. */
	if( *nhrnew > 23 ){
		*nexday = *nhrnew/24;
		*nhrnew = *nhrnew - 24**nexday;
	}
	else if( *nhrnew < 0 ){
		*nexday = *nhrnew/24 - 1;
		*nhrnew = *nhrnew - 24**nexday;
	}

L_8888:
	return;

	/**********************************************************************
	 * MODIFICATION HISTORY:
	 *    970328:  Fixed bug if number of seconds = 60, did same for
	 *             minutes while I was there.  maf
	 *    801018:  Converted to integer second/millisecond representation.
	 *    801029:  Corrected error involving above change.
	 *    810309:  Corrected error with negative time increments.
	 ********************************************************************** */

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    810000:  Original version.
	 *===================================================================== */

} /* end of function */

