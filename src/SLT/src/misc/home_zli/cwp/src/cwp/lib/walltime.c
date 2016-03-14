/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

#include "cwp.h"
#include <time.h>

float
walltime()
/*****************************************************************************
return elapsed time (wall clock time) in seconds using ANSI C built-ins

NOTES:
	return value will be an integral number of seconds since t1 and t2,
	as returned by the time() intrinsic, are the number of seconds
	since the epoch
******************************************************************************
Author:		Jack K. Cohen, Colorado School of Mines, 07/27/90
*****************************************************************************/
{
	static bool firsttime = true;	/* First entry?                 */
	static time_t lasttime;		/* Save return for next entry   */
	time_t t1, t2;			/* Resp. last and current times */

	if (firsttime) {
		firsttime = false;
		lasttime = time(&t1);
		return 0.0;
	} else {
		t1 = lasttime;
		lasttime = time(&t2);
		return (float) difftime(t2, t1);
	}
}


#ifdef TEST
#include "cwp.h"

main()
{
	int i, n = 1000000;
	float wall_used, a = 0.0, b = 1.0;

	wall_used = walltime();
	for (i = 0; i < n; ++i)  a += b;
	wall_used = walltime() - wall_used;
	printf("a = %f  wall time = %f\n", a, wall_used);
}
#endif
