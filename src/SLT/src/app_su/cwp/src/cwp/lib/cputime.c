/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

#include <time.h>

/* ANSI C book says clock() units are CLK_TCK, but IBM docs say millisecs */
#ifdef _IBMR2
#define CLOCK_UNIT	1000000
#else
/* #define CLOCK_UNIT	CLK_TCK  */
#define CLOCK_UNIT	1000000     /* SUN uses microseconds */
#endif

float
cputime()
/*****************************************************************************
return cpu time (UNIX user time) in seconds using ANSI C built-ins
******************************************************************************
Author:		Jack K. Cohen, Colorado School of Mines, 07/27/90
*****************************************************************************/
{
	return clock() / (float) CLOCK_UNIT;
}



#ifdef TEST

main()
{
	int i, n = 1000000;
	float cpu_used, a = 0.0, b = 1.0;

	cpu_used = cputime();
	for (i = 0; i < n; ++i)  a += b;
	cpu_used = cputime() - cpu_used;
	printf("a = %f  cpu time = %f\n", a, cpu_used);
}
#endif
