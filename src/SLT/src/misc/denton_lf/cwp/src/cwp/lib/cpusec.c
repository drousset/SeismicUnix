/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

#include <sys/time.h>
#include <sys/resource.h>

float
cpusec()
/*****************************************************************************
return cpu time (UNIX user time) in seconds
******************************************************************************
Author:		Dave Hale, Colorado School of Mines, 04/29/89
*****************************************************************************/
{
	struct rusage rusage;
	struct timeval time;
	getrusage(RUSAGE_SELF,&rusage);
	return ((float)((double)(rusage.ru_utime.tv_sec)+
		1.0e-6*(double)(rusage.ru_utime.tv_usec)));
}
