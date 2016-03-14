/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

#include <sys/time.h>

float
wallsec()
/*****************************************************************************
return elapsed time (wall clock time) in seconds
******************************************************************************
Author:		Dave Hale, Colorado School of Mines, 04/29/89
*****************************************************************************/
{
	struct timeval tp;
	struct timezone tzp;
	static int firsttime=1;
	static long firstsec,firstusec;
	long sec,usec;

	gettimeofday(&tp,&tzp);
	if (firsttime) {
		firsttime=0;
		firstsec = tp.tv_sec;
		firstusec = tp.tv_usec;
		return(0.0);
	} else {
		sec = tp.tv_sec-firstsec;
		usec = tp.tv_usec-firstusec;
		return((float)sec+1.0e-6*(float)usec);
	}
}
