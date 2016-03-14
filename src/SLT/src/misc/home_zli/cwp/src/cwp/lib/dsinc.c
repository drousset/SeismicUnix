/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION:  return sinc(x) = sin(PI*x)/(PI*x) (double version)

PARAMETERS:
x			i value at which to evaluate sinc(x)

AUTHOR:  Dave Hale, Colorado School of Mines, 06/02/89
*/

#include "cwp.h"

double dsinc (double x)
{
	double pix;

	if (x==0.0) {
		return 1.0;
	} else {
		pix = PI*x;
		return sin(pix)/pix;
	}
}
