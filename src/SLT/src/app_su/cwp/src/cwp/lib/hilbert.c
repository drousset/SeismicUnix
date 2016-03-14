/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION: compute Hilbert transform y of x

PARAMETERS:
n			i length of x and y
x			i function to be Hilbert transformed
y			o Hilbert transform of x

NOTES:
The Hilbert transform is computed by convolving x with a
windowed (approximate) version of the ideal Hilbert transformer.

AUTHOR:  Dave Hale, Colorado School of Mines, 06/02/89
*/

#include "cwp.h"

#define LHHALF 30			/* half-length of Hilbert transform filter */
#define LH 2*LHHALF+1		/* filter length must be odd */

void hilbert (int n, float x[], float y[])
{
	static int madeh=0;
	static float h[LH];
	int i;
	float taper;

	/* make Hilbert transform filter if not already made; use Hamming window */
	if (!madeh) {
		h[LHHALF] = 0.0;
		for (i=1; i<=LHHALF; i++) {
			taper = 0.54+0.46*cos(PI*(float)i/(float)(LHHALF));
			h[LHHALF+i] = taper*(-(float)(i%2)*2.0/(PI*(float)(i)));
			h[LHHALF-i] = -h[LHHALF+i];
		}
		madeh = 1;
	}

	/* convolve Hilbert transform with input array */
	conv(LH,-LHHALF,h,n,0,x,n,0,y);
}
