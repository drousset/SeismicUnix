/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

#include "cwp.h"

#define O2 0.5000000
#define O6 0.1666667

void intcub (int ideriv, int nin, float xin[], float ydin[][4], 
	int nout, float xout[], float yout[])
/*****************************************************************************
evaluate y(x), y'(x), y''(x), ... via piecewise cubic interpolation
******************************************************************************
Input:
ideriv		=0 if y(x) desired; =1 if y'(x) desired, ...
nin		length of xin and ydin arrays
xin		array[nin] of monotonically increasing or decreasing x values
ydin		array[nin][4] of y(x), y'(x), y''(x), and y'''(x)
nout		length of xout and yout arrays
xout		array[nout] of x values at which to evaluate y(x), y'(x), ...

Output:
yout		array[nout] of y(x), y'(x), ... values
******************************************************************************
Notes:
xin values must be monotonically increasing or decreasing.

Extrapolation of the function y(x) for xout values outside the range
spanned by the xin values is performed using the derivatives in 
ydin[0][0:3] or ydin[nin-1][0:3], depending on whether xout is closest
to xin[0] or xin[nin-1], respectively.
******************************************************************************
Author:		Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
{
	static int index;
	int iout;
	float delx;

	/* y(x) is desired, then */
	if (ideriv==0) {
		for (iout=0; iout<nout; iout++) {
			xindex(nin,xin,xout[iout],&index);
			delx = xout[iout]-xin[index];
			yout[iout] = (ydin[index][0]+delx*
				(ydin[index][1]+delx*
				(ydin[index][2]*O2+delx*
				(ydin[index][3]*O6))));
		}
		
	/* else, if y'(x) is desired, then */
	} else if (ideriv==1) {
		for (iout=0; iout<nout; iout++) {
			xindex(nin,xin,xout[iout],&index);
			delx = xout[iout]-xin[index];
			yout[iout] = (ydin[index][1]+delx*
				(ydin[index][2]+delx*
				(ydin[index][3]*O2)));
		}
		
	/* else, if y''(x) is desired, then */
	} else if (ideriv==2) {
		for (iout=0; iout<nout; iout++) {
			xindex(nin,xin,xout[iout],&index);
			delx = xout[iout]-xin[index];
			yout[iout] = (ydin[index][2]+delx*
				(ydin[index][3]));
		}
		
	/* else, if y'''(x) is desired, then */
	} else if (ideriv==3) {
		for (iout=0; iout<nout; iout++) {
			xindex(nin,xin,xout[iout],&index);
			delx = xout[iout]-xin[index];
			yout[iout] = (ydin[index][3]);
		}
		
	/* else, if any other derivative is desired, then */
	} else {
		for (iout=0; iout<nout; iout++)
			yout[iout] = 0.0;
	}
}
