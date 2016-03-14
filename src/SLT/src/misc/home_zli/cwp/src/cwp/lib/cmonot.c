/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

#include "cwp.h"

void cmonot (int n, float x[], float y[], float yd[][4])
/*****************************************************************************
compute cubic interpolation coefficients via the Fritsch-Carlson method,
which preserves monotonicity
******************************************************************************
Input:
n		number of samples
x  		array[n] of monotonically increasing or decreasing abscissae
y		array[n] of ordinates

Output:
yd		array[n][4] of cubic interpolation coefficients (see notes)
******************************************************************************
Notes:
The computed cubic spline coefficients are as follows:
yd[i][0] = y(x[i])    (the value of y at x = x[i])
yd[i][1] = y'(x[i])   (the 1st derivative of y at x = x[i])
yd[i][2] = y''(x[i])  (the 2nd derivative of y at x = x[i])
yd[i][3] = y'''(x[i]) (the 3rd derivative of y at x = x[i])

To evaluate y(x) for x between x[i] and x[i+1] and h = x-x[i],
use the computed coefficients as follows:
y(x) = yd[i][0]+h*(yd[i][1]+h*(yd[i][2]/2.0+h*yd[i][3]/6.0))

The Fritsch-Carlson method yields continuous 1st derivatives, but 2nd
and 3rd derivatives are discontinuous.  The method will yield a 
monotonic interpolant for monotonic data.  1st derivatives are set 
to zero wherever first divided differences change sign.

For more information, see Fritsch, F. N., and Carlson, R. E., 1980, 
Monotone piecewise cubic interpolation:  SIAM J. Numer. Anal., v. 17,
n. 2, p. 238-246.

Also, see the book by Kahaner, D., Moler, C., and Nash, S., 1989, 
Numerical Methods and Software, Prentice Hall.  This function was 
derived from SUBROUTINE PCHEZ contained on the diskette that comes 
with the book.
******************************************************************************
Author:		Dave Hale, Colorado School of Mines, 09/30/89
Modified:	Dave Hale, Colorado School of Mines, 02/28/91
		changed to work for n=1.
*****************************************************************************/
{
	int i;
	float h1,h2,del1,del2,dmin,dmax,hsum,hsum3,w1,w2,drat1,drat2,divdf3;

	/* copy ordinates into output array */
	for (i=0; i<n; i++)
		yd[i][0] = y[i];

	/* if n=1, then use constant interpolation */
	if (n==1) {
		yd[0][1] = 0.0;
		yd[0][2] = 0.0;
		yd[0][3] = 0.0;
		return;

	/* else, if n=2, then use linear interpolation */
	} else if (n==2) {
		yd[0][1] = yd[1][1] = (y[1]-y[0])/(x[1]-x[0]);
		yd[0][2] = yd[1][2] = 0.0;
		yd[0][3] = yd[1][3] = 0.0;
		return;
	}

	/* set left end derivative via shape-preserving 3-point formula */
	h1 = x[1]-x[0];
	h2 = x[2]-x[1];
	hsum = h1+h2;
	del1 = (y[1]-y[0])/h1;
	del2 = (y[2]-y[1])/h2;
	w1 = (h1+hsum)/hsum;
	w2 = -h1/hsum;
	yd[0][1] = w1*del1+w2*del2;
	if (yd[0][1]*del1<=0.0)
		yd[0][1] = del1;
	else if (del1*del2<0.0) {
		dmax = 3.0*del1;
		if (ABS(yd[0][1])>ABS(dmax)) yd[0][1] = dmax;
	}

	/* loop over interior points */
	for (i=1; i<n-1; i++) {

		/* compute intervals and slopes */
		h1 = x[i]-x[i-1];
		h2 = x[i+1]-x[i];
		hsum = h1+h2;
		del1 = (y[i]-y[i-1])/h1;
		del2 = (y[i+1]-y[i])/h2;

		/* if not strictly monotonic, zero derivative */
		if (del1*del2<=0.0) {
			yd[i][1] = 0.0;
		
		/*
		 * else, if strictly monotonic, use Butland's formula:
		 *      3*(h1+h2)*del1*del2 
		 * -------------------------------
		 * ((2*h1+h2)*del1+(h1+2*h2)*del2)
		 * computed as follows to avoid roundoff error
		 */
		} else {
			hsum3 = hsum+hsum+hsum;
			w1 = (hsum+h1)/hsum3;
			w2 = (hsum+h2)/hsum3;
			dmin = MIN(ABS(del1),ABS(del2));
			dmax = MAX(ABS(del1),ABS(del2));
			drat1 = del1/dmax;
			drat2 = del2/dmax;
			yd[i][1] = dmin/(w1*drat1+w2*drat2);
		}
	}

	/* set right end derivative via shape-preserving 3-point formula */
	w1 = -h2/hsum;
	w2 = (h2+hsum)/hsum;
	yd[n-1][1] = w1*del1+w2*del2;
	if (yd[n-1][1]*del2<=0.0)
		yd[n-1][1] = 0.0;
	else if (del1*del2<0.0) {
		dmax = 3.0*del2;
		if (ABS(yd[n-1][1])>ABS(dmax)) yd[n-1][1] = dmax;
	}

	/* compute 2nd and 3rd derivatives of cubic polynomials */
	for (i=0; i<n-1; i++) {
		h2 = x[i+1]-x[i];
		del2 = (y[i+1]-y[i])/h2;
		divdf3 = yd[i][1]+yd[i+1][1]-2.0*del2;
		yd[i][2] = 2.0*(del2-yd[i][1]-divdf3)/h2;
		yd[i][3] = (divdf3/h2)*(6.0/h2);
	}
	yd[n-1][2] = yd[n-2][2]+(x[n-1]-x[n-2])*yd[n-2][3];
	yd[n-1][3] = yd[n-2][3];
}
