/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*****************************************************************************
Single precision basic linear algebra subroutines
(adapted from LINPACK FORTRAN):

isamax	return index of element with maximum absolute value
sasum	return sum of absolute values
saxpy	compute y[i] = a*x[i]+y[i]
scopy	copy x[i] to y[i] (i.e., set y[i] = x[i])
sdot	return sum of x[i]*y[i] (i.e., return the dot product of x and y)
snrm2	return square root of sum of squares of x[i]
sscal	compute x[i] = a*x[i]
sswap	swap x[i] and y[i]
******************************************************************************
Author:    	Dave Hale, Colorado School of Mines, 10/01/89
*****************************************************************************/

#include "cwp.h"

int isamax (int n, float *sx, int incx)
{
	int i,j,imax;
	float smax,xmag;

	/* if increment is not 1 */
	if (incx!=1) {
		imax = 0;
		smax = ABS(sx[0]);
		for (i=j=0; i<n; i++,j+=incx) {
			xmag = ABS(sx[j]);
			if (xmag>smax) {
				imax = i;
				smax = xmag;
			}
		}
	
	/* else, if increment is 1 */
	} else {
		imax = 0;
		smax = ABS(sx[0]);
		for (i=1; i<n; i++) {
			xmag = ABS(sx[i]);
			if (xmag>smax) {
				imax = i;
				smax = xmag;
			}
		}
	}
	return imax;
}

float sasum (int n, float *sx, int incx)
{
	int i,j,m;
	float sum=0.0;

	if (n<=0) return 0.0;

	/* if increment is not 1 */
	if (incx!=1) {
		for (i=j=0; i<n; i++,j+=incx)
			sum += ABS(sx[j]);

	/* else, if increment is 1 */
	} else {
		/* clean-up so remaining vector length is a multiple of 6 */
		m = n%6;
		for (i=0; i<m; i++)
			sum += ABS(sx[i]);
		
		/* finish in multiples of 6 */
		for (i=m; i<n; i+=6)
			sum += ABS(sx[i])+
				ABS(sx[i+1])+
				ABS(sx[i+2])+
				ABS(sx[i+3])+
				ABS(sx[i+4])+
				ABS(sx[i+5]);
	}
	return sum;
}

void saxpy (int n, float sa, float *sx, int incx, float *sy, int incy)
{
	int i,j,ix,iy,m;

	if (n<=0 || sa==0.0) return;

	/* if nonequal or nonpositive increments */
	if (incx!=incy || incx<0 || incy<0) {
		ix = (incx>=0)?0:(-n+1)*incx;
		iy = (incy>=0)?0:(-n+1)*incy;
		for (i=0; i<n; i++,ix+=incx,iy+=incy)
			sy[iy] += sa*sx[ix];
	
	/* else, if equal, positive, nonunit increments */
	} else if (incx!=1) {
		for (i=j=0; i<n; i++,j+=incx)
			sy[j] += sa*sx[j];
	
	/* else, if both increments equal 1 */
	} else {
		/* clean-up so remaining vector length is a multiple of 4 */
		m = n%4;
		for (i=0; i<m; i++)
			sy[i] += sa*sx[i];
		
		/* finish in multiples of 4 */
		for (i=m; i<n; i+=4) {
			sy[i] += sa*sx[i];
			sy[i+1] += sa*sx[i+1];
			sy[i+2] += sa*sx[i+2];
			sy[i+3] += sa*sx[i+3];
		}
	}
}

void scopy (int n, float *sx, int incx, float *sy, int incy)
{
	int i,j,ix,iy,m;

	if (n<=0) return;

	/* if nonequal or nonpositive increments */
	if (incx!=incy || incx<0 || incy<0) {
		ix = (incx>=0)?0:(-n+1)*incx;
		iy = (incy>=0)?0:(-n+1)*incy;
		for (i=0; i<n; i++,ix+=incx,iy+=incy)
			sy[iy] = sx[ix];
	
	/* else, if equal, positive, nonunit increments */
	} else if (incx!=1) {
		for (i=j=0; i<n; i++,j+=incx)
			sy[j] = sx[j];
	
	/* else, if both increments equal 1 */
	} else {
		/* clean-up so remaining vector length is a multiple of 7 */
		m = n%7;
		for (i=0; i<m; i++)
			sy[i] = sx[i];
		
		/* finish in multiples of 7 */
		for (i=m; i<n; i+=7) {
			sy[i] = sx[i];
			sy[i+1] = sx[i+1];
			sy[i+2] = sx[i+2];
			sy[i+3] = sx[i+3];
			sy[i+4] = sx[i+4];
			sy[i+5] = sx[i+5];
			sy[i+6] = sx[i+6];
		}
	}
}

float sdot (int n, float *sx, int incx, float *sy, int incy)
{
	int i,j,ix,iy,m;
	float sum=0.0;

	if (n<=0) return 0.0;

	/* if nonequal or nonpositive increments */
	if (incx!=incy || incx<0 || incy<0) {
		ix = (incx>=0)?0:(-n+1)*incx;
		iy = (incy>=0)?0:(-n+1)*incy;
		for (i=0; i<n; i++,ix+=incx,iy+=incy)
			sum += sx[ix]*sy[iy];
	
	/* else, if equal, positive, nonunit increments */
	} else if (incx!=1) {
		for (i=j=0; i<n; i++,j+=incx)
			sum += sx[j]*sy[j];
	
	/* else, if both increments equal 1 */
	} else {
		/* clean-up so remaining vector length is a multiple of 5 */
		m = n%5;
		for (i=0; i<m; i++)
			sum += sx[i]*sy[i];
		
		/* finish in multiples of 5 */
		for (i=m; i<n; i+=5) {
			sum += sx[i]*sy[i]+
				sx[i+1]*sy[i+1]+
				sx[i+2]*sy[i+2]+
				sx[i+3]*sy[i+3]+
				sx[i+4]*sy[i+4];
		}
	}
	return sum;
}

float snrm2 (int n, float *sx, int incx)
/* simple version - may cause overflow or underflow! */
{
	int i,j,m;
	float sum=0.0;

	if (n<=0) return 0.0;

	/* if increment is not 1 */
	if (incx!=1) {
		for (i=j=0; i<n; i++,j+=incx)
			sum += sx[j]*sx[j];

	/* else, if increment is 1 */
	} else {
		/* clean-up so remaining vector length is a multiple of 6 */
		m = n%6;
		for (i=0; i<m; i++)
			sum += sx[i]*sx[i];
		
		/* finish in multiples of 6 */
		for (i=m; i<n; i+=6)
			sum += sx[i]*sx[i]+
				sx[i+1]*sx[i+1]+
				sx[i+2]*sx[i+2]+
				sx[i+3]*sx[i+3]+
				sx[i+4]*sx[i+4]+
				sx[i+5]*sx[i+5];
	}
	return sqrt(sum);
}

void sscal (int n, float sa, float *sx, int incx)
{
	int i,j,m;

	if (n<=0) return;

	/* if increment not equal to 1 */
	if (incx!=1) {
		for (i=j=0; i<n; i++,j+=incx)
			sx[j] *= sa;
	
	/* else, if increment equal to 1 */
	} else {
		/* clean-up so remaining vector length is a multiple of 5 */
		m = n%5;
		for (i=0; i<m; i++)
			sx[i] *= sa;
		
		/* finish in multiples of 5 */
		for (i=m; i<n; i+=5) {
			sx[i] *= sa;
			sx[i+1] *= sa;
			sx[i+2] *= sa;
			sx[i+3] *= sa;
			sx[i+4] *= sa;
		}
	}
}

void sswap (int n, float *sx, int incx, float *sy, int incy)
{
	int i,j,ix,iy,m;
	float stemp1,stemp2,stemp3;

	if (n<=0) return;

	/* if nonequal or nonpositive increments */
	if (incx!=incy || incx<0 || incy<0) {
		ix = (incx>=0)?0:(-n+1)*incx;
		iy = (incy>=0)?0:(-n+1)*incy;
		for (i=0; i<n; i++,ix+=incx,iy+=incy) {
			stemp1 = sx[ix];
			sx[ix] = sy[iy];
			sy[iy] = stemp1;
		}
	
	/* else, if equal, positive, nonunit increments */
	} else if (incx!=1) {
		for (i=j=0; i<n; i++,j+=incx) {
			stemp1 = sx[j];
			sx[j] = sy[j];
			sy[j] = stemp1;
		}
	
	/* else, if both increments equal 1 */
	} else {
		/* clean-up so remaining vector length is a multiple of 3 */
		m = n%3;
		for (i=0; i<m; i++) {
			stemp1 = sx[i];
			sx[i] = sy[i];
			sy[i] = stemp1;
		}
		
		/* finish in multiples of 3 */
		for (i=m; i<n; i+=3) {
			stemp1 = sx[i];
			stemp2 = sx[i+1];
			stemp3 = sx[i+2];
			sx[i] = sy[i];
			sx[i+1] = sy[i+1];
			sx[i+2] = sy[i+2];
			sy[i] = stemp1;
			sy[i+1] = stemp2;
			sy[i+2] = stemp3;
		}
	}
}
