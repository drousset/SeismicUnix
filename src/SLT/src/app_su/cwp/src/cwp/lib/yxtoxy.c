/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION:  compute a regularly-sampled, monotonically increasing function x(y)
from a regularly-sampled, monotonically increasing function y(x) by inverse
linear interpolation

PARAMETERS:
nx			i number of samples of y(x)
dx			i x sampling interval; dx>0.0 is required
fx			i first x
y			i array of y(x) values; y[0] < y[1] < ... < y[nx-1] required
ny			i number of samples of x(y)
dy			i y sampling interval; dy>0.0 is required
fy			i first y
xylo		i x value assigned to x(y) when y is less than smallest y(x)
xyhi		i x value assigned to x(y) when y is greater than largest y(x)
x			o array of x(y) values

NOTES:
User must ensure that:
(1) dx>0.0 && dy>0.0
(2) y[0] < y[1] < ... < y[nx-1]

AUTHOR:  Dave Hale, Colorado School of Mines, 06/02/89
*/

void yxtoxy (int nx, float dx, float fx, float y[], 
	int ny, float dy, float fy, float xylo, float xyhi, float x[])
{
	int nxi,nyo,jxi1,jxi2,jyo;
	float dxi,fxi,dyo,fyo,fyi,yo,xi1,yi1,yi2; 

	nxi = nx; dxi = dx; fxi = fx;
	nyo = ny; dyo = dy; fyo = fy;
	fyi = y[0];

	/* loop over output y less than smallest input y */
	for (jyo=0,yo=fyo; jyo<nyo; jyo++,yo+=dyo) {
		if (yo>=fyi) break;
		x[jyo] = xylo;
	}

	/* loop over output y between smallest and largest input y */
	if (jyo==nyo-1 && yo==fyi) {
		x[jyo++] = fxi;
		yo += dyo;
	}
	jxi1 = 0;
	jxi2 = 1;
	xi1 = fxi;
	while (jxi2<nxi && jyo<nyo) {
		yi1 = y[jxi1];
		yi2 = y[jxi2];
		if (yi1<=yo && yo<=yi2) {
			x[jyo++] = xi1+dxi*(yo-yi1)/(yi2-yi1);
			yo += dyo;
		} else {
			jxi1++;
			jxi2++;
			xi1 += dxi;
		}
	}

	/* loop over output y greater than largest input y */
	while (jyo<nyo)
		x[jyo++] = xyhi;
}
