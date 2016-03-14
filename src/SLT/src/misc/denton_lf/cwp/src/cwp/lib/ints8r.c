/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION:  interpolation of a uniformly-sampled real function y(x) via a
table of 8-coefficient sinc approximations; maximum error for frequiencies
less than 0.6 nyquist is less than one percent.

PARAMETERS:
nxin		i number of x values at which y(x) is input
dxin		i x sampling interval for input y(x)
fxin		i x value of first sample input
yin			i array of input y(x) values:  yin[0] = y(fxin), etc.
yinl		i value used to extrapolate yin values to left of yin[0]
yinr		i value used to extrapolate yin values to right of yin[nxin-1]
nxout		i number of x values a which y(x) is output
xout		i array of x values at which y(x) is output
yout		o array of output y(x) values:  yout[0] = y(xout[0]), etc.

NOTES:
Because extrapolation of the input function y(x) is defined by the
left and right values yinl and yinr, the xout values are not restricted
to lie within the range of sample locations defined by nxin, dxin, and
fxin.

AUTHOR:  Dave Hale, Colorado School of Mines, 06/02/89
*/

#include "cwp.h"

#define LTABLE 8
#define NTABLE 513

void ints8r (int nxin, float dxin, float fxin, float yin[], 
	float yinl, float yinr, int nxout, float xout[], float yout[])
{
	static float table[NTABLE][LTABLE];
	static int tabled=0;
	int jtable;
	float frac;

	/* tabulate sinc interpolation coefficients if not already tabulated */
	if (!tabled) {
		for (jtable=1; jtable<NTABLE-1; jtable++) {
			frac = (float)jtable/(float)(NTABLE-1);
			mksinc(frac,LTABLE,&table[jtable][0]);
		}
		for (jtable=0; jtable<LTABLE; jtable++) {
			table[0][jtable] = 0.0;
			table[NTABLE-1][jtable] = 0.0;
		}
		table[0][LTABLE/2-1] = 1.0;
		table[NTABLE-1][LTABLE/2] = 1.0;
		tabled = 1;
	}

	/* interpolate using tabulated coefficients */
	intt8r(NTABLE,table,nxin,dxin,fxin,yin,yinl,yinr,nxout,xout,yout);
}
