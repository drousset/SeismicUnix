/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION:  shift a uniformly-sampled real-valued function y(x) via
a table of 8-coefficient sinc approximations; maximum error for
frequencies less than 0.6*nyquist is less than one percent.

PARAMETERS:
dx			i x sampling interval for both input and output y(x)
nxin		i number of x values at which y(x) is input
fxin		i x value of first sample input
yin			i array of input y(x) values:  yin[0] = y(fxin), etc.
yinl		i value used to extrapolate yin values to left of yin[0]
yinr		i value used to extrapolate yin values to right of yin[nxin-1]
nxout		i number of x values a which y(x) is output
fxout		i x value of first sample output
yout		o array of output y(x) values:  yout[0] = y(fxout), etc.

NOTES:
Because extrapolation of the input function y(x) is defined by the
left and right values yinl and yinr, the output samples defined by
dx, nxout, and fxout are not restricted to lie within the range of 
input sample locations defined by dx, nxin, and fxin.

AUTHOR:  Dave Hale, Colorado School of Mines, 06/02/89
*/

#include "cwp.h"

#define LTABLE 8
#define NTABLE 513

void shfs8r (float dx, int nxin, float fxin, float yin[], 
	float yinl, float yinr, int nxout, float fxout, float yout[])
{
	static float table[NTABLE][LTABLE];
	static int tabled=0;
	int jtable,ishift,ktable,ixout,itable,jshift,ilo,ihi;
	float frac,shift,dshift,tablei,youti;

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

	/* determine most appropriate set of tabulated coefficients */
	shift = (fxout-fxin)/dx;
	ishift = (int)shift;
	if (shift<0.0) ishift--;
	dshift = shift-(float)ishift;
	ktable = NINT(dshift*(NTABLE-1));

	/* convolve sinc approximation with input function */
	for (ixout=0; ixout<nxout; ixout++)
		yout[ixout] = 0.0;
	for (itable=0; itable<LTABLE; itable++) {
		tablei = table[ktable][itable];
		jshift = ishift+itable-LTABLE/2+1;
		for (ixout=0,youti=yinl*tablei; ixout<-jshift; ixout++)
			yout[ixout] += youti;
		ilo = MAX(0,-jshift);
		ihi = MIN(nxout-1,nxin-jshift-1);
		for (ixout=ilo; ixout<=ihi; ixout++)
			yout[ixout] += yin[ixout+jshift]*tablei;
		for (ixout=nxin-jshift,youti=yinr*tablei; ixout<nxout; ixout++)
			yout[ixout] += youti;
	}
}
