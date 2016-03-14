/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

#include "cwp.h"

void ress8r (int nxin, float dxin, float fxin, float yin[], 
	float yinl, float yinr, 
	int nxout, float dxout, float fxout, float yout[])
/*****************************************************************************
Resample a uniformly-sampled real function y(x) via 8-coefficient sinc
approximations; maximum error for frequiencies less than 0.6 nyquist is 
less than one percent.
******************************************************************************
Input:
nxin		number of x values at which y(x) is input
dxin		x sampling interval for input y(x)
fxin		x value of first sample input
yin		array of input y(x) values:  yin[0] = y(fxin), etc.
yinl		value used to extrapolate yin values to left of yin[0]
yinr		value used to extrapolate yin values to right of yin[nxin-1]
nxout		number of x values at which y(x) is output
dxout		x sampling interval for output y(x)
fxout		x value of first sample output

Output:
yout		array of output y(x) values:  yout[0] = y(xout[0]), etc.
******************************************************************************
Notes:
Because extrapolation of the input function y(x) is defined by the
left and right values yinl and yinr, the output x values defined
by nxout, dxout, and fxout are not restricted to lie within the range 
of input x values defined by nxin, dxin, and fxin.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/06/90
*****************************************************************************/
{
	int ixin,ixout;
	float odxin,tiny,xout,xini,error,*xoutt;
	
	/* initially, assume output samples fall on input samples */
	odxin = 1.0/dxin;
	tiny = 0.01;
	for (ixout=0,xout=fxout; ixout<nxout; ++ixout,xout+=dxout) {
		xini = (xout-fxin)*odxin;
		ixin = NINT(xini);
		error = xini-ixin;
		if (ABS(error)>tiny) break;
		if (ixin<0)
			yout[ixout] = yinl;
		else if (ixin>=nxin)
			yout[ixout] = yinr;
		else
			yout[ixout] = yin[ixin];
	}
	
	/* if all output samples fell on input samples, then done */
	if (ixout==nxout) return;
	
	/* otherwise, must interpolate */
	xoutt = alloc1float(nxout);
	for (ixout=0; ixout<nxout; ++ixout)
		xoutt[ixout] = fxout+ixout*dxout;
	ints8r(nxin,dxin,fxin,yin,yinl,yinr,nxout,xoutt,yout);
	free1float(xoutt);
}
