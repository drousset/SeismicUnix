/*
FUNCTION:  interpolation of a uniformly-sampled real function y(x) via
linear interpolation.

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

Author:		Zhiming Li		       	6-1-93
*/

#include "subc.h"

void intsln (int nxin, float dxin, float fxin, float yin[], 
	float yinl, float yinr, int nxout, float xout[], float yout[])
{
	int ix,ox;
	float tmp,res;
	float odxin;
	
	odxin = 1./dxin;

	for (ox=0;ox<nxout;ox++) {
		tmp = (xout[ox] - fxin)*odxin;
		ix = tmp;
		if(ix<0) {
			yout[ox] = yinl;
		} else if (ix>=nxin) {
			yout[ox] = yinr;
		} else if(ix==nxin-1) {
			yout[ox] = yin[ix];
		} else {
			res = tmp - ix;
			yout[ox] = (1.0-res)*yin[ix] + res*yin[ix+1];
		}
		
	}
}
