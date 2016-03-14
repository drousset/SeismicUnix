/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION: compute z = x cross-correlated with y; i.e.,

           ifx+lx-1
    z[i] =   sum    x[j]*y[i+j]  ;  i = ifz,...,ifz+lz-1
            j=ifx

PARAMETERS:
lx			i length of x array
ifx			i sample index of first x
x			i array to be cross-correlated with y
ly			i length of y array
ify			i sample index of first y
y			i array with which x is to be cross-correlated
lz			i length of z array
ifz			i sample index of first z
z			o array containing x cross-correlated with y

AUTHOR:  Dave Hale, Colorado School of Mines, 06/02/89
MODIFIED:  Dave Hale, Colorado School of Mines, 03/01/90
	Use dot products for faster performance on non-vector machines.
*/

void xcor (int lx, int ifx, float x[], int ly, int ify, float y[],
	int lz, int ifz, float z[])
{
	int ilx=ifx+lx-1,ily=ify+ly-1,ilz=ifz+lz-1,i,j,jlow,jhigh;
	float *xp=x-ifx,*yp=y-ify,*zp=z-ifz,sum;

	for (i=ifz; i<=ilz; ++i) {
		jlow = ify-i;  if (jlow<ifx) jlow = ifx;
		jhigh = ily-i;  if (jhigh>ilx) jhigh = ilx;
		for (j=jlow,sum=0.0; j<=jhigh; ++j)
			sum += xp[j]*yp[i+j];
		zp[i] = sum;
	}
}
