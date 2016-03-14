/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION:  prime factor fft:  real to complex transform

PARAMETERS:
isign       i sign of isign is the sign of exponent in fourier kernel
n           i length of transform; must be even (see notes below)
rz          i array of n real values (may be equivalenced to cz)
cz          o array of n/2+1 complex values (may be equivalenced to rz)
 
NOTES:
Because pfarc uses pfacc to do most of the work, n must be even 
and n/2 must be a valid length for pfacc.  The simplest way to
obtain a valid n is via n = npfar(nmin).  A more optimal n can be 
obtained with npfaro.

REFERENCES:  
Press et al, 1988, Numerical Recipes in C, p. 417.

Also, see notes and references for function pfacc.

AUTHOR:  I. D. Hale, Colorado School of Mines, 06/13/89
*/

#include "cwp.h"

void pfarc (int isign, int n, float rz[], complex cz[])
{
    int i,ir,ii,jr,ji,no2;
    float *z,tempr,tempi,sumr,sumi,difr,difi;
    double wr,wi,wpr,wpi,wtemp,theta;

    /* copy input to output while scaling */
    z = (float*)cz;
    for (i=0; i<n; i++)
        z[i] = 0.5*rz[i];

    /* do complex to complex transform */
    pfacc(isign,n/2,cz);

    /* fix dc and nyquist */
    z[n] = 2.0*(z[0]-z[1]);
    z[0] = 2.0*(z[0]+z[1]);
    z[n+1] = 0.0;
    z[1] = 0.0;

    /* initialize cosine-sine recurrence */
    theta = 2.0*PI/(double)n;
    if (isign<0) theta = -theta;
    wtemp = sin(0.5*theta);
    wpr = -2.0*wtemp*wtemp;
    wpi = sin(theta);
    wr = 1.0+wpr;
    wi = wpi;

    /* twiddle */
    no2 = n/2;
    for (ir=2,ii=3,jr=n-2,ji=n-1; ir<=no2; ir+=2,ii+=2,jr-=2,ji-=2) {
        sumr = z[ir]+z[jr];
        sumi = z[ii]+z[ji];
        difr = z[ir]-z[jr];
        difi = z[ii]-z[ji];
        tempr = wi*difr+wr*sumi;
        tempi = wi*sumi-wr*difr;
        z[ir] = sumr+tempr;
        z[ii] = difi+tempi;
        z[jr] = sumr-tempr;
        z[ji] = tempi-difi;
        wtemp = wr;
        wr += wr*wpr-wi*wpi;
        wi += wi*wpr+wtemp*wpi;
    }
}
