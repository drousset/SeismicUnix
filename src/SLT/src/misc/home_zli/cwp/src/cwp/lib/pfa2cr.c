/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION:  prime factor fft:  2-D complex to real transforms

PARAMETERS:
isign       i sign of isign is the sign of exponent in fourier kernel
idim        i dimension to transform, which must be either 1 or 2 (see notes)
n1          i 1st (fast) dimension of array to be transformed (see notes)
n2          i 2nd (slow) dimension of array to be transformed (see notes)
cz          i array of complex values (may be equivalenced to rz)
rz          o array of real values (may be equivalenced to cz)
 
NOTES:
If idim equals 1, then n2 transforms of n1/2+1 complex elements to n1 real 
elements are performed; else, if idim equals 2, then n1 transforms of n2/2+1 
complex elements to n2 real elements are performed.

Although rz appears in the argument list as a one-dimensional array,
rz may be viewed as an n1 by n2 two-dimensional array:  rz[n2][n1].  
Likewise, depending on idim, cz may be viewed as either an n1/2+1 by 
n2 or an n1 by n2/2+1 two-dimensional array of complex elements.

Let n denote the transform length, either n1 or n2, depending on idim.
Because pfa2rc uses pfa2cc to do most of the work, n must be even 
and n/2 must be a valid length for pfa2cc.  The simplest way to
obtain a valid n is via n = npfar(nmin).  A more optimal n can be 
obtained with npfaro.
 
REFERENCES:  
Press et al, 1988, Numerical Recipes in C, p. 417.

Also, see notes and references for function pfa2cc.

AUTHOR:  I. D. Hale, Colorado School of Mines, 06/13/89
MODIFIED:  I. D. Hale, Colorado School of Mines, 11/26/89
	Fixed bug that gave wrong answers for idim==2 and cz not 
	equivalenced to rz.  The bug was in copying the input
	to the output in the first section below - the j index
	was not being incremented along with i1.  Fixed by adding
	j++ after the i1++.
*/

#include "cwp.h"

void pfa2cr (int isign, int idim, int n1, int n2, complex cz[], float rz[])
{
    int i1,i2,j,k,it,jt,kt,n,nt,itmul,itinc;
    float *z,*temp,tempr,tempi,sumr,sumi,difr,difi;
    double wr,wi,wpr,wpi,wtemp,theta;

    /* if transforming dimension 1 */
    if (idim==1) {

        /* copy input to output and fix dc and nyquist */
        z = (float*)cz;
        for (i2=0,jt=0,kt=0; i2<n2; i2++,jt+=n1,kt+=(n1+2)) {
            rz[jt+1] = z[kt]-z[kt+n1];
            rz[jt] = z[kt]+z[kt+n1];
            for (i1=2,j=jt+2,k=kt+2; i1<n1; i1++,j++,k++)
                rz[j] = z[k];
        }
        z = rz;

        /* set transform length, number of transforms and strides */
        n = n1;
        nt = n2;
        itmul = 1;
        itinc = n1;

    /* else, if transforming dimension 2 */
    } else {

        /* copy input to output and fix dc and nyquist */
        z = (float*)cz;
        for (i2=1; i2<n2/2; i2++) {
            for (i1=0,j=i2*n1*2; i1<n1*2; i1++,j++)
                rz[j] = z[j];
        }
        for (i1=0,j=n1*n2; i1<n1*2; i1+=2,j+=2) {
            rz[i1+1] = z[i1]-z[j];
            rz[i1] = z[i1]+z[j];
        }
        z = rz;

        /* set transform length, number of transforms and strides */
        n = n2;
        nt = n1;
        itmul = n1;
        itinc = 2;
    }

    /* initialize cosine-sine recurrence */
    theta = 2.0*PI/(double)n;
    if (isign>0) theta = -theta;
    wtemp = sin(0.5*theta);
    wpr = -2.0*wtemp*wtemp;
    wpi = sin(theta);
    wr = 1.0+wpr;
    wi = wpi;

    /* twiddle transforms simultaneously */
    for (j=2,k=n-2; j<=n/2; j+=2,k-=2) {
        jt = j*itmul;
        kt = k*itmul;
        for (it=0; it<nt; it++) {
            sumr = z[jt]+z[kt];
            sumi = z[jt+1]+z[kt+1];
            difr = z[jt]-z[kt];
            difi = z[jt+1]-z[kt+1];
            tempr = wi*difr-wr*sumi;
            tempi = wi*sumi+wr*difr;
            z[jt] = sumr+tempr;
            z[jt+1] = difi+tempi;
            z[kt] = sumr-tempr;
            z[kt+1] = tempi-difi;
            jt += itinc;
            kt += itinc;
        }
        wtemp = wr;
        wr += wr*wpr-wi*wpi;
        wi += wi*wpr+wtemp*wpi;
    }

    /* if transforming dimension 1 */
    if (idim==1) {

        /* transform as complex elements */
        pfa2cc(isign,1,n1/2,n2,(complex*)z);

    /* else, if transforming dimension 2 */
    } else {

        /* transform as complex elements */
        pfa2cc(isign,2,n1,n2/2,(complex*)z);

        /* unmerge even and odd vectors */
        temp = (float*)malloc(n1*sizeof(float));
        for (i2=0; i2<n2; i2+=2) {
            for (i1=0,j=i2*n1+1; i1<n1; i1++,j+=2)
                temp[i1] = z[j];
            for (i1=0,j=i2*n1,k=i2*n1; i1<n1; i1++,j+=2,k++)
                z[k] = z[j];
            for (i1=0,j=(i2+1)*n1; i1<n1; i1++,j++)
                z[j] = temp[i1];
        }
        free(temp);
    }
}
