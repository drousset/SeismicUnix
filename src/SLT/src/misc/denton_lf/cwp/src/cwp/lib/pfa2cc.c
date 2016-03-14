/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION:  prime factor fft:  2-D complex to complex transforms, in place

PARAMETERS:
isign       i sign of isign is the sign of exponent in fourier kernel
idim        i dimension to transform, which must be either 1 or 2 (see notes)
n1          i 1st (fast) dimension of array to be transformed (see notes)
n2          i 2nd (slow) dimension of array to be transformed (see notes)
z           b array of complex elements to be transformed in place
 
NOTES:
Only one (either the 1st or 2nd) dimension of the 2-D array is transformed.

If idim equals 1, then n2 transforms of n1 complex elements are performed; 
else, if idim equals 2, then n1 transforms of n2 complex elements are 
performed.

Although z appears in the argument list as a one-dimensional array,
z may be viewed as an n1 by n2 two-dimensional array:  z[n2][n1].

Let n denote the transform length, either n1 or n2, depending on idim.
Then, n must be factorable into mutually prime factors taken 
from the set {2,3,4,5,7,8,9,11,13,16}.  in other words,
    n = 2**p * 3**q * 5**r * 7**s * 11**t * 13**u
where
    0 <= p <= 4,  0 <= q <= 2,  0 <= r,s,t,u <= 1
is required for pfa2cc to yield meaningful results.  this
restriction implies that n is restricted to the range
    1 <= n <= 720720 (= 5*7*9*11*13*16)

To perform a two-dimensional transform of an n1 by n2 complex array 
(assuming that both n1 and n2 are valid "n"), stored with n1 fast 
and n2 slow:  pfa2cc(isign,1,n1,n2,z);  pfa2cc(isign,2,n1,n2,z);
 
REFERENCES:  
Temperton, C., 1985, Implementation of a self-sorting
in-place prime factor fft algorithm:  Journal of
Computational Physics, v. 58, p. 283-299.

Temperton, C., 1988, A new set of minimum-add rotated
rotated dft modules: Journal of Computational Physics,
v. 75, p. 190-198.

AUTHOR:  I. D. Hale, Colorado School of Mines, 06/15/89
*/

#include "cwp.h"

void pfa2cc (int isign, int idim, int n1, int n2, complex cz[])
{
    int n,nt,k,kt;

    /* determine transform length, number of transforms, and strides */
    if (idim==1) {
        n = n1;
        nt = n2;
        k = 1;
        kt = n1;
    } else {
        n = n2;
        nt = n1;
        k = n1;
        kt = 1;
    }

    /* do multiple complex to complex transforms */
    pfamcc(isign,n,nt,k,kt,cz);
}
