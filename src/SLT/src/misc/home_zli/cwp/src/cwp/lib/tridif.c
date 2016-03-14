/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION:  solve a tridiagonal linear system of equations Tu=r for u
(float version)

PARAMETERS:
n		i dimension of system
a		i coefficients of lower sub-diagonal of T (a[0] ignored)
b		i coefficients of diagonal of T
c		i coefficients of upper super-diagonal of T (c[n-1] ignored)
r		i array of right-hand-side column vector
u		o array of solution (left-hand-side) column vector

NOTES:
For example, a tridiagonal system of dimension 4 is specified as:

    |b[0]    c[0]     0       0  | |u[0]|     |r[0]|
    |a[1]    b[1]    c[1]     0  | |u[1]|  =  |r[1]|
    | 0      a[2]    b[2]    c[2]| |u[2]|     |r[2]|
    | 0       0      a[3]    b[3]| |u[3]|     |r[3]|

The tridiagonal matrix is assumed to be non-singular.

AUTHOR:  Dave Hale, Colorado School of Mines, 10/03/89
*/

#include <stdlib.h>

void tridif (int n, float a[], float b[], float c[], float r[], float u[])
{
	int j;
	float t,*w;
	
	w = (float*)malloc(n*sizeof(float));
	t = b[0];
	u[0] = r[0]/t;
	for (j=1; j<n; j++) {
		w[j] = c[j-1]/t;
		t = b[j]-a[j]*w[j];
		u[j] = (r[j]-a[j]*u[j-1])/t;
	}
	for (j=n-2; j>=0; j--)
		u[j] -= w[j+1]*u[j+1];
	free(w);
}
