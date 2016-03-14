/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION:  solve Vandermonde system of equations Vx=b (double version)

PARAMETERS:
n			i dimension of system
v			i array of 2nd row of Vandermonde matrix (1st row is all ones)
b			i array of right-hand-side column vector
x			o array of solution column vector

NOTES:
The arrays b and x may be equivalenced.

Adapted from Algorithm 5.6-2 in Golub, G. H., and Van Loan, C. F., 1983,
Matrix Computations, John-Hopkins University Press.

AUTHOR:  Dave Hale, Colorado School of Mines, 06/02/89
*/

void vanded (int n, double v[], double b[], double x[])
{
	int i,j;

	for (i=0; i<n; i++)
		x[i] = b[i];
	for (i=0; i<n-1; i++)
		for (j=n-1; j>i; j--)
			x[j] -= v[i]*x[j-1];
	for (i=n-1; i>0; i--) {
		for (j=i; j<n; j++)
			x[j] /= (v[j]-v[j-i]);
		for (j=i; j<n; j++)
			x[j-1] -= x[j];
	}
}
