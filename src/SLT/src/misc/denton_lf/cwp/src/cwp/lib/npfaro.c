/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION:  return optimal n between nmin and nmax for
real-to-complex or complex-to-real prime factor ffts

PARAMETERS:
nmin        i lower bound on returned value (see notes below)
nmax        i desired (but not guaranteed) upper bound on returned value

NOTES:
Current implemenations of real-to-complex and complex-to-real prime 
factor ffts require that the transform length n be even and that n/2 
be a valid length for a complex-to-complex prime factor fft.  The 
value returned by npfaro satisfies these conditions.

Also, see notes for npfao.

AUTHOR:  I. D. Hale, Colorado School of Mines, 06/16/89
*/

#include "cwp.h"

int npfaro (int nmin, int nmax)
{
    return 2*npfao((nmin+1)/2,(nmax+1)/2);
}
