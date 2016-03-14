/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION:  return smallest valid n not less than nmin for 
real-to-complex or complex-to-real prime factor ffts

PARAMETERS:
nmin        i lower bound on returned value (see notes below)

NOTES:
Current implemenations of real-to-complex and complex-to-real prime 
factor ffts require that the transform length n be even and that n/2 
be a valid length for a complex-to-complex prime factor fft.  The 
value returned by npfar satisfies these conditions.

Also, see notes for npfa.

AUTHOR:  I. D. Hale, Colorado School of Mines, 06/16/89
*/

#include "cwp.h"

int npfar (int nmin)
{
    return 2*npfa((nmin+1)/2);
}
