/* CONV_FLOAT: $Revision: 1.2 $ ; $Date: 90/08/08 16:42:48 $	*/

/*----------------------------------------------------------------------
 * Copyright (c) Colorado School of Mines, 1989.
 * All rights reserved.
 *
 * This code is part of SU.  SU stands for Seismic Unix, a processing line
 * developed at the Colorado School of Mines, partially based on Stanford
 * Exploration Project (SEP) software.  Inquiries should be addressed to:
 *
 *  Jack K. Cohen, Center for Wave Phenomena, Colorado School of Mines,
 *  Golden, CO 80401  (isis!csm9a!jkcohen)
 *----------------------------------------------------------------------
 */

#include "su.h"

/* conv_float - convert between 32 bit IBM and IEEE floating numbers
 *
 * Credits:
 *	CWP: Brian
 *            : H. Lam and Z. Li
 *              fixed bug (a factor of 4) when doing IBM-IEEE conversion  
 *              of the previous version
 *
 * Parameters:
 *    from	- input vector
 *    to	- output vector, can be same as input vector
 *    len	- number of floats in vectors
 *    type	- conversion type (1=imb2ieee  2=ieee2ibm)
 *
 * Notes:
 *	Up to 3 bits lost on IEEE -> IBM
 *
 *	IBM -> IEEE may overflow or underflow, taken care of by 
 *	substituting large number or zero
 *
 *	Byte swapping branches have been inserted to ease rewriting
 *	this machine dependent subroutine for other machines (eg. VAX).
 *
 *
 */

#define	IBMtoIEEE	1
#define IEEEtoIBM	2


void conv_float(char *from, char *to,int len, int type)
{

	unsigned fconv, fsign, fexpn;
	register unsigned fmant;
	register int t;
	register char *bp = (char *) &fconv;


	if (len <= 0) return;

	switch(type) {
	case IEEEtoIBM:
		while (len--) {

			/* Load. Swap bytes if necessary for Vax */
			bp[0] = from[0];
			bp[1] = from[1];
			bp[2] = from[2];
			bp[3] = from[3];
			from += 4;

			fsign = (unsigned) 0x80000000 & fconv;
			fexpn = (unsigned) 0x7f800000 & fconv;
			fmant = ((unsigned) 0x007fffff & fconv) |
                                (unsigned) 0x00800000; /* add 1 at 9-th bit */

			if (!fexpn) fconv = 0;
			else {
				/* IEEE 1.Fieee    IBM 0.Fibm
				   add 1 at 9-th bit position of Fieee and
				   right-shift 1 to get 0.Fibm.
				   Also left-shift 1 to get proper bit
				   position for Fibm.
				   This means add 1 at 9-th bit position
			           of Fieee and add 1 to t */

				t = (int) (fexpn >> 23) - 127 + 1;
				while (t & 0x7) {
					++t;
					fmant >>= 1;
				}
			
				fexpn = (unsigned) ((t>>2) + 64) << 24;
				fconv = fsign | fexpn | fmant;
			}

			*to++ = bp[0];
			*to++ = bp[1];
			*to++ = bp[2];
			*to++ = bp[3];
		}
		break;
	
	case IBMtoIEEE:
		while (len--) {

			/* Load. Swap bytes if necessary */
			bp[0] = from[0];
			bp[1] = from[1];
			bp[2] = from[2];
			bp[3] = from[3];
			from += 4;

			fsign = (unsigned) 0x80000000 & fconv;
			fexpn = (unsigned) 0x7f000000 & fconv;
			fmant = (unsigned) 0x00ffffff & fconv;

			if (!fmant) fconv = 0;
			else {
				t = (int) (fexpn >> 22) - 256;

				while (!(fmant & 0x00800000)) {
					--t;
					fmant <<= 1;
				}


				/* Check for valid IEEE exponent */

				if (t > 127) {
					fconv = fsign | 0x7ff80000;
				} else if (t < -128) {
					fconv = 0;
				} else if (t==-127) {
					t = 0;
					fmant>>=1;
					fexpn = ((unsigned) (t)) << 23;
					fconv = fsign | fexpn |
						(0x007fffff & fmant);
				} else {
					fexpn = ((unsigned) (t+127-1)) << 23;

					/* IEEE 1.Fieee    IBM 0.Fibm
					   Fibm left-shift 1 to get 1.Fieee 
					   right-shift 1 to get proper bit
					   position for Fieee 
					   This means no shift, drop the
					   first bit of Fibm , and add -1 to
				           fexpn		*/

					fconv = fsign | fexpn |
						(0x007fffff & fmant);
				}
			}

			*to++ = bp[0];
			*to++ = bp[1];
			*to++ = bp[2];
			*to++ = bp[3];
		}
		break;
	
	default:
		break;
	}
	return;
}
