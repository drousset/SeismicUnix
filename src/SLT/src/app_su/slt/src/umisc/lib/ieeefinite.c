
#include "subc.h"

/* detect if an ieee floating point number is finite */
/* return 1 if finite, 0 otherwise */

int ieeefinite(float *x) {
	unsigned fexpn;
	unsigned fx;
        register int t;
	register char *bp = (char *) &fx;

	bcopy(x,bp,4);

	fexpn = (unsigned) 0x7f800000 & fx;
	t = (int) (fexpn >> 23) - 128;
	while (t & 0x7) ++t;
	if(t==128) {
		return 0;
	} else {
		return 1;
	}
}
