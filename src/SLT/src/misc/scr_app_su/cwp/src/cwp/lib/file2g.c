#include "cwp.h"

void file2g(FILE *fp) {
	off_t lofset;
	long long n;

	n = 0;
	bcopy(&n,&lofset,8);	
	fseeko(fp,lofset,1);
}
