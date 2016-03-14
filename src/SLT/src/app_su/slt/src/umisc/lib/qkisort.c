
#include "subc.h"

/* a fortran interface to cwp's qkisort c subroutine */
/* call example in fortran:

	call qkisort(n,a,i)

   input:
	n	int*4 		number of elements to sort
	a	real*4		values to sort
	i	int*4		index (0,1,2,...,n-1)
   output:
	i	int*4		sorted index
				
Note:
	fortran program after calling qkisort, do the following

		do j=1,n
			i(j) = i(j) + 1
		end do
	
	to use the index i properly
*/
       
	
void qkisort_(int *n, float *a, int *i) {
	qkisort(*n,a,i);
}

void qkfind_(int *m, int *n, float *a) {
    qkfind(*m,*n,a);
}

