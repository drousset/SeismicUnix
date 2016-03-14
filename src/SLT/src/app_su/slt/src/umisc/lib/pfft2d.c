#include "subc.h"
/* fortran callable 2-d fft of 2-d data routines */
/* see cwplib for documentation */

/* read to complex fft */ 
void pfa2rc_(int *isign,int *idim,int *n1,int *n2,float *rz,complex *cz) {
	pfa2rc(*isign,*idim,*n1,*n2,rz,cz);
}
/* complex to complex fft */ 
void pfa2cc_(int *isign,int *idim,int *n1,int *n2,complex *cz) {
	pfa2cc(*isign,*idim,*n1,*n2,cz);
}
/* complex to real fft */ 
void pfa2cr_(int *isign,int *idim,int *n1,int *n2,complex *cz,float *rz) {
	pfa2cr(*isign,*idim,*n1,*n2,cz,rz);
}
