/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*****************************************************************************
Allocate and free multi-dimensional arrays with error reports.
These routines simply call those in ...cwp/lib/alloc.c and issue
an error message via the syserr routine if the underlying malloc
came up empty.  See alloc.c for notes on the routines.

Author:    	Jack
Credit:		Dave for the underlying routines
*****************************************************************************/

#include "par.h"
#define ERROR	NULL

/* allocate a 1-d array */
void *ealloc1 (size_t n1, size_t size)
{
	void *p;

	if (ERROR == (p=alloc1(n1, size)))
		syserr("%s: malloc failed", __FILE__);
	return p;
}


/* re-allocate a 1-d array */
void *erealloc1 (void *v, size_t n1, size_t size)
{
	void *p;

	if (ERROR == (p=realloc1(v, n1, size)))
		syserr("%s: realloc failed", __FILE__);
	return p;
}


/* allocate a 2-d array */
void **ealloc2 (size_t n1, size_t n2, size_t size)
{
	void **p;

	if (ERROR == (p=alloc2(n1, n2, size)))
		syserr("%s: malloc failed", __FILE__);
	return p;
}


/* allocate a 3-d array */
void ***ealloc3 (size_t n1, size_t n2, size_t n3, size_t size)
{
	void ***p;

	if (ERROR == (p=alloc3(n1, n2, n3, size)))
		syserr("%s: malloc failed", __FILE__);
	return p;
}


/* allocate a 4-d array */
void ****ealloc4 (size_t n1, size_t n2, size_t n3, size_t n4, size_t size)
{
	void ****p;

	if (ERROR == (p=alloc4(n1, n2, n3, n4, size)))
		syserr("%s: malloc failed", __FILE__);
	return p;
}


/* allocate a 1-d array of ints */
int *ealloc1int(size_t n1)
{
	int *p;

	if (ERROR == (p=alloc1int(n1)))
		syserr("%s: malloc failed", __FILE__);
	return p;
}


/* re-allocate a 1-d array of ints */
int *erealloc1int(int *v, size_t n1)
{
	int *p;

	if (ERROR == (p=realloc1int(v,n1)))
		syserr("%s: realloc failed", __FILE__);
	return p;
}


/* allocate a 2-d array of ints */
int **ealloc2int(size_t n1, size_t n2)
{
	int **p;

	if (ERROR == (p=alloc2int(n1, n2)))
		syserr("%s: malloc failed", __FILE__);
	return p;
}


/* allocate a 3-d array of ints */
int ***ealloc3int(size_t n1, size_t n2, size_t n3)
{
	int ***p;

	if (ERROR == (p=alloc3int(n1, n2, n3)))
		syserr("%s: malloc failed", __FILE__);
	return p;
}


/* allocate a 1-d array of floats */
float *ealloc1float(size_t n1)
{
	float *p;

	if (ERROR == (p=alloc1float(n1)))
		syserr("%s: malloc failed", __FILE__);
	return p;
}


/* re-allocate a 1-d array of floats */
float *erealloc1float(float *v, size_t n1)
{
	float *p;

	if (ERROR == (p=realloc1float(v, n1)))
		syserr("%s: realloc failed", __FILE__);
	return p;
}


/* allocate a 2-d array of floats */
float **ealloc2float(size_t n1, size_t n2)
{
	float **p;

	if (ERROR == (p=alloc2float(n1, n2)))
		syserr("%s: malloc failed", __FILE__);
	return p;
}


/* allocate a 3-d array of floats */
float ***ealloc3float(size_t n1, size_t n2, size_t n3)
{
	float ***p;

	if (ERROR == (p=alloc3float(n1, n2, n3)))
		syserr("%s: malloc failed", __FILE__);
	return p;
}


/* allocate a 1-d array of doubles */
double *ealloc1double(size_t n1)
{
	double *p;

	if (ERROR == (p=alloc1double(n1)))
		syserr("%s: malloc failed", __FILE__);
	return p;
}


/* re-allocate a 1-d array of doubles */
double *erealloc1double(double *v, size_t n1)
{
	double *p;

	if (ERROR == (p=realloc1double(v, n1)))
		syserr("%s: realloc failed", __FILE__);
	return p;
}


/* allocate a 2-d array of doubles */
double **ealloc2double(size_t n1, size_t n2)
{
	double **p;

	if (ERROR == (p=alloc2double(n1, n2)))
		syserr("%s: malloc failed", __FILE__);
	return p;
}


/* allocate a 3-d array of doubles */
double ***ealloc3double(size_t n1, size_t n2, size_t n3)
{
	double ***p;

	if (ERROR == (p=alloc3double(n1, n2, n3)))
		syserr("%s: malloc failed", __FILE__);
	return p;
}


/* allocate a 1-d array of complexs */
complex *ealloc1complex(size_t n1)
{
	complex *p;

	if (ERROR == (p=alloc1complex(n1)))
		syserr("%s: malloc failed", __FILE__);
	return p;
}


/* re-allocate a 1-d array of complexs */
complex *erealloc1complex(complex *v, size_t n1)
{
	complex *p;

	if (ERROR == (p=realloc1complex(v, n1)))
		syserr("%s: realloc failed", __FILE__);
	return p;
}


/* allocate a 2-d array of complexs */
complex **ealloc2complex(size_t n1, size_t n2)
{
	complex **p;

	if (ERROR == (p=alloc2complex(n1, n2)))
		syserr("%s: malloc failed", __FILE__);
	return p;
}


/* allocate a 3-d array of complexs */
complex ***ealloc3complex(size_t n1, size_t n2, size_t n3)
{
	complex ***p;

	if (ERROR == (p=alloc3complex(n1, n2, n3)))
		syserr("%s: malloc failed", __FILE__);
	return p;
}



#ifdef TEST
main()
{
	short   *hv, **hm;
	int     *iv, **im;
	float   *fv, **fm;
	double  *dv, **dm;
	size_t i1, i2;
	size_t n1, n2;

	scanf("%d %*[^\n]", &n1);
	scanf("%d %*[^\n]", &n2);

	/* Exercise 1-d routines */
	hv = (short *) ealloc1(n1, sizeof(short));
	iv = ealloc1int(n1);
	fv = ealloc1float(n1);
	dv = ealloc1double(n1);

	for (i1 = 0; i1 < n1; ++i1) {
		hv[i1] = i1;
		iv[i1] = i1;
		fv[i1]  = (float) i1;
		dv[i1] = (double) i1;
	}

	printf("short vector:\n");
	for (i1 = 0; i1 < n1; ++i1) {
		printf("hv[%d] = %hd\n", i1, hv[i1]);
	}
	putchar('\n');

	printf("int vector:\n");
	for (i1 = 0; i1 < n1; ++i1) {
		printf("iv[%d] = %d\n", i1, iv[i1]);
	}
	putchar('\n');

	printf("float vector:\n");
	for (i1 = 0; i1 < n1; ++i1) {
		printf("fv[%d] = %f\n", i1, fv[i1]);
	}
	putchar('\n');

	printf("double vector:\n");
	for (i1 = 0; i1 < n1; ++i1) {
		printf("dv[%d] = %lf\n", i1, dv[i1]);
	}
	putchar('\n');


	free1(hv);
	free1int(iv);
	free1float(fv);
	free1double(dv);


	/* Exercise 2-d routines */
	hm = (short **) ealloc2(n1, n2, sizeof(short));
	im = ealloc2int(n1, n2);
	fm = ealloc2float(n1, n2);
	dm = ealloc2double(n1, n2);

	for (i2 = 0; i2 < n2; ++i2) {
		for (i1 = 0; i1 < n1; ++i1) {
			hm[i2][i1] = i1 + 2*i2;
			im[i2][i1] = i1 + 2*i2;
			fm[i2][i1] = (float) (i1 + 2*i2);
			dm[i2][i1] = (double) (i1 + 2*i2);
		}
	}

	printf("short matrix:\n");
	for (i2 = 0; i2 < n2; ++i2) {
		for (i1 = 0; i1 < n1; ++i1) {
			printf("hm[%d, %d] = %hd ", i2, i1, hm[i2][i1]);
		}
		putchar('\n');
	}
	putchar('\n');

	printf("int matrix:\n");
	for (i2 = 0; i2 < n2; ++i2) {
		for (i1 = 0; i1 < n1; ++i1) {
			printf("im[%d, %d] = %d ", i2, i1, im[i2][i1]);
		}
		putchar('\n');
	}
	putchar('\n');

	printf("float matrix:\n");
	for (i2 = 0; i2 < n2; ++i2) {
		for (i1 = 0; i1 < n1; ++i1) {
			printf("fm[%d, %d] = %f ", i2, i1, fm[i2][i1]);
		}
		putchar('\n');
	}
	putchar('\n');

	printf("double matrix:\n");
	for (i2 = 0; i2 < n2; ++i2) {
		for (i1 = 0; i1 < n1; ++i1) {
			printf("dm[%d, %d] = %lf ", i2, i1, dm[i2][i1]);
		}
		putchar('\n');
	}
	putchar('\n');

	free2((void*)hm);
	free2int(im);
	free2float(fm);
	free2double(dm);

	return EXIT_SUCCESS;
}
#endif
