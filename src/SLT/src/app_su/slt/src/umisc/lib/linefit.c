#include "subc.h"


/* linear fit through data points */
/* input *x, *y of n element floating-point arrays 
   output a and b of y=a+bx 			*/
/*  Z. Li         */

void linefit(float *x, float *y, int n, float *a, float *b) {

	double xisum, yisum, xi2sum, xiyisum;
	int i;
	 
	xisum = 0.;
	yisum = 0.;
	xi2sum = 0.;
	xiyisum = 0.;

	for(i=0;i<n;i++) {
		xisum += x[i];
		yisum += y[i];
		xiyisum += x[i]*y[i];
		xi2sum += x[i]*x[i];
	}

	*a = (yisum*xi2sum - xiyisum*xisum)/(n*xi2sum-xisum*xisum);
	*b = (yisum - n*(*a))/xisum;
}
