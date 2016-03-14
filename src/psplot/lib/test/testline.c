/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "psplot.h"

#define N 1000
float x[N],y[N];

main()
{
	int i;
	float rscale=1.0/2147483648.0;

	beginps();
	newpage("1",1);
	for (i=0; i<N; i++) {
		x[i] = (float)(rand()*rscale);
		y[i] = (float)(rand()*rscale); /* (float)i/N; */
	}
	scale(72.0,72.0);
	rectclip(2.0,2.0,4.5,7.0);
	scale(8.5,11.0);
	setlinewidth(0.0);
	polyline(x,y,N);
	showpage();
	endps();
}
