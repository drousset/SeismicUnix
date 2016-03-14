/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "psplot.h"

#define N 100

main()
{
	int i;
	float x,y,rscale=1.0/2147483648.0;

	beginps();
	newpage("1",1);
	setlinewidth(4.0);
	for (i=0; i<N; i++) {
		x = (float)(rand()*rscale)*612;
		y = (float)(rand()*rscale)*792;
		markto(x,y,i,10.0);
	}
	showpage();
	endps();
}
