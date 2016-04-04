/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include <math.h>
#include "psplot.h"

#define N 398

main()
{
	int i,fill=1;
	float z[N],zmin=-1.0,zmax=1.0,zbase=0.0,
		yzmin=100,yzmax=300,xfirst=100,xlast=500;

	for (i=0; i<N; i++)
		z[i] = cos(i*0.1);

	beginps();
	newpage("1",1);

	setlinewidth(0.0);
	psWiggle(N,z,zmin,zmax,zbase,yzmin,yzmax,xfirst,xlast,fill);

	showpage();
	endps();
}
