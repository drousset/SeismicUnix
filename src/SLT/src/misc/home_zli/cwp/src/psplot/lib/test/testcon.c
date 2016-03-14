/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

#include <math.h>
#include "psplot.h"

#define NX 100
#define NY 200
#define NZ NX*NY
float x[NX],y[NY],z[NZ];

main()
{
	int ix,iy,iz;
	float xscale,xbase,yscale,ybase,rscale=1.0/2147483648.0;
	float dash[10];

	beginps();
	newpage("1",1);

	xscale = (500.0-100.0)/(float)(NX-1);
	xbase = 100.0;
	yscale = (600.0-100.0)/(float)(NY-1);
	ybase = 100.0;

	for (ix=0; ix<NX; ix++)
		x[ix] = xbase+ix*xscale;
	for (iy=0; iy<NY; iy++)
		y[iy] = ybase+iy*yscale;
	for (iy=0,iz=0; iy<NY; iy++)
		for (ix=0; ix<NX; ix++,iz++)
			z[iz] = sin(ix*0.1)*sin(iy*0.2);
			/* z[iz] = (float)(rand()*rscale); */

	setgray(0.1);
	setlinewidth(2.0);
	dash[0] = 3.0;
	setdash(dash,1,0.0);
	psContour(0.5,NX,x,NY,y,z);

	setgray(0.9);
	setlinewidth(2.0);
	dash[0] = 3.0;  dash[1] = 6.0;
	setdash(dash,2,0.0);
	psContour(-0.5,NX,x,NY,y,z);

	setgray(0.5);
	setlinewidth(1.0);
	setdash(dash,0,0.0);
	psContour(0.0,NX,x,NY,y,z);

	showpage();
	endps();
}
