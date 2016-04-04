/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "psplot.h"

main()
{
	beginps();

	newpage("1",1);
	psAxesBox(
		100.0,100.0,400.0,500.0,
		1.71,3.14,0.0,0.0,
		0.0,0.0,1,NONE,"Horizontal Axis",
		-10.0,31.0,0.0,0.0,
		0.0,0.0,1,NONE,"Vertical Axis",
		"Times-Roman",12.0,
		"Title","Times-Bold",24.0,
		NORMAL);
	showpage();

	newpage("2",2);
	psAxesBox(
		100.0,100.0,400.0,500.0,
		0.0,1000.0,0.0,0.0,
		0.0,0.0,5,DOT,"Horizontal Axis",
		10.0,0.0,0.0,0.0,
		0.0,0.0,5,DASH,"Vertical Axis",
		"Courier-Bold",18,
		"Title","Helvetica-Bold",24,
		NORMAL);
	showpage();

	newpage("3",3);
	psAxesBox(
		100.0,100.0,400.0,500.0,
		0.0,1000.0,0.0,0.0,
		0.0,0.0,5,DOT,"Horizontal Axis",
		10.0,0.0,0.0,0.0,
		0.0,0.0,5,DASH,"Vertical Axis",
		"Courier-Bold",18,
		"Title","Helvetica-Bold",24,
		SEISMIC);
	showpage();

	endps();
}
