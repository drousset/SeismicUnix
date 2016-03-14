/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

#include <stdio.h>
#include <math.h>
#include "psplot.h"

#define INCHW 4
#define INCHH 6
#define RES 72

#define W INCHW*RES
#define H INCHH*RES
#define ROW (1+(W-1)/8)
#define NBYTES ROW*H
unsigned char bits[NBYTES];
float m[6] = {W,0,0,-H,0,H};

#define NZ 100
float z[NZ];

main()
{
	int i;

	for (i=0; i<NBYTES; i++)
		bits[i] = 0;
	for (i=0; i<NZ; i++)
		z[i] = sin(i*10.0/NZ);
	rfwtva(NZ,z,-1.0,1.0,0.0,
		0,W-1,0,H-1,
		1,ROW,bits);
	for (i=0; i<NBYTES; i++)
		bits[i] = ~bits[i];

	beginps();
	newpage("1",1);

	translate(10.0,10.0);
	scale(INCHW*72.0,INCHH*72.0);
	image(W,H,1,m,bits);

	showpage();
	endps();
}
