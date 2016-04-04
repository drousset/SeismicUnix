/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include <math.h>
#include <stdio.h>
#include "psplot.h"

#define W 144
#define H 144
#define BPS 8
#define ROW (1+(W*BPS-1)/8)
#define NZ ROW*H
#define INCHW 4
#define INCHH 4

unsigned char z[NZ];

main()
{
	int i;
	float m[6],rscale=1.0/2147483648.0;

	beginps();
	newpage("1",1);

	for (i=0; i<NZ; i++)
		z[i] = 255.0*rand()*rscale;

	translate(100.0,100.0);
	scale(INCHW*72.0,INCHH*72.0);
	m[0] = W;  m[1] = 0;  m[2] = 0;  m[3] = -H;  m[4] = 0;  m[5] = H;
	image(W,H,BPS,m,z);

	showpage();
	endps();
}
