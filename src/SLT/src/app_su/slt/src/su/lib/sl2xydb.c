#include "usu.h"
#include "par.h"
/* from s and l coordinates, compute x and y positions */ 
/* author: z. li	      		9/94 	*/
/*
	| y
	|   .l	      * (x4,y4)
	|    .     .    .
	|     .  .       .         . s 
       (x3,y3) *	  .      .   
	|	 .          .  . 
	|	  .          * (x2,y2)
	|	   .       .
	|	     .  .
	|	      * (x1,y1)
	|
	|    
	|
	|--------------------------------- x

given (x,y) and (s,l) of 3 corners of a rantangular region, this routine
compute (x,y) position of an input (s,l) point 

	x1,y1		smallest s and smallest l position
	x2,y2		largest s and smallest l position
	x3,y3		smallest s and largest l position
	s		shotpoint position within a 3d line
	l		line position 
*/

void sl2xydb(double s1, double l1, double x1, double y1, 
	   double s2, double l2, double x2, double y2, 
	   double s3, double l3, double x3, double y3, 
	   double s,  double l,  double *x, double *y) {

	static int ifirst=1;
	static double dxds,dxdl,dyds,dydl;

	if(ifirst==1) {
		/* error checking */
		if(l1!=l2) err("l1=%g not the same as l2=%g",l1,l2);
		if(s1!=s3) err("s1=%g not the same as s3=%g",s1,s3);

		ifirst = 0;
		dxds = (x2-x1)/(s2-s1);
		dxdl = (x3-x1)/(l3-l1);
		dyds = (y2-y1)/(s2-s1);
		dydl = (y3-y1)/(l3-l1);
	}

	*x =  x1 + (s-s1)*dxds+(l-l1)*dxdl;
	*y =  y1 + (s-s1)*dyds+(l-l1)*dydl; 

}
