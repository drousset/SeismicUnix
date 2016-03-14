#include "usu.h"
#include "par.h"
/* from x and y coordinates, compute shotpoint and line positions */ 
/* author: z. li	      		2/92 	*/
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
compute (s,l) position of an input (x,y) point 

	x1,y1		smallest s and smallest l position
	x2,y2		largest s and smallest l position
	x3,y3		smallest s and largest l position
	s		shotpoint position within a 3d line
	l		line position 
*/

void xy2sldb(double x1, double y1, double s1, double l1, 
	   double x2, double y2, double s2, double l2, 
	   double x3, double y3, double s3, double l3, 
	   double x,  double y,  double *s, double *l) {

	static int ifirst=1;
	static double dl, ds;

	if(ifirst==1) {
		/* error checking */
		/*
		if(l1>l2 || l1>l3 || s1>s2 || s1>s3 ) {
			err("(x1,y1) not at smallest (s,l) position");
		}
		*/
		if(l1!=l2) err("l1=%g not the same as l2=%g",l1,l2);
		if(s1!=s3) err("s1=%g not the same as s3=%g",s1,s3);

		ifirst = 0;

		if(x1==x2 && y1==y2) {
			ds = 1.;
		} else {
			ds = (s2-s1)/((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1));
		}

		if(x1==x3 && y3==y1) {
			dl = 1.;
		} else {	
			dl = (l3-l1)/((x3-x1)*(x3-x1)+(y3-y1)*(y3-y1));
		}
	}

	*s =  s1 + ( (x-x1)*(x2-x1)+(y-y1)*(y2-y1) ) * ds; 
	*l =  l1 + ( (x-x1)*(x3-x1)+(y-y1)*(y3-y1) ) * dl; 

}
