/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SULCTHW: $Revision: 1.4 $ ; $Date: 2011/11/16 22:10:29 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"

#include "su.h"

/*********************** self documentation *****************************/
char *sdoc[] = {
" SULCTHW - Linear Coordinate Transformation of Header Words		",
"									",
"   sulcthw <infile >outfile						",
"									",
" xt=0.0	Translation of X					",
" yt=0.0	Translation of Y					",
" zt=0.0	Translation of Z					",
" xr=0.0	Rotation around X in degrees	 			",
" yr=0.0	Rotation aroun Y  in degrees	 			",
" zr=0.0	Rotation around Z in degrees 				",
"									",
" Notes:								",
" Translation:							",
" x = x'+ xt;y = y'+ yt;z = z' + zt;					",
"									",
" Rotations:					  			",
" Around Z axis							",
" X = x*cos(zr)+y*sin(zr);			  			",
" Y = y*cos(zr)-x*sin(zr);			  			",
" Around Y axis							",
" Z = z*cos(yr)+x*sin(yr);			  			",
" X = x*cos(yr)-z*sin(yr);			  			",
" Around X axis							",
" Y = y*cos(xr)+z*sin(xr);			  			",
" Z = Z*cos(xr)-y*sin(xr);			  			",
"									",
" Header words triplets that are transformed				",
" sx,sy,selev								",
" gx,gy,gelev								",
"									",
" The header words restored as double precision numbers using SEG-Y	",
" convention (with coordinate scalers scalco and scalel).		",
"									",
" After transformation they are converted back to			",
" short and stored, no.				 			",
NULL};

/*
 *  Credits: Potash Corporation of Saskatchewan: Balasz Nemeth   c. 2008
 *
 */

/**************** end self doc ********************************/
#define PP180 0.017453292

/* Segy data */
segy tr;				/* SEGY trace */

/* type defined to store coordinates */
typedef struct {
		double x;
		double y;
		double z;
} ctrp;

/* define transformation function */
ctrp transf(ctrp p,ctrp t,ctrp r);

int main( int argc, char *argv[] )
{

	/* declarations */
	ctrp p;
	ctrp pt;
	ctrp t;
	ctrp r;
	float tmp;

	/* Initargs */
	initargs(argc, argv);
   	requestdoc(1);
		
	
	/* Get parameters */
	if (!getparfloat("xt", &tmp)) tmp = 0.0;
	t.x = (double)tmp;
	if (!getparfloat("yt", &tmp)) tmp = 0.0;
	t.y = (double)tmp;
	if (!getparfloat("zt", &tmp)) tmp = 0.0;
	t.z = (double)tmp;
	if (!getparfloat("xr", &tmp)) tmp = 0.0;
	r.x = (double)tmp;
	if (!getparfloat("yr", &tmp)) tmp = 0.0;
	r.y = (double)tmp;
	if (!getparfloat("zr", &tmp)) tmp = 0.0;
	r.z = (double)tmp;
	
        checkpars();
	
	/* from degree to rad */
	r.x *= PP180;
	r.y *= PP180;
	r.z *= PP180;
	
	/* get info from first trace */
	if (!gettr(&tr)) err("can't get first trace");

	do {
		/* honor scalco and scalel */
		p.x = (double)tr.sx*pow(10.0,(double)tr.scalco);
		p.y = (double)tr.sy*pow(10.0,(double)tr.scalco);
		p.z = (double)tr.selev*pow(10.0,(double)tr.scalel);

		/* apply transformation */
		pt = transf(p,t,r);
		
		/* write new values to header words sx, sy, selev */
		tr.sx	=  NINT(pt.x/pow(10.0,(double)tr.scalco));
		tr.sy	=  NINT(pt.y/pow(10.0,(double)tr.scalco));
		tr.selev =  NINT(pt.z/pow(10.0,(double)tr.scalel));
		
		/* again, honor scalco */
		p.x = (double)tr.gx*pow(10.0,(double)tr.scalco);
		p.y = (double)tr.gy*pow(10.0,(double)tr.scalco);
		p.z = (double)tr.gelev*pow(10.0,(double)tr.scalel);
		
		/* transformation */
		pt = transf(p,t,r);
		
		/* write new values of gx,gy, gelev */
		tr.gx	=  NINT(pt.x/pow(10.0,(double)tr.scalco));
		tr.gy	=  NINT(pt.y/pow(10.0,(double)tr.scalco));
		tr.gelev =  NINT(pt.z/pow(10.0,(double)tr.scalel));
				
		/* output altered trace */
		puttr(&tr);

	} while(gettr(&tr));
	
	return(CWP_Exit());
}

ctrp transf(ctrp p,ctrp t,ctrp r)
/* linear transformation */
{

	ctrp pt;

	/* Translate */
	p.x +=t.x;
	p.y +=t.y;
	p.z +=t.z;
	
	/* Rotate */
	/* Z */
	pt.x = p.x*cos(r.z)+p.y*sin(r.z);
	pt.y = p.y*cos(r.z)-p.x*sin(r.z);
	pt.z = p.z;
	
	/* Y */
	pt.z = pt.z*cos(r.y)+pt.x*sin(r.y);
	pt.x = pt.x*cos(r.y)-pt.z*sin(r.y);
	
	/* X */
	pt.y = pt.y*cos(r.x)+pt.z*sin(r.x);
	pt.z = pt.z*cos(r.x)-pt.y*sin(r.x);
	
	return(pt);

}
