/* Copyright (c) Colorado School of Mines, 1990.*/
/* All rights reserved.                       */

#include "cwp.h"
#include "psplot.h"

void psCubeAxesBox(
	float x,float y,float size1,float size2,float size3,float angle,
	float x1Beg, float x1End, float p1Beg, float p1End, 
	float d1Num, float f1Num, int n1Tic, int grid1, char *label1,
	float x2Beg, float x2End, float p2Beg, float p2End,
	float d2Num, float f2Num, int n2Tic, int grid2, char *label2,
	float x3Beg, float x3End, float p3Beg, float p3End,
	float d3Num, float f3Num, int n3Tic, int grid3, char *label3,
	char *labelFont, float labelSize,
	char *title, char *titleFont, float titleSize)
/*
FUNCTION:  plot axes, tics, labels and titles of a cube via PostScript

PARAMETERS:
x		 x coordinate of lower left corner of box
y		 y coordinate of lower left corner of box
size1		 size of 1st dimension of the input cube
size2		 size of 2nd dimension of the input cube
size3		 size of 3rd dimension of the input cube
angle		 projection angle of the cube	 
x1Beg  		 axis value at beginning of axis 1
x1End		 axis value at end of axis 1
p1Beg  		 pad value at beginning of axis 1
p1End		 pad value at end of axis 1
d1Num		 numbered tic increment for axis 1 (0.0 for automatic)
f1Num		 first numbered tic for axis 1
n1Tic		 number of tics for axis 1
grid1		 grid code for axis 1:  NONE, DOT, DASH, or SOLID
label1		 label for axis 1
x2Beg  		 axis value at beginning of axis 2
x2End		 axis value at end of axis 2
p2Beg  		 pad value at beginning of axis 2
p2End		 pad value at end of axis 2
d2Num		 numbered tic increment for axis 2 (0.0 for automatic)
f2Num		 first numbered tic for axis 2
n2Tic		 number of tics for axis 2
grid2		 grid code for axis 2:  NONE, DOT, DASH, or SOLID
label2		 label for axis 2
x3Beg  		 axis value at beginning of axis 3
x3End		 axis value at end of axis 3
p3Beg  		 pad value at beginning of axis 3
p3End		 pad value at end of axis 3
d3Num		 numbered tic increment for axis 3 (0.0 for automatic)
f3Num		 first numbered tic for axis 3
n3Tic		 number of tics for axis 3
grid3		 grid code for axis 3:  NONE, DOT, DASH, or SOLID
label3		 label for axis 3
labelFont	 name of font to use for axes labels
labelSize	 size of font to use for axes labels
title		 axes box title
titleFont	 name of font to use for title
titleSize	 size of font to use for title

Author: 	 Zhiming Li & Dave Hale,     6/90    CSM 
Modified: 	 Craig Artley,     3/12/93    CSM 
			Changed name to psCubeAxesBox (from psAxes3),
			fixed minor bugs.
*/
{
	float xxx,yyy,xx,yy;
	int n1num,n2num,n3num,ntic,ndash,grided;
	float xa,ya,ticsize,dnum,fnum,dtic,amin,amax,azero,
		width,height,base,scale,anum,atic,
		ticb,numb,labelb,dash[2],
		labelCH,labelCW,labelCA,labelCD,
		titleCH,titleCW,titleCA,titleCD;
	char str[256];

	/* determine font dimensions */
	labelCH = fontheight(labelFont,labelSize);
	labelCW = fontwidth(labelFont,labelSize);
	labelCA = fontascender(labelFont,labelSize);
	labelCD = fontdescender(labelFont,labelSize);
	titleCH = fontheight(titleFont,titleSize);
	titleCW = fontwidth(titleFont,titleSize);
	titleCA = fontascender(titleFont,titleSize);
	titleCD = fontdescender(titleFont,titleSize);

	/* determine numbered tic intervals */
	if (d1Num==0.0) {
		n1num = size1/(4*labelCW);
		scaxis(x1Beg,x1End,&n1num,&d1Num,&f1Num);
	}
	if (d2Num==0.0) {
		n2num = size2/(4*labelCW);
		scaxis(x2Beg,x2End,&n2num,&d2Num,&f2Num);
	}
	if (d3Num==0.0) {
		n3num = size3/(4*labelCW);
		scaxis(x3Beg,x3End,&n3num,&d3Num,&f3Num);
	}


	/* save graphic status */
	gsave();

	/* set gray (for black axes) */
	setgray(0.0);

	/* translate coordinate system, so that origin is at x,y */
	translate(x,y);

	/* erase two triagles */
	gsave();
	newpath();
	yyy = size1; xxx=0.;
	moveto(xxx,yyy);
	yyy = yyy + size3*sin(angle);
	lineto(xxx,yyy);
	xxx = xxx + size3*cos(angle);
	lineto(xxx,yyy);
	closepath();
	setgray(1.);
	fill();
	grestore();

	gsave();
	newpath();
	yyy = 0.; xxx=size2;
	moveto(xxx,yyy);
	xxx = xxx + size3*cos(angle);
	lineto(xxx,yyy);
	yyy = yyy + size3*sin(angle);
	lineto(xxx,yyy);
	closepath();
	setgray(1.0);
	fill();
	grestore();
	

	/* set font and character size */
	setfont(labelFont,labelSize);

	/* determine tic size */
	ticsize = 0.3*labelCH;

	height = size1 + size3*sin(angle); 
	width = size2 + size3*cos(angle); 

	/* draw axis 1 */
	translate(0.,size1);
	rotate(-90.0);

	amin = (x1Beg<x1End)?x1Beg:x1End;
	amax = (x1Beg>x1End)?x1Beg:x1End;
	azero = 0.0001*(amax-amin);
	dnum = d1Num;  fnum = f1Num;  ntic = n1Tic;
	scale = size1/(x1End+p1End-x1Beg-p1Beg);
	base = -scale*(x1Beg+p1Beg);
	ticb = -ticsize;
	numb = ticb+labelCD;
	labelb = numb-2.0*labelCW;
	for (anum=fnum; anum<=amax; anum+=dnum) {
		if (anum<amin) continue;
		xa = base+scale*anum;
		moveto(xa,0.0);  lineto(xa,ticb);
		if (anum>-azero && anum<azero)
			sprintf(str,"%.4g",0.0);
		else
			sprintf(str,"%.4g",anum);
		xx = xa + 0.3*labelCH; 
		moveto(xx,numb);
		rotate(90.);
		justshow(-1.0,str);
		rotate(-90.);
	}
	stroke();
	dtic = dnum/ntic;
	for (atic=fnum-ntic*dtic-dtic; atic<=amax; atic+=dtic) {
		if (atic<amin) continue;
		xa = base+scale*atic;
		moveto(xa,0.0);  lineto(xa,ticb/2);
	}
	stroke();
	if (grid1==SOLID) {
		grided = 1;
		ndash = 0;
	} else if (grid1==DASH) {
		grided = 1;
		ndash = 1;  dash[0] = 10;
	} else if (grid1==DOT) {
		grided = 1;
		ndash = 2;  dash[0] = 1;  dash[1] = 5;
	} else
		grided = 0;
	if (grided) {
		for (anum=fnum; anum<=amax; anum+=dnum) {
			if (anum<amin) continue;
			xa = base+scale*anum;
			moveto(xa,0.0);  lineto(xa,size2);
			xa = xa - size3*sin(angle);
			ya = size2+size3*cos(angle);
			lineto(xa,ya);
		}
		setdash(dash,ndash,0.0);
		stroke();
		setdash(dash,0,0.0);
	}
	moveto(size1/2.0,labelb);
	rotate(180.);
	justshow(-0.5,label1);
	rotate(-180.);

	rotate(90.0);
	translate(0.,-size1);


	/* draw axis 2 */
	xxx = width - size2; 
	translate(xxx,height);
	rotate(-90.);

	amin = (x2Beg<x2End)?x2Beg:x2End;
	amax = (x2Beg>x2End)?x2Beg:x2End;
	azero = 0.0001*(amax-amin);
	dnum = d2Num;  fnum = f2Num;  ntic = n2Tic;
	scale = size2/(x2End+p2End-x2Beg-p2Beg);
	base = -scale*(x2Beg+p2Beg);
	ticb = -ticsize;
	numb = ticb+labelCD;
	labelb = numb-labelCW;
	for (anum=fnum; anum<=amax; anum+=dnum) {
		if (anum<amin) continue;
		ya = base+scale*anum;
		moveto(0.0,ya);  lineto(ticb,ya);
		if (anum>-azero && anum<azero)
			sprintf(str,"%.4g",0.0);
		else
			sprintf(str,"%.4g",anum);
		moveto(numb,ya); 
		rotate(90.0); 
		justshow(-0.5,str);
		rotate(-90.0);
	}
	stroke();
	dtic = dnum/ntic;
	for (atic=fnum-ntic*dtic-dtic; atic<=amax; atic+=dtic) {
		if (atic<amin) continue;
		ya = base+scale*atic;
		moveto(0.0,ya);  lineto(ticb/2,ya);
	}
	stroke();
	if (grid2==SOLID) {
		grided = 1;
		ndash = 0;
	} else if (grid2==DASH) {
		grided = 1;
		ndash = 1;  dash[0] = 10;
	} else if (grid2==DOT) {
		grided = 1;
		ndash = 2;  dash[0] = 1;  dash[1] = 5;
	} else
		grided = 0;
	if (grided) {
		for (anum=fnum; anum<=amax; anum+=dnum) {
			if (anum<amin) continue;
			ya = base+scale*anum;
			moveto(0.0,ya);  
			ya = ya-size3*cos(angle);
			xa = size3*sin(angle);
			lineto(xa,ya);
			xa = xa+size1;
			lineto(xa,ya);
		}
		setdash(dash,ndash,0.0);
		stroke();
		setdash(dash,0,0.0);
	}
	moveto(labelb,size2/2.0);
	rotate(90.0);
	justshow(-0.5,label2);

	xxx = width - size2; 
	translate(-xxx,-height);
	
	/* draw axis 3 */
	xxx = size1; 
	translate(0.,xxx);
	yyy = angle*180./3.141592654;
	rotate(yyy);

	amin = (x3Beg<x3End)?x3Beg:x3End;
	amax = (x3Beg>x3End)?x3Beg:x3End;
	azero = 0.0001*(amax-amin);
	dnum = d3Num;  fnum = f3Num;  ntic = n3Tic;
	scale = size3/(x3End+p3End-x3Beg-p3Beg);
	base = -scale*(x3Beg+p3Beg);
	ticb = ticsize;
	numb = ticb-labelCD;
	labelb = numb+labelCW;
	for (anum=fnum; anum<=amax; anum+=dnum) {
		if (anum<amin) continue;
		xa = base+scale*anum;
		xx = xa-ticb*cos(angle);
		yy = ticb*sin(angle); 
		moveto(xa,0.0);  lineto(xx,yy);
		if (anum>-azero && anum<azero)
			sprintf(str,"%.4g",0.0);
		else
			sprintf(str,"%.4g",anum);
		xx = xa-numb*cos(angle)+labelCD*sin(angle);
		yy = numb*sin(angle)+labelCD*cos(angle);
		moveto(xx,yy);
		rotate(-yyy);
		justshow(-1.0,str);
		rotate(yyy);
	}
	stroke();
	dtic = dnum/ntic;
	for (atic=fnum-ntic*dtic-dtic; atic<=amax; atic+=dtic) {
		if (atic<amin) continue;
		xa = base+scale*atic;
		xx = xa - ticb/2.*cos(angle);
		yy = ticb/2. * sin(angle);
		moveto(xa,0.0);  lineto(xx,yy);
	}
	stroke();
	if (grid3==SOLID) {
		grided = 1;
		ndash = 0;
	} else if (grid3==DASH) {
		grided = 1;
		ndash = 1;  dash[0] = 10;
	} else if (grid3==DOT) {
		grided = 1;
		ndash = 2;  dash[0] = 1;  dash[1] = 5;
	} else
		grided = 0;
	if (grided) {
		for (anum=fnum; anum<=amax; anum+=dnum) {
			if (anum<amin) continue;
			xa = base+scale*anum;
			moveto(xa,0.0);  
			xa = xa + size2*cos(angle);
		        ya = -size2*sin(angle);
			lineto(xa,ya);
			xa = xa - size1*sin(angle);
			ya = ya - size1*cos(angle);
			lineto(xa,ya);
		}
		setdash(dash,ndash,0.0);
		stroke();
		setdash(dash,0,0.0);
	}
	xxx = size3/2.0;
	moveto(xxx,labelb);
	justshow(-0.5,label3);

	yyy = angle*180./3.141592654;
	rotate(-yyy);
	xxx = size1; 
	translate(0.,-xxx);

	/* draw title */
	setfont(titleFont,titleSize);
	xxx = (size2+size3*cos(angle))/2.;
	moveto(xxx,-0.5*titleCH-titleCA);
	justshow(-0.5,title);

	/* draw axes box */
	moveto(0.0,0.0);
	yyy=0.; xxx = size2;
	lineto(xxx,yyy);
	yyy=size1;
	lineto(xxx,yyy);
	xxx=0.;
	lineto(xxx,yyy);
	yyy=0.;
	lineto(xxx,yyy);
	xxx=0.;yyy=size1;
	moveto(xxx,yyy);
	xxx=xxx+size3*cos(angle);
	yyy=yyy+size3*sin(angle);
	lineto(xxx,yyy);
	xxx = xxx + size2;
	lineto(xxx,yyy);
	xxx = size2; yyy=size1;
	lineto(xxx,yyy);
	xxx = size2; yyy=0.;
	moveto(xxx,yyy);
	xxx=xxx+size3*cos(angle);
	yyy=yyy+size3*sin(angle);
	lineto(xxx,yyy);
	yyy=yyy+size1;
	lineto(xxx,yyy);
	stroke();

	/* restore graphics state */
	grestore();
}
