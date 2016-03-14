/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

#include "cwp.h"
#include "psplot.h"

void psAxesBox(
	float x, float y, float width, float height,
	float x1Beg, float x1End, float p1Beg, float p1End, 
	float d1Num, float f1Num, int n1Tic, int grid1, char *label1,
	float x2Beg, float x2End, float p2Beg, float p2End,
	float d2Num, float f2Num, int n2Tic, int grid2, char *label2,
	char *labelFont, float labelSize,
	char *title, char *titleFont, float titleSize,
	int style)
/*
FUNCTION:  draw an axes box via PostScript

PARAMETERS:
x		i x coordinate of lower left corner of box
y		i y coordinate of lower left corner of box
width		i width of box
height		i height of box
x1Beg  		i axis value at beginning of axis 1
x1End		i axis value at end of axis 1
p1Beg  		i pad value at beginning of axis 1
p1End		i pad value at end of axis 1
d1Num		i numbered tic increment for axis 1 (0.0 for automatic)
f1Num		i first numbered tic for axis 1
n1Tic		i number of horizontal tics per numbered tic for axis 1
grid1		i grid code for axis 1:  NONE, DOT, DASH, or SOLID
label1		i label for axis 1
x2Beg  		i axis value at beginning of axis 2
x2End		i axis value at end of axis 2
p2Beg  		i pad value at beginning of axis 2
p2End		i pad value at end of axis 2
d2Num		i vertical numbered tic increment (0.0 for automatic)
f2Num		i first numbered vertical tic
n2Tic		i number of vertical tics per numbered tic
grid2		i grid code for vertical axis:  NONE, DOT, DASH, or SOLID
label2		i vertical axis label
labelFont	i name of font to use for axes labels
labelSize	i size of font to use for axes labels
title		i axes box title
titleFont	i name of font to use for title
titleSize	i size of font to use for title
style		i NORMAL (axis 1 on bottom, axis 2 on left) 
		  SEISMIC (axis 1 on left, axis 2 on top)

NOTES:
psAxesBox will determine the numbered tic increment and first
numbered tic automatically, if the specified increment is zero.
Axis numbering is in scientific notation, if necessary and is plotted to four significant digits.

Pad values must be specified in the same units as the corresponding 
axes values.  These pads are useful when the contents of the axes box 
requires more space than implied by the axes values.  For example, 
the first and last seismic wiggle traces plotted inside an axes box 
will typically extend beyond the axes values corresponding to the 
first and last traces.  However, all tics will lie with the limits 
specified in the axes values (x1Beg, x1End, x2Beg, x2End).

AUTHOR:   Dave Hale,  Colorado School of Mines, 06/27/89
MODIFIED: Ken Larner, Colorado School of Mines, 08/30/90
*/
{
	int n1num,n2num,ntic,ndash,grided;
	int ndig,ndigits,nexp,nexpmax,nexplot1,nexplot2,
		nplaces,nplacesmax,nformat;
	float xa,xas,ya,yas,yb,xb,ticsize,dnum,fnum,dtic,amin,amax,azero,
		base,scale,anum,anumnorm,atic,fnexp,
		size1,size2,ticb,numb,numb2,labelb,dash[2],
		labelCH,labelCW,labelCA,labelCD,
		titleCH,titleCW,titleCA,titleCD,
		pnorm,fdexp,azeronorm;
	char str[256],str2[256],sformat[256];

	/* determine font dimensions */
	labelCH = fontheight(labelFont,labelSize);
	labelCW = fontwidth(labelFont,labelSize);
	labelCA = fontascender(labelFont,labelSize);
	labelCD = fontdescender(labelFont,labelSize);
	titleCH = fontheight(titleFont,titleSize);
	titleCW = fontwidth(titleFont,titleSize);
	titleCA = fontascender(titleFont,titleSize);
	titleCD = fontdescender(titleFont,titleSize);

	/* determine sizes of axes 1 and 2 */
	if (style==NORMAL) {
		size1 = width;
		size2 = height;
	} else {
		size1 = height;
		size2 = width;
	}

	/* determine numbered tic intervals */
	if (d1Num==0.0) {
		n1num = size1/(4*labelCW);
		scaxis(x1Beg,x1End,&n1num,&d1Num,&f1Num);
	}
	if (d2Num==0.0) {
		n2num = size2/(4*labelCW);
		scaxis(x2Beg,x2End,&n2num,&d2Num,&f2Num);
	}

	/* save graphics state */
	gsave();

	/* set gray (for black axes) */
	setgray(0.0);

	/* translate coordinate system, so that origin is at x,y */
	translate(x,y);

	/* if style is not NORMAL, rotate coordinate system */
	if (style!=NORMAL) {
		rotate(-90.0);
		translate(-height,0.0);
	}

	/* start a new path (just to be safe) */
	newpath();

	/* set font and character size */
	setfont(labelFont,labelSize);

	/* determine tic size */
	ticsize = 0.3*labelCH;

	/* draw axis 1 */
	amin = (x1Beg<x1End)?x1Beg:x1End;
	amax = (x1Beg>x1End)?x1Beg:x1End;
	azero = 0.0001*(amax-amin);
	dnum = d1Num;  fnum = f1Num;  ntic = n1Tic;
	scale = size1/(x1End+p1End-x1Beg-p1Beg);
	base = -scale*(x1Beg+p1Beg);
	ticb = -ticsize;
	numb = 1.2*ticb-labelCA;
	if(style!=NORMAL)   numb = 1.2*ticb;
	labelb = numb-labelCH;

	/* determine axes1 exponent for scientific notation   */
	ndigits = 0;
	ndig    = 0;
	fdexp = log10(1.001*ABS(dnum));
	if(fdexp<0.)   fdexp -=1.0;
	for (anum=fnum; anum<=amax; anum+=dnum) {
		if(anum==fnum && anum==0.)
			nexpmax = 0;
		nexp    = 0;
		if(anum!=0.)   {
			fnexp = log10(1.001*ABS(anum));
			if(fnexp>0.)   nexp    = (int)fnexp;
			else           nexp    = (int)fnexp-1;		
			if(anum==fnum)
				nexpmax = nexp;
			if(nexpmax<nexp || nexpmax==0)    
				nexpmax = nexp;
			ndig = 1+nexp-(int)fdexp;
		}
		if(ndigits<ndig)   ndigits = ndig;
	}
	nexplot1 = 0;
	if(ABS(nexpmax)>3)   
		nexplot1 = nexpmax;
	if((nexpmax<0) && (ndigits+ABS(nexpmax))>4)   
		nexplot1 = nexpmax;
	
	/* loop for axis1 numbering */
	nplacesmax = 0;
	pnorm      = pow(10.0,(double)nexplot1);
	azeronorm  = azero/pnorm;
	fdexp = log10(1.001*ABS(dnum/pnorm));
	if(fdexp<0.)   fdexp -=1.0;
	for (anum=fnum; anum<=amax; anum+=dnum) {

		if (anum<amin) continue;
		xa = base+scale*anum;
		xas = xa+0.25*labelCH;
		moveto(xa,0.0);  lineto(xa,ticb);
		anumnorm = anum/pnorm;

		/* find the number of places in axis1 numbers */
		nplaces = 1;
		nexp    = 1;
		nexp    = 0;
		if (anumnorm<-azeronorm || anumnorm>azeronorm)   {
			fnexp = log10(1.001*ABS(anumnorm));
			if(fnexp>0.)   {
				nexp    = (int)fnexp;
				nplaces = nexp+2;
			}
			else   {
				nexp    = (int)fnexp-1;
				nplaces = -nexp+2;
			}
		}
		/* numbers limited to four significant digits */
		ndigits = 1+nexp-(int)fdexp;
		if(ndigits>4)   ndigits = 4;

		if((nexp>=0) && ((ndigits+1)>nplaces))    
			nplaces = ndigits+1;
		if(anum<0. )   
			nplaces +=1;
		if(nexp<0)   
			nplaces = nplaces+ndigits-1;
		if(nplacesmax<nplaces)   
			nplacesmax = nplaces;

		nformat = ndigits-(nexp+1);
		if(nformat<0)   nformat = 0;

		if (anumnorm>-azeronorm && anumnorm<azeronorm)
			sprintf(str,"%.4g",0.0);
		else	{
			sprintf(sformat,"%%.%df",nformat);
			sprintf(str,sformat,anumnorm);
		}
	
		if (style!=NORMAL) {
			moveto(xas,numb);
			rotate(90.0);
			justshow(-1.0,str);
			rotate(-90.0);
		}
		else   {
			moveto(xa,numb);
			justshow(-0.5,str);
		}
			
	}
	/* draw exponential multiplier for axis1 */
	if(nexplot1!=0)  {
		if(style==NORMAL)   {
			moveto(size1,labelb);
			show("x10");
			moveto(size1+1.5*labelCW,labelb+0.5*labelCA);
			sprintf(str2,"%d",nexplot1);
			show(str2);
		}
		else   {
			moveto(size1+1.4*labelCH,-2.0*labelCW);
			rotate(90.);
			show("x10");
			rotate(-90.);
			moveto(size1+1.05*labelCH,-0.5*labelCW);
			sprintf(str2,"%d",nexplot1);
			rotate(90.);
			show(str2);
			rotate(-90.);
		}
	}
	stroke();

	/* draw axis1 tick marks */
	if(style!=NORMAL)   labelb = numb-0.5*nplacesmax*labelCW;
	dtic = dnum/ntic;
	for (atic=fnum-ntic*dtic-dtic; atic<=amax; atic+=dtic) {
		if (atic<amin) continue;
		xa = base+scale*atic;
		moveto(xa,0.0);  lineto(xa,ticb/2);
	}
	stroke();

	/* draw axis1 grid lines */
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
		}
		setdash(dash,ndash,0.0);
		stroke();
		setdash(dash,0,0.0);
	}
	
	/* draw axis1 label */
	moveto(size1/2.0,labelb);
	if(style!=NORMAL)   {
		rotate(180.);
		justshow(-0.5,label1);
		rotate(-180.);
	}
	else 	justshow(-0.5,label1);
 
	/* draw axis 2 */
	amin = (x2Beg<x2End)?x2Beg:x2End;
	amax = (x2Beg>x2End)?x2Beg:x2End;
	azero = 0.0001*(amax-amin);
	dnum = d2Num;  fnum = f2Num;  ntic = n2Tic;
	scale = size2/(x2End+p2End-x2Beg-p2Beg);
	base = -scale*(x2Beg+p2Beg);
	ticb = -ticsize;
	numb = ticb+labelCD;
	labelb = numb-labelCH;
	
	/* determine axes2 exponent for scientific notation   */
	ndigits = 0;
	ndig    = 0;
	fdexp   = log10(1.001*ABS(dnum));
	if(fdexp<0.)   fdexp -=1.0;
	for (anum=fnum; anum<=amax; anum+=dnum) {
		if(anum==fnum && anum==0.)
			nexpmax = 0;
		nexp    = 0;
		if(anum!=0.)   {
			fnexp = log10(1.001*ABS(anum));
			if(fnexp>0.)   nexp    = (int)fnexp;
			else           nexp    = (int)fnexp-1;		
			if(anum==fnum)
				nexpmax = nexp;
			if(nexpmax<nexp || nexpmax==0)    
				nexpmax = nexp;
			ndig = 1+nexp-(int)fdexp;
		}
		if(ndigits<ndig)   ndigits = ndig;
	}
	nexplot2 = 0;
	if(ABS(nexpmax)>3)   
		nexplot2 = nexpmax;
	if((nexpmax<0) && (ndigits+ABS(nexpmax))>4)   
		nexplot2 = nexpmax;
	
	/* loop for axis 2 numbering */
	nplacesmax = 0;
	pnorm      = pow(10.0,(double)nexplot2);
	azeronorm  = azero/pnorm;
	fdexp = log10(1.001*ABS(dnum/pnorm));
	if(fdexp<0.)   fdexp -=1.0;
	for (anum=fnum; anum<=amax; anum+=dnum) {
		ya = base+scale*anum;
		moveto(0.0,ya);  lineto(ticb,ya);
		anumnorm = anum/pnorm;
		yas = ya-0.25*labelCH;
	
		/* find the number of places in axis2 numbers */
		nplaces = 1;
		nexp    = 1;
		if (anumnorm<-azeronorm || anumnorm>azeronorm)   {
			fnexp = log10(1.001*ABS(anumnorm));
			if(fnexp>0.)   {
				nexp    = (int)fnexp;
				nplaces = nexp+2;
			}
			else   {
				nexp    = (int)fnexp-1;
				nplaces = -nexp+2;
			}
		}
		/* numbers limited to four significant digits */
		ndigits = 1+nexp-(int)fdexp;
		if(ndigits>4)   ndigits = 4;

		if((nexp>=0) && ((ndigits+1)>nplaces))    
			nplaces = ndigits+1;
		if(anum<0. )   
			nplaces +=1;
		if(nexp<0)   
			nplaces = nplaces+ndigits-1;
		if(nplacesmax<nplaces)   
			nplacesmax = nplaces;

		nformat = ndigits-(nexp+1);
		if(nformat<0)   nformat = 0;
	
		if (anumnorm>-azeronorm && anumnorm<azeronorm)
			sprintf(str,"%.4g",0.0);
		else	{
			sprintf(sformat,"%%.%df",nformat);
			sprintf(str,sformat,anumnorm);
		}

		if (style!=NORMAL) {
			moveto(numb,ya);
			rotate(90.0);
			justshow(-0.5,str);
			rotate(-90.0);
		}
		else   {
			moveto(numb,yas); 
			justshow(-1.,str);
		}
	}
	/* draw exponential mulitplier for axis2 */
	if(nexplot2!=0)  {
		if(style==NORMAL)   {
			numb2 = size2+.5*labelCH;
			moveto(labelb,numb2);
			show("x10");
			xb = labelb+1.5*labelCW;
			numb2 = numb2+0.5*labelCA;
			moveto(xb,numb2);
			sprintf(str2,"%d",nexplot2);
			show(str2);
		}
		else   {
			moveto(labelb,size2-.5*labelCW);
			rotate(90.);
			show("x10");
			rotate(-90.);
			moveto(labelb-.35*labelCH,size2+1.0*labelCW);
			sprintf(str2,"%d",nexplot2);
			rotate(90.);
			show(str2);
			rotate(-90.);
		}
	}
	stroke();

	/* draw axis2 tick marks */
	if(style==NORMAL)   labelb = numb-0.5*nplacesmax*labelCW;	
	dtic = dnum/ntic;
	for (atic=fnum-ntic*dtic-dtic; atic<=amax; atic+=dtic) {
		if (atic<amin) continue;
		ya = base+scale*atic;
		moveto(0.0,ya);  lineto(ticb/2,ya);
	}
	stroke();
	
	/* draw axis2 grid lines */
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
			moveto(0.0,ya);  lineto(size1,ya);
		}
		setdash(dash,ndash,0.0);
		stroke();
		setdash(dash,0,0.0);
	}
	
	/* draw axis2 label */
	moveto(labelb,size2/2.0);
	rotate(90.0);
	justshow(-0.5,label2);
	rotate(-90.0);

	/* draw title */
	setfont(titleFont,titleSize);
	if (style==NORMAL) {
		numb2 = size2+0.5*titleCH-titleCD;
		if(nexplot2!=0)	numb2 = numb2+.5*(labelCH+labelCA);
		moveto(size1/2.0,numb2);
		justshow(-0.5,title);
	} else {
		numb2 = size1+0.5*titleCH+titleCA;
		if(nexplot1!=0)	numb2 = numb2+labelCH;
		moveto(numb2,size2/2.0);
		rotate(90.0);
		justshow(-0.5,title);
		rotate(-90.0);
	}

	/* draw axes box */
	moveto(0.0,0.0);
	lineto(size1,0.0);
	lineto(size1,size2);
	lineto(0.0,size2);
	lineto(0.0,0.0);
	stroke();

	/* restore graphics state */
	grestore();
}

void psAxesBBox(
	float x, float y, float width, float height,
	char *labelFont, float labelSize,
	char *titleFont, float titleSize,
	int style, int bbox[])
/*
FUNCTION:  estimate bounding box for an axes box drawn via psAxesBox

PARAMETERS:
x			i x coordinate of lower left corner of box
y			i y coordinate of lower left corner of box
width		i width of box
height		i height of box
labelFont	i name of font to use for axes labels
labelSize	i size of font to use for axes labels
titleFont	i name of font to use for title
titleSize	i size of font to use for title
style		i NORMAL (axis 1 on bottom, axis 2 on left) 
			  SEISMIC (axis 1 on left, axis 2 on top)
bbox		o bounding box (bbox[0:3] = llx, lly, ulx, uly)

NOTES:
psAxesBBox uses font sizes to estimate the bounding box for
an axes box drawn with psAxesBox.  To be on the safe side, 
psAxesBBox overestimates.

psAxesBBox assumes that the axes labels and titles do not extend
beyond the corresponding edges of the axes box.

AUTHOR:   Dave Hale,  Colorado School of Mines, 06/27/89
MODIFIED: Ken Larner, Colorado School of Mines, 07/24/90
*/
{
	float labelCH,labelCW,titleCH;

	/* determine font dimensions */
	labelCH = fontheight(labelFont,labelSize);
	labelCW = fontwidth(labelFont,labelSize);
	titleCH = fontheight(titleFont,titleSize);

	/* determine bounding box */
	if (style==NORMAL) {
		bbox[0] = x-3.5*labelCW-labelCH;
		bbox[1] = y-3.0*labelCH;
		bbox[2] = x+width+3.0*labelCW;
		bbox[3] = y+height+2.0*titleCH+labelCH;
	} else {
		bbox[0] = x-3.5*labelCW-labelCH;
		bbox[1] = y-2.0*titleCH-labelCH;
		bbox[2] = x+width+3.0*labelCW;
		bbox[3] = y+height+3.0*labelCH;
	}
}
