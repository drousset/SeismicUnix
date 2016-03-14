/* Copyright (c) Colorado School of Mines, 1997.*/
/* All rights reserved.                       */

/* LEGENDBOX: $Revision: 1.0 $ ; $Date: 2004/05/27 20:26:05 $	*/

#include "xplot.h"
#include "par.h"

#define EPS .001

/*********************** self documentation **********************/
/*****************************************************************************
LEGENDBOX - draw a labeled axes box for a legend (i.e. colorscale)

******************************************************************************
Function Prototype:
void xDrawLegendBox (Display *dpy, Window win,
	int x, int y, int width, int height,
	float bclip, float wclip, char *units, char *legendfont,
	char *labelfont, char *title, char *titlefont,
	char *axescolor, char *titlecolor, char *gridcolor,
	int style);

******************************************************************************
Input:
dpy		display pointer
win		window
x		x coordinate of upper left corner of box
y		y coordinate of upper left corner of box
width		width of box
height		height of box
units		label for legend
legendfont	name of font to use for legend labels
labelfont	name of font to use for axes labels
title		axes box title
titlefont	name of font to use for title
axescolor	name of color to use for axes
titlecolor	name of color to use for title
gridcolor	name of color to use for grid
int style	NORMAL (axis 1 on bottom, axis 2 on left)
		SEISMIC (axis 1 on left, axis 2 on top)
******************************************************************************
Notes:
xDrawLegendBox will determine the numbered tic incremenet and first
numbered tic automatically, if the specified increment is zero.

Pad values must be specified in the same units as the corresponding
axes values.  These pads are useful when the contents of the axes box
requires more space than implied by the axes values.  For example,
the first and last seismic wiggle traces plotted inside an axes box
will typically extend beyond the axes values corresponding to the
first and last traces.  However, all tics will lie within the limits
specified in the axes values (x1beg, x1end, x2beg, x2end).
******************************************************************************
Author:		Dave Hale, Colorado School of Mines, 01/27/90
Author:		Berend Scheffers , TNO Delft, 06/11/92
*****************************************************************************/
/**************** end self doc ********************************/

void
xDrawLegendBox (Display *dpy, Window win,
	int x, int y, int width, int height,
	float bclip, float wclip, char *units, char *legendfont,
	char *labelfont, char *title, char *titlefont,
	char *axescolor, char *titlecolor, char *gridcolor,
	int style)
/*****************************************************************************
draw a labeled axes box
******************************************************************************
Input:
dpy		display pointer
win		window
x		x coordinate of upper left corner of box
y		y coordinate of upper left corner of box
width		width of box
height		height of box
units		label for legend
legendfont	name of font to use for legend labels
labelfont	name of font to use for axes labels
title		axes box title
titlefont	name of font to use for title
axescolor	name of color to use for axes
titlecolor	name of color to use for title
gridcolor	name of color to use for grid
int style	NORMAL (axis 1 on bottom, axis 2 on left)
		SEISMIC (axis 1 on left, axis 2 on top)
******************************************************************************
Notes:
xDrawLegendBox will determine the numbered tic incremenet and first
numbered tic automatically, if the specified increment is zero.

Pad values must be specified in the same units as the corresponding
axes values.  These pads are useful when the contents of the axes box
requires more space than implied by the axes values.  For example,
the first and last seismic wiggle traces plotted inside an axes box
will typically extend beyond the axes values corresponding to the
first and last traces.  However, all tics will lie within the limits
specified in the axes values (x1beg, x1end, x2beg, x2end).
******************************************************************************
Author:		Dave Hale, Colorado School of Mines, 01/27/90
Author:		Berend Scheffers , TNO Delft, 06/11/92
*****************************************************************************/
{
	GC gca,gct,gcg;
	XGCValues *values=NULL;
	XColor scolor,ecolor;
	XFontStruct *fa,*ft;
	XWindowAttributes wa;
	Colormap cmap;
	int labelca,labelcd,labelch,labelcw,titleca,titlecd,titlech,titlecw,
		xa,ya,tw,ticsize,ticb,numb,lstr,scr;
	float dnum=EPS,amin,amax,base,anum,azero;
	char str[256];

	/* get screen */
	scr = DefaultScreen(dpy);

	/* create graphics contexts */
	gca = XCreateGC(dpy,win,0,values);
	gct = XCreateGC(dpy,win,0,values);
	gcg = XCreateGC(dpy,win,0,values);

	/* get and set fonts and determine character dimensions */
	fa = XLoadQueryFont(dpy,legendfont);
	if (fa==NULL) fa = XLoadQueryFont(dpy,"fixed");
	if (fa==NULL) {
		fprintf(stderr,"Cannot load/query legendfont=%s\n",legendfont);
		exit(-1);
	}
	XSetFont(dpy,gca,fa->fid);
	labelca = fa->max_bounds.ascent;
	labelcd = fa->max_bounds.descent;
	labelch = fa->max_bounds.ascent+fa->max_bounds.descent;
	labelcw = fa->max_bounds.lbearing+fa->max_bounds.rbearing;
	ft = XLoadQueryFont(dpy,titlefont);
	if (ft==NULL) ft = XLoadQueryFont(dpy,"fixed");
	if (ft==NULL) {
		fprintf(stderr,"Cannot load/query titlefont=%s\n",titlefont);
		exit(-1);
	}
	XSetFont(dpy,gct,ft->fid);
	titleca = ft->max_bounds.ascent;
	titlecd = ft->max_bounds.descent;
	titlech = ft->max_bounds.ascent+ft->max_bounds.descent;
	titlecw = ft->max_bounds.lbearing+ft->max_bounds.rbearing;

	/* determine window's current colormap */
	XGetWindowAttributes(dpy,win,&wa);
	cmap = wa.colormap;

	/* get and set colors */
	if (XAllocNamedColor(dpy,cmap,axescolor,&scolor,&ecolor))
		XSetForeground(dpy,gca,ecolor.pixel);
	else
		XSetForeground(dpy,gca,1L);
	if (XAllocNamedColor(dpy,cmap,titlecolor,&scolor,&ecolor))
		XSetForeground(dpy,gct,ecolor.pixel);
	else
		XSetForeground(dpy,gct,1L);
	if (XAllocNamedColor(dpy,cmap,gridcolor,&scolor,&ecolor))
		XSetForeground(dpy,gcg,ecolor.pixel);
	else
		XSetForeground(dpy,gcg,1L);

	/* determine tic size */
	ticsize = labelcw;

	/* draw vertical axis */
	amin = wclip;
	amax = bclip;

	/* trap for cases when bclip=wclip */
	if ((bclip - wclip)!=0) dnum = (bclip-wclip)/4;

	base = y;

	ticb = -ticsize;
	numb = ticb-ticsize/4;

	azero = 0.0001*(amax-amin);

	xa = x+1.25*width;
	ya = y;
	for (anum=amax; anum>=amin; anum-=dnum) {
		if (anum<amin) continue;
		XDrawLine(dpy,win,gca,x,ya,x+width,ya);
		XDrawLine(dpy,win,gca,x+width,ya,xa,ya);
		sprintf(str,"%1.4g",anum);
		lstr = strlen(str);
		tw = XTextWidth(fa,str,lstr);
		XDrawString(dpy,win,gca,xa+width/4,ya+labelca/2,str,lstr);
		ya += height/4;
	}

	xa = x+width/8;
	ya = y+height+labelch;
	sprintf(str,units);
	lstr = strlen(str);
	tw = XTextWidth(fa,str,lstr);
	XDrawString(dpy,win,gca,xa,ya,str,lstr);


	/* draw axes box */
	XDrawRectangle(dpy,win,gca,x,y,width,height);

	/* free resources before returning */
	XFreeGC(dpy,gca);
	XFreeGC(dpy,gct);
	XFreeGC(dpy,gcg);
}



