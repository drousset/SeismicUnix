/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*****************************************************************************
Functions to support floating point coordinates in X
******************************************************************************
Notes:
The functions defined below are designed to resemble the equivalent 
X functions.  For example, FXDrawLine() is analogous to XDrawLine.
Each of the FXDraw<xxx>() functions requires an FGC instead of a GC
(graphics context).  An FGC contains a GC, along with the information 
required to transform floating point coordinates to integer (pixel) 
coordinates.

Additional functions are provided to transform floating point coordinates
to integer coordinates and vice versa.  Where feasible, macros are also
provided to perform these coordinate transformations.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/24/90
******************************************************************************/
#include "Xcwp/Xcwp.h"

#define NPBUF 512	/* size of temporary arrays of points */

/* map float x to x */
int FMapFX(FGC fgc, float fx)
{
	return(fgc->xshift+fx*fgc->xscale);
}

/* map float y to y */
int FMapFY(FGC fgc, float fy)
{
	return(fgc->yshift+fy*fgc->yscale);
}

/* map float width to width */
int FMapFWidth(FGC fgc, float fwidth)
{
	return(fwidth*fgc->xscale);
}

/* map float height to height */
int FMapFHeight(FGC fgc, float fheight)
{
	return(fheight*fgc->yscale);
}

/* map float angle to angle */
int FMapFAngle(FGC fgc, float fangle)
{
	return(fangle*64.0);
}

/* map float x,y to x,y */
void FMapFPoint(FGC fgc, float fx, float fy, int *x_return, int *y_return)
{
	*x_return = MapFX(fgc,fx);
	*y_return = MapFY(fgc,fy);
}

/* map float points to points */
void FMapFPoints(FGC fgc, FXPoint fpoints[], int npoints, 
	XPoint points_return[])
{
	int i,ix,iy;
	for (i=0; i<npoints; i++) {
		points_return[i].x = MapFX(fgc,fpoints[i].fx);
		points_return[i].y = MapFY(fgc,fpoints[i].fy);
	}
}

/* inverse map x to float x */
float FMapX(FGC fgc, int x)
{
	return((x-fgc->xshift)/fgc->xscale);
}

/* inverse map y to float y */
float FMapY(FGC fgc, int y)
{
	return((y-fgc->yshift)/fgc->yscale);
}

/* inverse map width to float width */
float FMapWidth(FGC fgc, int width)
{
	return(width/fgc->xscale);
}

/* inverse map height to float height */
float FMapHeight(FGC fgc, int height)
{
	return(height/fgc->yscale);
}

/* inverse map angle to float angle */
float FMapAngle(FGC fgc, int angle)
{
	return(angle/64.0);
}

/* map x,y to float x,y */
void FMapPoint(FGC fgc, int x, int y, float *fx_return, float *fy_return)
{
	*fx_return = MapX(fgc,x);
	*fy_return = MapY(fgc,y);
}

/* map points to float points */
void FMapPoints(FGC fgc, XPoint points[], int npoints, 
	FXPoint fpoints_return[])
{
	int i;
	for (i=0; i<npoints; i++) {
		fpoints_return[i].fx = MapX(fgc,points[i].x);
		fpoints_return[i].fy = MapY(fgc,points[i].y);
	}
}

/* set graphics context */
void FSetGC(FGC fgc, GC gc)
{
	fgc->gc = gc;
}

/* set map (scales and shifts) */
void FSetMap(FGC fgc, int x, int y, int width, int height,
	float fx, float fy, float fwidth, float fheight)
{
	fgc->xscale = width/fwidth;
	fgc->yscale = height/fheight;
	fgc->xshift = x-fx*fgc->xscale;
	fgc->yshift = y-fy*fgc->yscale;
}

/* create float graphics context */
FGC FXCreateFGC(GC gc, int x, int y, int width, int height,
	float fx, float fy, float fwidth, float fheight)
{
	FGC fgc;
	fgc = (FGC)malloc(sizeof(*fgc));
	FSetGC(fgc,gc);
	FSetMap(fgc,x,y,width,height,fx,fy,fwidth,fheight);
	return(fgc);
}

/* free float graphic context */
void FXFreeFGC(FGC fgc)
{
	free(fgc);
}

/* draw point at float x,y */
void FXDrawPoint(Display *display, Drawable d, FGC fgc, float fx, float fy)
{
	XDrawPoint(display,d,fgc->gc,MapFX(fgc,fx),MapFY(fgc,fy));
}

/* draw float points */
void FXDrawPoints(Display *display, Drawable d, FGC fgc, 
	FXPoint fpoints[], int npoints, int mode)
{      
	XPoint *points,pbuf[NPBUF];
	if (npoints>NPBUF)
		points = (XPoint *)malloc(npoints*sizeof(*points));
	else
		points = pbuf;
	FMapFPoints(fgc,fpoints,npoints,points);
	XDrawPoints(display,d,fgc->gc,points,npoints,mode);
	if (npoints>NPBUF) free(points);
}

/* draw line from float x1,y1 to float x2,y2 */
void FXDrawLine(Display *display, Drawable d, FGC fgc,
	float fx1, float fy1, float fx2, float fy2)
{
	XDrawLine(display,d,fgc->gc,
		MapFX(fgc,fx1),MapFY(fgc,fy1),
		MapFX(fgc,fx2),MapFY(fgc,fy2));
}

/* draw lines between float points */
void FXDrawLines(Display *display, Drawable d, FGC fgc,
	FXPoint fpoints[], int npoints, int mode)
{
	XPoint *points,pbuf[NPBUF];
	if (npoints>NPBUF)
		points = (XPoint *)malloc(npoints*sizeof(*points));
	else
		points = pbuf;
	FMapFPoints(fgc,fpoints,npoints,points);
	XDrawLines(display,d,fgc->gc,points,npoints,mode);
	if (npoints>NPBUF) free(points);
}

/* draw rectangle with float x,y,width,height */
void FXDrawRectangle(Display *display, Drawable d, FGC fgc, 
	float fx, float fy, float fwidth, float fheight)
{          
	XDrawRectangle(display,d,fgc->gc,
		MapFX(fgc,fx),MapFY(fgc,fy),
		MapFWidth(fgc,fwidth),MapFHeight(fgc,fheight));
}

/* draw arc with float x,y,width,height,angle1,angle2 */
void FXDrawArc(Display *display, Drawable d, FGC fgc,
	float fx, float fy, float fwidth, float fheight, 
	float fangle1, float fangle2)
{          
	XDrawArc(display,d,fgc->gc,
		MapFX(fgc,fx),MapFY(fgc,fy),
		MapFWidth(fgc,fwidth),MapFHeight(fgc,fheight),
		MapFAngle(fgc,fangle1),MapFAngle(fgc,fangle2));
}

/* draw string at float x,y */
void FXDrawString(Display *display, Drawable d, FGC fgc, 
	float fx, float fy, char *string, int length)
{          
	XDrawString(display,d,fgc->gc,MapFX(fgc,fx),MapFY(fgc,fy),
		string,length);
}
