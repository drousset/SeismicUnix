/* Copyright (c) Colorado School of Mines, 1997.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
IMAGE - Function for making the image in an X-windows image plot

xNewImage	make a new image of pixels from bytes

******************************************************************************
Function Prototype:
XImage *xNewImage (Display *dpy, unsigned long pmin, unsigned long pmax,
	int width, int height, float blank, unsigned char *bytes);

******************************************************************************
Input:
dpy		display pointer
pmin		minimum pixel value (corresponding to byte=0)
pmax		maximum pixel value (corresponding to byte=255)
width		number of bytes in x dimension
height		number of bytes in y dimension
blank		portion for blanking (0 to 1)
bytes		unsigned bytes to be mapped to an image

******************************************************************************
Author:	  Dave Hale, Colorado School of Mines, 06/08/90
Revision: Brian Zook, Southwest Research Institute, 6/27/96 added blank option
	  This allows replacing the low end by the background.
*****************************************************************************/
/**************** end self doc ********************************/

#include "xplot.h"

XImage *xNewImage (Display *dpy, unsigned long pmin, unsigned long pmax,
	int width, int height, float blank, unsigned char *bytes)
/*****************************************************************************
make a new image of pixels from bytes
******************************************************************************
Input:
dpy		display pointer
pmin		minimum pixel value (corresponding to byte=0)
pmax		maximum pixel value (corresponding to byte=255)
width		number of bytes in x dimension
height		number of bytes in y dimension
blank		portion for blanking (0 to 1)
bytes		unsigned bytes to be mapped to an image
******************************************************************************
Author:		Dave Hale, Colorado School of Mines, 06/08/90
*****************************************************************************/
{
	int scr=DefaultScreen(dpy);
	int i,j,k,line,iline,jline,widthpad;
	float base,scale;
	unsigned char map[256],bkgnd;
	unsigned char *data;

	/* build map for translating bytes to pixels */
	base = pmin+0.499;
	scale = (pmax-pmin)/255.0;
	for (i=0; i<=255; ++i)
		map[i] = base+i*scale;

	/* blanking */
	bkgnd = WhitePixel(dpy,scr);
	j = MAX(0,MIN(256,(int)(256*blank)));
	for (i = 0; i < j; i++)
		map[255-i] = bkgnd;

	/* allocate memory for image data */
	widthpad = (1+(width-1)/(BitmapPad(dpy)/8))*BitmapPad(dpy)/8;
	data = ealloc1(widthpad*height,sizeof(unsigned char));

	/* translate bytes to pixels, padding scanlines as necessary */
	for (line=0; line<height; line++) {
		iline = line*width;
		jline = line*widthpad;
		for (i=iline,j=jline,k=0; k<width; ++i,++j,++k)
			data[j] = map[bytes[i]];
		for (j=jline+width,k=width; k<widthpad; ++j,++k)
			data[j] = data[jline+width-1];
	}
	
	/* create and return image structure */
	return XCreateImage(	(Display *) dpy,
				(Visual *) DefaultVisual(dpy,scr),
				(unsigned int) DefaultDepth(dpy,scr),
				(int) ZPixmap,
				(int) 0,
				(char *) data,
				(unsigned int) widthpad,
				(unsigned int) height,
				(int) BitmapPad(dpy),
				(int) widthpad);
}
