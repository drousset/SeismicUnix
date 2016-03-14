/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION:  rasterize a float array as wiggle-trace-variable-area

PARAMETERS:
n				i	number of samples in array to rasterize
z				i	array to rasterize
zmin			i	z values below zmin will be clipped
zmax			i	z values above zmax will be clipped
zbase			i	z values between zbase and zmax will be filled (see notes)
yzmin			i	horizontal raster coordinate corresponding to zmin
yzmax			i	horizontal raster coordinate corresponding to zmax
xfirst			i	vertical raster coordinate of z[0] (see notes)
xlast			i	vertical raster coordinate of z[n-1] (see notes)
wiggle			i	=0 for no wiggle (VA only); =1 for wiggle (with VA)
nbpr			i	number of bytes per row of bits
bits			b	pointer to first (top,left) byte in image

NOTES:
The raster coordinate of the (top,left) bit in the image is (0,0).
In other words, x increases downward and y increases to the right.
Raster scan lines run from left to right, and from top to bottom.
Therefore, xfirst, xlast, yzmin, and yzmax should not be less than 0.
Likewise, yzmin and yzmax should not be greater than nbpr*8-1, and 
care should be taken to ensure that xfirst and xlast do not cause bits 
to be set outside (off the bottom) of the image. 

Variable area fill is performed on the right-hand (increasing y) side
of the wiggle.  If yzmin is greater than yzmax, then z values between
zmin will be plotted to the right of zmax, and z values between zbase
and zmin are filled.  Swapping yzmin and yzmax is an easy way to 
reverse the polarity of a wiggle.

AUTHOR:  Dave Hale, Colorado School of Mines, 07/01/89
*/
void rfwtva (
	int n, float z[], float zmin, float zmax, float zbase,
	int yzmin, int yzmax, int xfirst, int xlast,
	int wiggle, int nbpr, unsigned char *bits)
{
	int iscale,xscale,dx,dy,i,x,y,
		ymin,ymax,ybase,ythis,ynext,xthis,xnext,xstep;
	float yscale,yoffset,zthis,znext,dz;
	register int bit;
	register unsigned char *byte;
	
	/* determine min and max y coordinates */
	ymin = (yzmin<yzmax)?yzmin:yzmax;
	ymax = (yzmax>yzmin)?yzmax:yzmin;

	/* restrict min and max y coordinates */
	ymin = (ymin>0)?ymin:0;
	ymax = (ymax<nbpr*8-1)?ymax:nbpr*8-1;
	
	/* determine sample index scale factor */
	iscale = n-1;
	
	/* determine y scale factor and offset */
	yscale = (zmax>zmin)?(yzmax-yzmin)/(zmax-zmin):0.0;
	yoffset = yzmin-zmin*yscale;
	
	/* determine x scale factor and step */
	xscale = (n>1)?xlast-xfirst:0;
	xstep = (xlast>xfirst)?1:-1;
	
	/* determine base y coordinate */
	ybase = yoffset+zbase*yscale;
	ybase = (ybase>ymin)?ybase:ymin;
	ybase = (ybase<ymax)?ybase:ymax;
	
	/* initialize next values of x, y, and z */
	znext = *z;
	ynext = yoffset+znext*yscale;
	xnext = xfirst;
	
	/* loop over samples */
	for (i=0; i<n; i++,z++) {
		
		/* determine x coordinate for this sample */
		xthis = xnext;
		
		/* determine x coordinate for next sample */
		xnext = (i<iscale)?xfirst+(i+1)*xscale/iscale:xthis+xstep;

		/* skip sample if next sample falls on same x coordinate */
		if (xnext==xthis) continue;
		
		/* determine difference in x coordinates */
		dx = xnext-xthis;
		
		/* determine this sample value */
		zthis = znext;
		
		/* determine next sample value */
		znext = (i<n-1)?*(z+1):zthis;
		
		/* determine difference in sample values */
		dz = znext-zthis;
		
		/* determine y coordinate for this sample */
		ythis = ynext;
		
		/* determine y coordinate for next sample */
		ynext = yoffset+znext*yscale;
		
		/* determine difference in y coordinates */
		dy = ynext-ythis;
		
		/* loop over x coordinates */
		for (x=xthis,y=ythis; x!=xnext; x+=xstep,y=ythis+(x-xthis)*dy/dx) {
			
			/* apply clip */
			if (y<ymin) y = ymin;
			if (y>ymax) y = ymax;
			
			/* determine the bit and byte */
			bit = 7-y&7;
			byte = bits+x*nbpr+(y>>3);

			/* if wiggle or filling, then set the bit */
			if (wiggle || y>ybase)
				*byte |= 1<<bit;
			
			/* while y greater than base, set more bits */
			while (y>ybase) {
				y--;
				bit++;
				if (bit==8) {
					byte--;
					bit = 0;
				}
				*byte |= 1<<bit;
			}
		}
	}
}
