#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/keysym.h>
#include <X11/Xresource.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "config.h"
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/gtm.h"
#include "../../inc/contouring.h"
#include "../../inc/gd3.x11.h"
#include "../../inc/gdm.h"


void put_image3(data, xloc, yloc, width, height, nerr)
char *data;
unsigned int xloc, yloc;
unsigned int width, height;
long int *nerr;
{

   XImage *image;
   static int screen;


	/*=====================================================================
	 * PURPOSE:
	 *         
         *         
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    
	 *    
	 *    
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag.  Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:
	 *=====================================================================
	 * GLOBAL INPUT:
	 *  
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *  
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

        screen = DefaultScreen(display3);

        /* create the image structure. */

#ifdef USE_X11_MULTIPLE_DEPTHS
	/* 
	   Bitmap Pad     Depth
	   8 bits         8bpp
	   16 bits        16bpp
	   32 bits        24bpp
	   32 bits        32bpp
	*/
	{
	  int depth;
	  int bitmap_pad;
	  Visual *v;
	  v = DefaultVisual(display3, DefaultScreen(display3));
	  depth = DefaultDepth(display3, DefaultScreen(display3));

	  if(depth > 16) 
	    bitmap_pad = 32;
	  else if(depth > 8)
	    bitmap_pad = 16;
	  else
	    bitmap_pad = 8;

	  image =  XCreateImage(display3, v, depth, ZPixmap, 0, data, 
				width, height, 
				bitmap_pad, 0);
	}

	XPutImage(display3, plotw3_pixmap, plotw3[cmgdm.iwindow].gc,
		  image, 0, 0, xloc, yloc, width, height);
#else /* USE_X11_MULTIPLE_DEPTHS */
        image =  XCreateImage(display3,DefaultVisual(display3,screen),
                              8, ZPixmap, 0, NULL, width,
                              height, 8, 0);
        image->data = data;

        XPutImage(display3, plotw3[cmgdm.iwindow].win, plotw3[cmgdm.iwindow].gc,
                  image, 0, 0, xloc, yloc, width, height);
#endif /* USE_X11_MULTIPLE_DEPTHS */

        XFlush(display3);

        /* destroy the image structure */
        XDestroyImage(image);

L_8888:

	return;

} /* end of function */






