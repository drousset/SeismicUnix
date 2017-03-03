/*******************************************************************************
** PURPOSE:
*    To set the color table.
*
** INPUT ARGUMENTS:
*    win_num:      Window number to set table for. (Pointer)
*    ncolors:      Number of entries in color table. (Pointer)
*    red:          Array containing intensities of reds.  Range:  [0.0,1.0]
*    green:        Array containing intensities of greens.  Range: [0.0,1.0]
*    blue:         Array containing intensities of blues.  Range: [0.0,1.0]
*
** GLOBAL OUTPUT:
*    gd3.x11.h:  pixdef->(pixel, red, green, blue)
*
** SUBROUTINES CALLED:
*    XGetWindowAttributes,XAllocColorCells,XStoreColors
*
** LOCAL VARIABLES:
*    Status:  Status of color cells.
*    i:       Loop counter.
*    planes:  Plane mask.
*    pixels:  Pixel values.
*    full:    'Fullest color' -- Colors are in range [0, full].
*
** LIMITATIONS:
*    - Maximum of 256 colors.
*******************************************************************************
** MODIFICATION HISTORY:
*    910318:  Added check for black and white monitor (ammon)
*    890608:  Modified to run under X11 rather than X10.  (kjm)
*    870316:  Changed call sequence and arrangement of color table.
*    870310:  Original Version
*******************************************************************************/

#include <stdio.h>
#include <X11/Xlib.h>
#include "config.h"
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/gd3.x11.h"
#include "../../inc/gdm.h"
#define DOINITS
#include "../../inc/color.h"
#undef DOINITS

setpsctable3(win_num, nentry, red, green, blue)
  int *win_num;
  unsigned int nentry;
  float red[], green[], blue[];
{ 
  Status status;
  int i, nent;
  int planes;
  int default_depth;
  unsigned long pixels[256];
  int full = 65535;
  unsigned long masks[256];
  XWindowAttributes xwinatt;
  XSetWindowAttributes winatt;
  unsigned long valuemask;
  Visual *v;
  XColor colorcell[1];
  XColor pixdeftest[256];
  unsigned int nalloc;
  long maxcolormaps,mincolormaps;
  Screen *screenptr;


/*  check for depth (black and white = 1; color != 1)  */
  default_depth = DefaultDepth(display3,DefaultScreen(display3));
  if(default_depth != 1)
  {

/* Scale red, green, blue to be in range [0, full]. */
/** Colors are passed in with the first color representing the background   **/
/** color and the last color representing the foreground color.  These      **/
/** need to be switched around to conform to X standards -- pixel value     **/
/** 0 representing foreground (black) and pixel value of 1 representing     **/
/** background (white).                                                     **/

  npscolors = cmgdm.npscimage;

  nalloc = nentry + npscolors + 7;

  v = DefaultVisual(display3,DefaultScreen(display3));
  
#ifdef USE_X11_MULTIPLE_DEPTHS

  {

    colormap = DefaultColormap(display3, DefaultScreen(display3));

    for(i = 0; i<7; i++) {
      pixdef3[i].pixel = i;
      pixdef3[i].flags = DoRed | DoGreen | DoBlue;
    }
    
    /* Foreground -- White */
    pixdef3[nentry - 1 + 7].red = full * red[nentry-1];
    pixdef3[nentry - 1 + 7].green = full * green[nentry-1];
    pixdef3[nentry - 1 + 7].blue = full * blue[nentry-1];
    pixdef3[nentry - 1 + 7].flags = DoRed | DoGreen | DoBlue;

    /* Background -- Black */
    pixdef3[7].red = full * red[0];
    pixdef3[7].green = full * green[0];
    pixdef3[7].blue = full * blue[0];
    pixdef3[7].flags = DoRed | DoGreen | DoBlue;
    
    for (i = 8; i< nentry-1+7; i++) {
      pixdef3[i].red = full * red[i-7];
      pixdef3[i].green = full * green[i-7];
      pixdef3[i].blue = full * blue[i-7];
      pixdef3[i].flags = DoRed | DoGreen | DoBlue;
    }
    for (i = nentry+7; i < nalloc; i++){
      pixdef3[i].red = psred[i-(nentry+7)];
      pixdef3[i].green = psgreen[i-(nentry+7)];
      pixdef3[i].blue = psblue[i-(nentry+7)];
      pixdef3[i].flags = DoRed | DoGreen | DoBlue;
    }
    for ( i = 0; i <= nalloc; i++) {
      if(XAllocColor(display3, colormap, &(pixdef3[i])) == 0) {
	fprintf(stderr, "XAllocColor: allocation of (%d %d %d) color failed %d\n",
		pixdef3[i].red, pixdef3[i].green, pixdef3[i].blue, i);
      }
    }
  }
#else /* USE_X11_MULTPLE_DEPTHS */
  colormap = XCreateColormap(display3,basew3[*win_num].win,v,AllocNone);

  status = XAllocColorCells(display3, colormap,True,masks,0, pixels,nalloc);
  if (status != 0) {
    colorcell->flags = DoRed | DoGreen | DoBlue;
    for (i=0; i<7; i++){
      colorcell->pixel = i;
      XQueryColor(display3,DefaultColormap(display3,DefaultScreen(display3)),colorcell);
      pixdef3[i].pixel = pixels[i+2];
      pixdef3[i].red = colorcell->red;
      pixdef3[i].green = colorcell->green;
      pixdef3[i].blue = colorcell->blue;
      pixdef3[i].flags = DoRed | DoGreen | DoBlue;
      
    }

    pixdef3[nentry - 1 + 7].pixel = pixels[1];
    pixdef3[nentry - 1 + 7].red = full * red[nentry-1];
    pixdef3[nentry - 1 + 7].green = full * green[nentry-1];
    pixdef3[nentry - 1 + 7].blue = full * blue[nentry-1];
    pixdef3[nentry - 1 + 7].flags = DoRed | DoGreen | DoBlue;

    pixdef3[7].pixel = pixels[0];
    pixdef3[7].red = full * red[0];
    pixdef3[7].green = full * green[0];
    pixdef3[7].blue = full * blue[0];
    pixdef3[7].flags = DoRed | DoGreen | DoBlue;

    for (i = 8; i< nentry-1+7; i++) {
      pixdef3[i].pixel = pixels[i+1];
      pixdef3[i].red = full * red[i-7];
      pixdef3[i].green = full * green[i-7];
      pixdef3[i].blue = full * blue[i-7];
      pixdef3[i].flags = DoRed | DoGreen | DoBlue;
    }

    for (i = nentry+7; i < nalloc; i++){
      pixdef3[i].pixel = pixels[i];
      pixdef3[i].red = psred[i-(nentry+7)];
      pixdef3[i].green = psgreen[i-(nentry+7)];
      pixdef3[i].blue = psblue[i-(nentry+7)];
      pixdef3[i].flags = DoRed | DoGreen | DoBlue;
    }
/* Store color table */

    XStoreColors(display3,colormap,pixdef3,nalloc);
    
    XSetWindowColormap(display3,basew3[*win_num].win,colormap); 

    XFlush(display3);

  } /* if(status != 0) */
#endif /* USE_X11_MULTPLE_DEPTHS */
  }
  else
  {
  /*  black and white */
    pixdef3[0].pixel = WhitePixel(display3,DefaultScreen(display3));
    nent = nentry - 1;
    for (i = 1; i< nent; i++) 
      pixdef3[i].pixel = BlackPixel(display3,DefaultScreen(display3));
  }
}

