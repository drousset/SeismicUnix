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
*    030227:  Fix for colors on 24 bit displays.  Intelligence provided by
*             Bob Herrmann.  Implemented by Mike Firpo (maf).
*    910318:  Added check for black and white monitor (ammon)
*    890608:  Modified to run under X11 rather than X10.  (kjm)
*    870316:  Changed call sequence and arrangement of color table.
*    870310:  Original Version
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd3.x11.h"


setctable3(win_num, nentry, red, green, blue)
int *win_num;
unsigned int nentry;
float red[], green[], blue[];
{ 
  Status status;
  int i, nent;
  int planes;
  int default_depth;
  unsigned long pixels[256];
  int full = 65535.0;
  unsigned long masks[256];
  XWindowAttributes xwinatt;
  Colormap colormap;

  XColor exact_def ;        /* 030227, maf */

  /*  check for depth (black and white = 1; color != 1)  */
  default_depth = DefaultDepth(display3,DefaultScreen(display3));
  if(default_depth != 1)
  {

    /* Scale red, green, blue to be in range [0, full]. */
    /* Colors are passed in with the first color representing the background */
    /* color and the last color representing the foreground color.  These    */
    /* need to be switched around to conform to X standards -- pixel value   */
    /* 0 representing foreground (black) and pixel value of 1 representing   */
    /* background (white).                                                   */

    colormap = DefaultColormap(display3,DefaultScreen(display3));
    pixdef3[nentry - 1].pixel = BlackPixel(display3,DefaultScreen(display3));
    pixdef3[nentry - 1].red = full * red[nentry - 1];
    pixdef3[nentry - 1].green = full * green[nentry - 1];
    pixdef3[nentry - 1].blue = full * blue[nentry - 1];
    pixdef3[nentry - 1].flags = DoRed | DoGreen | DoBlue;

    pixdef3[0].pixel = WhitePixel(display3,DefaultScreen(display3));
    pixdef3[0].red = full * red[0];
    pixdef3[0].green = full * green[0];
    pixdef3[0].blue = full * blue[0];
    pixdef3[0].flags = DoRed | DoGreen | DoBlue;

    for (i = 1; i< nentry-1; i++) {
      pixdef3[i].pixel = pixels[i - 1];
      pixdef3[i].red   = full * red[i];
      pixdef3[i].green = full * green[i];
      pixdef3[i].blue  = full * blue[i];
      pixdef3[i].flags = DoRed | DoGreen | DoBlue;
      exact_def.red   = pixdef3[ i ].red   ;
      exact_def.green = pixdef3[ i ].green ;
      exact_def.blue  = pixdef3[ i ].blue  ;
      XAllocColor( display3, colormap, &exact_def ) ;
      pixdef3[ i ].pixel = exact_def.pixel ;
      pixdef3[ i ].red   = exact_def.red   ;
      pixdef3[ i ].green = exact_def.green ;
      pixdef3[ i ].blue  = exact_def.blue  ;
    }

    /* Store color table */
    /* XStoreColors(display3,colormap,&pixdef3[1],nentry-2); */

    XFlush(display3);
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

