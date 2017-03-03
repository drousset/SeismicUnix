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
#include "../../inc/gd3.x11.h"
#include "../../inc/gdm.h"


char *fill_clrbar3(npseudocolors, width, npricolors, ndefcolors, nerr)

long npseudocolors;   /* number of pseudocolors in the colortable */
long width;       /* number of elements in one scan line of the color bar */
long npricolors;  /* number of SAC primary colors */
long ndefcolors;  /* number of default colors in the colortable */
long int *nerr;
{

unsigned int k;
long ibyte, i, j;
char *array;

#ifdef USE_X11_MULTIPLE_DEPTHS
  int depth;
  int pixel_size;
  unsigned short *array2;
  unsigned int *array4;
  
/*   Type            Pixel Size  Depth
  unsigned char  * => 1 byte =>  8bpp
  unsigned short * => 2 byte => 16bpp
  unsigned int *   => 4 byte => 24bpp
  unsigned int *   => 4 byte => 32bpp
*/
  depth = DefaultDepth(display3, DefaultScreen(display3));
  if(depth > 16) 
    pixel_size = 4;
  else if(depth > 8)
    pixel_size = 2;
  else 
    pixel_size = 1;
#endif

/*  Color bar  fill an entire scan line with one color */

  *nerr = 0;

/* allocate memory for colorbar byte array */
#ifndef USE_X11_MULTIPLE_DEPTHS
  if((array = (char *)malloc(width*npseudocolors*sizeof(char)))
                    == NULL){
#else  
  array = (char *)malloc(width * npseudocolors * pixel_size);
  array2 = (unsigned short *) array;
  array4 = (unsigned int *)   array;

  if(array == NULL) {
#endif
    printf("error allocating colorbar byte array--fill_clrbar3\n");
    *nerr = 301;
    goto L_8888;
  };

  k = npricolors+npseudocolors+ndefcolors;
  ibyte = 0;
  for (i=0; i< npseudocolors; i++){
     for (j=0; j< width; j++){
#ifdef USE_X11_MULTIPLE_DEPTHS
      if(depth > 16) 
	array4[ibyte++ ] = pixdef3[k].pixel;  
      else if(depth > 8) 
	array2[ibyte++ ] = pixdef3[k].pixel;  	 
      else 
#endif
       array[ibyte++ ] = pixdef3[k].pixel;  
     }
     k--;
  }

L_8888:
	return array;

} /* end of function */






