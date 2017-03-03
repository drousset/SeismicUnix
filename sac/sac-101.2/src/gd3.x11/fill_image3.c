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


char *fill_image3(height, width, data, dmin, range, npseudocolors, nsaccolors, ndefcolors, nerr)

unsigned int height; 
unsigned int width;  
float data[]; 
float dmin, range;
long npseudocolors;
long nsaccolors;
long ndefcolors;
long int *nerr;
{

  long i, j;
  float fsave,fmin,fmax;
  long isave;
  char *array;
  float wrange;

#ifdef USE_X11_MULTIPLE_DEPTHS
  unsigned short *array2;
  unsigned int *array4;
  int depth;
  int pixel_size;

  depth = DefaultDepth(display3, DefaultScreen(display3));
  if(depth > 16) 
    pixel_size = 4;
  else if(depth > 8)
    pixel_size = 2;
  else 
    pixel_size = 1;
  
#endif

  wrange = range == 0.0 ? RNDOFF: range;

#ifndef USE_X11_MULTIPLE_DEPTHS
  if((array  = (char *)malloc(width*height*sizeof(char)))
                 == NULL){
#else
  array = (char *) malloc(width * height * pixel_size);
 
  array4 = (unsigned int *)   array;
  array2 = (unsigned short *) array;

  if(array == NULL) {
#endif
    printf("error allocating image data byte array--fill_image3\n");
    *nerr = 0301;
    goto L_8888;
  }

/*  Standard algorithm to scale data to (cmgdm.nctsize-(cmgdm.nctsize+npscolors-1)) range */
  
        *nerr = 0;

        fmin = (float)nsaccolors+1.0+(float)ndefcolors;
        fmax = (float)(nsaccolors+npseudocolors+ndefcolors);
        isave = 0;
        for (i = height-1; i >= 0; i--){
          for (j = 0; j < width; j++)     {
          fsave = (((data[(i*width)+j] - dmin)/wrange)*(float)(npseudocolors))+(float)nsaccolors+1.0+(float)ndefcolors;
          fsave = fsave < fmin ? fmin : fsave;
          fsave = fsave > fmax ? fmax : fsave;
#ifdef USE_X11_MULTIPLE_DEPTHS
	  if(depth > 16) 
	    array4[isave++] = pixdef3[(unsigned int)fsave].pixel;
	  else if(depth > 8)
	    array2[isave++] = pixdef3[(unsigned int)fsave].pixel;
	  else 
#endif	  
          array[isave++] = pixdef3[(unsigned int)fsave].pixel;
          }
        }


L_8888:
	return array;

} /* end of function */






