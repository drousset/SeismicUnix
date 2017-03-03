/*******************************************************************************
** PURPOSE:
*    To begin graphics to the XWindow device.
*
** OUTPUT ARGUMENTS:
*    nerr:  Error flag.  Set to 0 if no error occurs. (Pointer)
*
** GLOBAL OUTPUT:
*    gd3.x11:  scr_width_p3, scr_height_p3, title_font3
*
** SUBROUTINES CALLED:
*    XOpenDisplay, DisplayWidth, DisplayHeight, XLoadFont
*
** LOCAL VARIABLES:
*    display:      Display descriptor. (Pointer)
*    root_info:    Root window descriptor.
*    root_status:  Status of root window.
*    fontname:     Name of font for title label.
*******************************************************************************/

#include <stdio.h>
#ifdef STELLAR
#include "/usr/include/X11/Xlib.h"
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#endif
#include "../../inc/gd3.x11.h"

static char *fontnames[] = { "9x15", "fixed", "6x13" };
#define NUMBER_OF_FONTS (sizeof(fontnames) / sizeof(char*))

begindevice3(nerr)
  int *nerr;
{
  int i;
  Display *display;
  char *fontname = "9x15";
  char *display_name = NULL;
  XFontStruct *fontstruct = NULL;

  *nerr = 0;

/* Open display, get attributes of root window, and get title label font */

  if ((display = XOpenDisplay(display_name)) == NULL) {
    *nerr = 203;
    return;
  }
  else {
    scr_width_p3 = DisplayWidth(display,DefaultScreen(display));
    scr_height_p3 = DisplayHeight(display,DefaultScreen(display));
    for(i = 0; i < NUMBER_OF_FONTS; i++) {
      if( (fontstruct = XLoadQueryFont(display, fontnames[i])) != NULL) {
	break;
      }
    }
    if(fontstruct == NULL) {
      fprintf(stderr, "SAC Error Loading Default Font\n");
      title_font3 = 0;
    } else {
      title_font3 = XLoadFont(display,fontnames[i]);
    }
    display3 = display;
  }
}

/*******************************************************************************
** MODIFICATION HISTORY:
*    890606:  Rewritten from X10 to X11.  (kjm)
*    870317:  Added font initialization.
*    860301:  Original Version
*******************************************************************************/
