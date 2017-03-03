/*******************************************************************************
** PURPOSE:
*    To erase the window.
*
** GLOBAL INPUT:
*    gd3.x11.h:  plotw3->win
*
** SUBROUTINES CALLED:
*    XClearWindow, XFlush
*******************************************************************************/

#include <X11/Xlib.h>
#include "config.h"
#include "../../inc/gd3.x11.h"
#include "complex.h"
#include "proto.h"
#include "gem.h"

void
fill_background3(int window) {
  long int color;
  color = (color_on()) ? color_background() : color_background_default() ;
  setcolor(color);

  XSetForeground(display3, plotw3[window].gc, color3);
  XSetBackground(display3, plotw3[window].gc, color3);

  XFillRectangle(display3, plotw3_pixmap, plotw3[window].gc, 
		 0, 0,
		 plotw3[window].width_p,
		 plotw3[window].height_p);  

  color = (color_on()) ? color_foreground() : color_foreground_default() ;
  setcolor(color);

}

void
erase3() {

/* Erase window */

#ifdef USE_X11_DOUBLE_BUFFER
  fill_background3(c_win3);
#endif /* USE_X11_DOUBLE_BUFFER */

  XClearWindow(display3,plotw3[c_win3].win);
  XFlush(display3);
}

/*******************************************************************************
** MODIFICATION HISTORY:
*    890608:  Modified to run under X11 rather than X10.  (kjm)
*    870318:  Changes due to gd3.x10.h structure change.
*    870223:  Original Version
*******************************************************************************/
