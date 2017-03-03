
#include <stdio.h>
#include <X11/Xlib.h>
#include "config.h"
#include "../../inc/gd3.x11.h"

void
expose3() {
  
#ifdef USE_X11_DOUBLE_BUFFER
  int out;
  XEvent event, peek;
  XWindowAttributes attributes;

  XGetWindowAttributes(display3, plotw3[c_win3].win, &attributes);

  event.type = Expose;
  event.xexpose.display = display3;
  event.xexpose.window  = plotw3[c_win3].win;
  event.xexpose.send_event = 1;
  event.xexpose.x = 0;
  event.xexpose.y = 0;
  event.xexpose.width  = attributes.width;
  event.xexpose.height = attributes.height;
  event.xexpose.count  = 0;
  
  XFlush(display3);
  XSendEvent(display3, plotw3[c_win3].win, False, ExposureMask, &event);
  XFlush(display3);
#endif /* USE_X11_DOUBLE_BUFFER */

}

