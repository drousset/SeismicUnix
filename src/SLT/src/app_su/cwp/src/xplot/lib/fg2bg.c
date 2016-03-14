#include "xplot.h"
#include "par.h"


/* save current window to background for later use */ 
void fg2bg(Display *dpy, Window win)
{
        XWindowAttributes WinAttr;
        Pixmap BGPix;
        GC localGC;

	/*
	fprintf(stderr,"enter fd2bg program win=%x \n",win);
	*/

        XGetWindowAttributes(dpy, win, &WinAttr);
        BGPix=XCreatePixmap(dpy, DefaultRootWindow(dpy),
                WinAttr.width, WinAttr.height, WinAttr.depth);
        localGC=XCreateGC(dpy, DefaultRootWindow(dpy), 0, 0);
	/*
        BGPix=XCreatePixmap(dpy, win,
                WinAttr.width, WinAttr.height, WinAttr.depth);
        localGC=XCreateGC(dpy, win, 0, 0);
	*/
        XCopyArea(dpy, win, BGPix, localGC,
                0, 0, WinAttr.width, WinAttr.height, 0, 0);
        XSetWindowBackgroundPixmap(dpy, win, BGPix);
        XClearWindow(dpy, win);
}
