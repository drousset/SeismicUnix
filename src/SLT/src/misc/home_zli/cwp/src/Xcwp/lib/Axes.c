/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/******************************************************************************
Axes.c: The Axes Widget
*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 08/28/90
******************************************************************************/

#include <X11/IntrinsicP.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/CoreP.h>
#include "cwp.h"
#include "Xcwp/Xcwp.h"
#include "Xcwp/AxesP.h"
#include "Xcwp/Axes.h"

/* resources */
static XtResource resources[] = {
	{XtNgrid1,XcwpCAxesGrid,XcwpRAxesGrid,sizeof(int),
		XtOffset(XcwpAxesWidget,axes.grid1), 
		XtRString,"none"},
	{XtNgrid2,XcwpCAxesGrid,XcwpRAxesGrid,sizeof(int),
		XtOffset(XcwpAxesWidget,axes.grid2), 
		XtRString,"none"},
	{XtNnTic1,XtCParameter,XtRInt,sizeof(int),
		XtOffset(XcwpAxesWidget,axes.n1tic), 
		XtRString,"1"},
	{XtNnTic2,XtCParameter,XtRInt,sizeof(int),
		XtOffset(XcwpAxesWidget,axes.n2tic), 
		XtRString,"1"},
	{XtNlabel1,XtCString,XtRString,sizeof(String),
		XtOffset(XcwpAxesWidget,axes.label1), 
		XtRString,""},
	{XtNlabel2,XtCString,XtRString,sizeof(String),
		XtOffset(XcwpAxesWidget,axes.label2), 
		XtRString,""},
	{XtNtitle,XtCString,XtRString,sizeof(String),
		XtOffset(XcwpAxesWidget,axes.title), 
		XtRString,""},
	{XtNstyle,XcwpCAxesStyle,XcwpRAxesStyle,sizeof(int),
		XtOffset(XcwpAxesWidget,axes.style), 
		XtRString,"seismic"},
	{XtNaxesColor,XtCColor,XtRPixel,sizeof(Pixel),
		XtOffset(XcwpAxesWidget,axes.axescolor), 
		XtRString,"black"},
	{XtNgridColor,XtCColor,XtRPixel,sizeof(Pixel),
		XtOffset(XcwpAxesWidget,axes.gridcolor), 
		XtRString,"black"},
	{XtNtitleColor,XtCColor,XtRPixel,sizeof(Pixel),
		XtOffset(XcwpAxesWidget,axes.titlecolor), 
		XtRString,"black"},
	{XtNlabelFont,XtCFont,XtRFont,sizeof(Font),
		XtOffset(XcwpAxesWidget,axes.labelfont), 
		XtRString,"fixed"},
	{XtNtitleFont,XtCFont,XtRFont,sizeof(Font),
		XtOffset(XcwpAxesWidget,axes.titlefont), 
		XtRString,"fixed"},
	{XtNresizeCallback,XtCCallback,XtRCallback,sizeof(caddr_t),
		XtOffset(XcwpAxesWidget,axes.resize), 
		XtRCallback,NULL},
	{XtNexposeCallback,XtCCallback,XtRCallback,sizeof(caddr_t),
		XtOffset(XcwpAxesWidget,axes.expose), 
		XtRCallback,NULL},
	{XtNinputCallback,XtCCallback,XtRCallback,sizeof(caddr_t),
		XtOffset(XcwpAxesWidget,axes.input), 
		XtRCallback,NULL},
};

/* functions defined and used internally */
static void ClassInitialize (void);
static void Initialize (XcwpAxesWidget request, XcwpAxesWidget new);
static void Destroy (XcwpAxesWidget w);
static void Resize (XcwpAxesWidget w);
static void Redisplay (XcwpAxesWidget w, XEvent *event, Region region);
static Boolean SetValues (XcwpAxesWidget current, 
	XcwpAxesWidget request, 
	XcwpAxesWidget new);
static void fillCallbackStruct (XcwpAxesWidget w,
	int reason, XEvent *event, XcwpAxesCallbackStruct *cb);
static void inputAxes (XcwpAxesWidget w, XEvent *event, 
	char *args[], int nargs);
static void XcwpStringToAxesGrid (XrmValue *args, int *nargs, 
	XrmValue *fromVal, XrmValue *toVal);
static void XcwpStringToAxesStyle (XrmValue *args, int *nargs, 
	XrmValue *fromVal, XrmValue *toVal);

/* translations */
static char defaultTranslations[] = 
	"<Btn1Down>: input()\n"
	"<Btn1Up>: input()\n"
	"<Btn1Motion>: input()\n";

/* action procedures */
static XtActionsRec actionsList[] = {
	{"input",(XtActionProc)inputAxes},
};

/* class record */
XcwpAxesClassRec  XcwpaxesClassRec = {
	/* CoreClassPart */
	{
	(WidgetClass) &widgetClassRec,  /* superclass            */
	"XcwpAxes",                     /* class_name            */
	sizeof(XcwpAxesRec),            /* widget_size           */
	ClassInitialize,                /* class_initialize      */
	NULL,                           /* class_part_initialize */
	FALSE,                          /* class_inited          */
	Initialize,                     /* initialize            */
	NULL,                           /* initialize_hook       */
	XtInheritRealize,               /* realize               */
	actionsList,                    /* actions               */
	XtNumber(actionsList),          /* num_actions           */
	resources,                      /* resources             */
	XtNumber(resources),            /* num_resources         */
	NULLQUARK,                      /* xrm_class             */
	TRUE,                           /* compress_motion       */
	TRUE,                           /* compress_exposure     */
	TRUE,                           /* compress_enterleave   */
	TRUE,                           /* visible_interest      */
	Destroy,                        /* destroy               */
	Resize,                         /* resize                */
	Redisplay,                      /* expose                */
	SetValues,                      /* set_values            */
	NULL,                           /* set_values_hook       */
	XtInheritSetValuesAlmost,       /* set_values_almost     */
	NULL,                           /* get_values_hook       */
	NULL,                           /* accept_focus          */
	XtVersion,                      /* version               */
	NULL,                           /* callback private      */
	defaultTranslations,            /* tm_table              */
	NULL,                           /* query_geometry        */
	NULL,                           /* display_accelerator   */
	NULL,                           /* extension             */
	},
	/* Axes class fields */
	{
	0,                              /* ignore                */
	}
};
WidgetClass xcwpAxesWidgetClass = (WidgetClass) &XcwpaxesClassRec;

/* class functions */
static void ClassInitialize (void)
{
	/* add type converters */
	XtAddConverter(XtRString,XcwpRAxesGrid,XcwpStringToAxesGrid,NULL,0);
	XtAddConverter(XtRString,XcwpRAxesStyle,XcwpStringToAxesStyle,NULL,0);
}
static void Initialize (XcwpAxesWidget request, XcwpAxesWidget new)
{
	/* initialize axes values and pads */
	new->axes.x1beg = 0.0;
	new->axes.x1end = 1.0;
	new->axes.x2beg = 0.0;
	new->axes.x2end = 1.0;
	new->axes.p1beg = 0.0;
	new->axes.p1end = 0.0;
	new->axes.p2beg = 0.0;
	new->axes.p2end = 0.0;
	
	/* ensure window size is not zero */	
	if (request->core.width==0) new->core.width = 200;
	if (request->core.height==0) new->core.height = 200;

	/* set parameters that depend on window size */
	Resize(new);
}
static void Destroy (XcwpAxesWidget w)
{
	XtRemoveAllCallbacks(w,XtNresizeCallback,w->axes.input);
	XtRemoveAllCallbacks(w,XtNexposeCallback,w->axes.input);
	XtRemoveAllCallbacks(w,XtNinputCallback,w->axes.input);
}
static void Resize (XcwpAxesWidget w)
{
	XcwpAxesCallbackStruct cb;
	XFontStruct *fa,*ft;
	int labelch,labelcw,titlech,titlecw,bl,bt,br,bb;
	
	/* get fonts and determine character dimensions */
	fa = XQueryFont(XtDisplay(w),w->axes.labelfont);
	labelch = fa->max_bounds.ascent+fa->max_bounds.descent;
	labelcw = fa->max_bounds.lbearing+fa->max_bounds.rbearing;
	ft = XQueryFont(XtDisplay(w),w->axes.titlefont);
	titlech = ft->max_bounds.ascent+ft->max_bounds.descent;
	titlecw = ft->max_bounds.lbearing+ft->max_bounds.rbearing;

	/* determine axes rectangle position and dimensions */
	bl = 10*labelcw;
	br = w->core.width-5*labelcw;
	while (br<bl) {
		br += labelcw;
		bl -= labelcw;
	}
	if (bl<0) bl = 0;
	if (br>w->core.width) br = w->core.width;
	if (w->axes.style==XcwpNORMAL) {
		bt = labelch+labelch/2+titlech;
		bb = w->core.height-3*labelch;
	} else {
		bt = 3*labelch;
		bb = w->core.height-labelch-labelch/2-titlech;
	}
	while (bb<bt) {
		bb += labelch;
		bt -= labelch;
	}
	if (bt<0) bt = 0;
	if (bb>w->core.height) bb = w->core.height;
	w->axes.x = bl;
	w->axes.y = bt;
	w->axes.width = br-bl;
	w->axes.height = bb-bt;
		
	/* call callbacks */
	fillCallbackStruct(w,XcwpCR_RESIZE,NULL,&cb);
	XtCallCallbacks (w,XtNresizeCallback,&cb);
} 
static void Redisplay (XcwpAxesWidget w, XEvent *event, Region region)
{
	Display *dpy=XtDisplay(w);
	Window *win=XtWindow(w);
	int x=w->axes.x;
	int y=w->axes.y;
	int width=w->axes.width;
	int height=w->axes.height;
	float x1beg=w->axes.x1beg;
	float x1end=w->axes.x1end;
	float x2beg=w->axes.x2beg;
	float x2end=w->axes.x2end;
	float p1beg=w->axes.p1beg;
	float p1end=w->axes.p1end;
	float p2beg=w->axes.p2beg;
	float p2end=w->axes.p2end;
	int n1tic=w->axes.n1tic;
	int n2tic=w->axes.n2tic;
	int grid1=w->axes.grid1;
	int grid2=w->axes.grid2;
	char *label1=w->axes.label1;
	char *label2=w->axes.label2;
	char *title=w->axes.title;
	Font labelfont=w->axes.labelfont;
	Font titlefont=w->axes.titlefont;
	Pixel axescolor=w->axes.axescolor;
	Pixel gridcolor=w->axes.gridcolor;
	Pixel titlecolor=w->axes.titlecolor;
	int style=w->axes.style;
	XGCValues values;
	GC gca,gcg,gct;
	XcwpAxesCallbackStruct cb;
	XFontStruct *fa,*ft;
	int labelca,labelcd,labelch,labelcw,titleca,titlecd,titlech,titlecw,
		nnum,ntic,xa,ya,tw,ticsize,ticb,numb,labelb,lstr,grided,grid,
		n1num,n2num,scr;
	float dnum,fnum,dtic,amin,amax,base,scale,anum,atic,azero;
	float d1num=0.0,f1num=0.0,d2num=0.0,f2num=0.0;
	char str[256],dash[2],*label;
	
	/* if not visible, then simply return */
	if (!w->core.visible) return;
		
	/* call callbacks before drawing axes (so grid will be on top) */
	fillCallbackStruct(w,XcwpCR_EXPOSE,event,&cb);
	XtCallCallbacks (w,XtNexposeCallback,&cb);
	
	/* create GCs */
	gca = XCreateGC(dpy,win,0,&values);
	gcg = XCreateGC(dpy,win,0,&values);
	gct = XCreateGC(dpy,win,0,&values);
	
	/* set colors */
	XSetForeground(dpy,gca,axescolor);
	XSetForeground(dpy,gcg,gridcolor);
	XSetForeground(dpy,gct,titlecolor);

	/* set fonts and determine character dimensions */
	fa = XQueryFont(dpy,labelfont);
	XSetFont(dpy,gca,labelfont);
	labelca = fa->max_bounds.ascent;
	labelcd = fa->max_bounds.descent;
	labelch = fa->max_bounds.ascent+fa->max_bounds.descent;
	labelcw = fa->max_bounds.lbearing+fa->max_bounds.rbearing;
	ft = XQueryFont(dpy,titlefont);
	XSetFont(dpy,gct,titlefont);
	titleca = ft->max_bounds.ascent;
	titlecd = ft->max_bounds.descent;
	titlech = ft->max_bounds.ascent+ft->max_bounds.descent;
	titlecw = ft->max_bounds.lbearing+ft->max_bounds.rbearing;

	/* determine tic size */
	ticsize = labelcw;

	/* determine numbered tic intervals */
	n1num = (style==XcwpNORMAL ? width : height)/(8*labelcw);
	scaxis(x1beg,x1end,&n1num,&d1num,&f1num);
	n2num = (style==XcwpNORMAL ? height : width)/(8*labelcw);
	scaxis(x2beg,x2end,&n2num,&d2num,&f2num);

	/* draw horizontal axis */
	if (style==XcwpNORMAL) {
		amin = (x1beg<x1end)?x1beg:x1end;
		amax = (x1beg>x1end)?x1beg:x1end;
		dnum = d1num;  fnum = f1num;  ntic = n1tic;
		scale = width/(x1end+p1end-x1beg-p1beg);
		base = x-scale*(x1beg+p1beg);
		ya = y+height;
		ticb = ticsize;
		numb = ticb+labelca;
		labelb = numb+labelch;
		grid = grid1;
		label = label1;
	} else {
		amin = (x2beg<x2end)?x2beg:x2end;
		amax = (x2beg>x2end)?x2beg:x2end;
		dnum = d2num;  fnum = f2num;  ntic = n2tic;
		scale = width/(x2end+p2end-x2beg-p2beg);
		base = x-scale*(x2beg+p2beg);
		ya = y;
		ticb = -ticsize;
		numb = ticb-labelcd;
		labelb = numb-labelch;
		grid = grid2;
		label = label2;
	}
	if (grid==XcwpSOLID) {
		XSetLineAttributes(dpy,gcg,1L,LineSolid,CapButt,JoinMiter);
		grided = True;
	} else if (grid==XcwpDASH) {
		grided = True;
		XSetLineAttributes(dpy,gcg,1L,LineOnOffDash,CapButt,JoinMiter);
		dash[0] = 8;  dash[1] = 4;
		XSetDashes(dpy,gcg,0,dash,2);
	} else if (grid==XcwpDOT) {
		grided = True;
		XSetLineAttributes(dpy,gcg,1L,LineOnOffDash,CapButt,JoinMiter);
		dash[0] = 1;  dash[1] = 4;
		XSetDashes(dpy,gcg,0,dash,2);
	} else
		grided = False;
	azero = 0.0001*(amax-amin);
	for (anum=fnum; anum<=amax; anum+=dnum) {
		if (anum<amin) continue;
		xa = base+scale*anum;
		if (grided) XDrawLine(dpy,win,gcg,xa,y,xa,y+height);
		XDrawLine(dpy,win,gca,xa,ya,xa,ya+ticb);
		if (anum>-azero && anum<azero)
			sprintf(str,"%1.5g",0.0);
		else
			sprintf(str,"%1.5g",anum);
		lstr = strlen(str);
		tw = XTextWidth(fa,str,lstr);
		XDrawString(dpy,win,gca,xa-tw/2,ya+numb,str,lstr);
	}
	dtic = dnum/ntic;
	for (atic=fnum-ntic*dtic-dtic; atic<=amax; atic+=dtic) {
		if (atic<amin) continue;
		xa = base+scale*atic;
		XDrawLine(dpy,win,gca,xa,ya,xa,ya+ticb/2);
	}
	lstr = strlen(label);
	tw = XTextWidth(fa,label,lstr);
	XDrawString(dpy,win,gca,x+width-tw,ya+labelb,label,lstr);

	/* draw vertical axis */
	if (style==XcwpNORMAL) {
		amin = (x2beg<x2end)?x2beg:x2end;
		amax = (x2beg>x2end)?x2beg:x2end;
		dnum = d2num;  fnum = f2num;  ntic = n2tic;
		scale = -height/(x2end+p2end-x2beg-p2beg);
		base = y+height-scale*(x2beg+p2beg);
		grid = grid2;
		label = label2;
	} else {
		amin = (x1beg<x1end)?x1beg:x1end;
		amax = (x1beg>x1end)?x1beg:x1end;
		dnum = d1num;  fnum = f1num;  ntic = n1tic;
		scale = height/(x1end+p1end-x1beg-p1beg);
		base = y-scale*(x1beg+p1beg);
		grid = grid1;
		label = label1;
	}
	xa = x;
	ticb = -ticsize;
	numb = ticb-ticsize/4;
	if (grid==XcwpSOLID) {
		XSetLineAttributes(dpy,gcg,1L,LineSolid,CapButt,JoinMiter);
		grided = True;
	} else if (grid==XcwpDASH) {
		grided = True;
		XSetLineAttributes(dpy,gcg,1L,LineOnOffDash,CapButt,JoinMiter);
		dash[0] = 8;  dash[1] = 4;
		XSetDashes(dpy,gcg,0,dash,2);
	} else if (grid==XcwpDOT) {
		grided = True;
		XSetLineAttributes(dpy,gcg,1L,LineOnOffDash,CapButt,JoinMiter);
		dash[0] = 1;  dash[1] = 4;
		XSetDashes(dpy,gcg,0,dash,2);
	} else
		grided = False;
	azero = 0.0001*(amax-amin);
	for (anum=fnum; anum<=amax; anum+=dnum) {
		if (anum<amin) continue;
		ya = base+scale*anum;
		if (grided) XDrawLine(dpy,win,gcg,x,ya,x+width,ya);
		XDrawLine(dpy,win,gca,xa,ya,xa+ticb,ya);
		if (anum>-azero && anum<azero)
			sprintf(str,"%1.5g",0.0);
		else
			sprintf(str,"%1.5g",anum);
		lstr = strlen(str);
		tw = XTextWidth(fa,str,lstr);
		XDrawString(dpy,win,gca,xa+numb-tw,ya+labelca/4,str,lstr);
	}
	dtic = dnum/ntic;
	for (atic=fnum-ntic*dtic-dtic; atic<=amax; atic+=dtic) {
		if (atic<amin) continue;
		ya = base+scale*atic;
		XDrawLine(dpy,win,gca,xa,ya,xa+ticb/2,ya);
	}
	lstr = strlen(label);
	if (style==XcwpNORMAL)
		XDrawString(dpy,win,gca,
			x+ticb-9*labelcw,
			y+labelca/4-labelch,label,lstr);
	else
		XDrawString(dpy,win,gca,
			x+ticb-9*labelcw,
			y+height+labelca/4+labelch,label,lstr);
	
	/* draw title */
	lstr = strlen(title);
	tw = XTextWidth(ft,title,lstr);
	if (style==XcwpNORMAL)
		XDrawString(dpy,win,gct,
			x+width/2-tw/2,
			y+labelca/4-labelch-labelch,title,lstr);
	else
		XDrawString(dpy,win,gct,
			x+width/2-tw/2,
			y+height+labelca/4+labelch+titleca,title,lstr);

	/* draw axes box */
	XDrawRectangle(dpy,win,gca,x,y,width,height);
	
	/* free GCs */
	XFreeGC(dpy,gca);
	XFreeGC(dpy,gcg);
	XFreeGC(dpy,gct);
}
static Boolean SetValues (XcwpAxesWidget current, 
	XcwpAxesWidget request, 
	XcwpAxesWidget new)
{
	Boolean redraw = FALSE;
	
	return redraw; 
} 

/* utilities */
static void fillCallbackStruct (XcwpAxesWidget w,
	int reason, XEvent *event, XcwpAxesCallbackStruct *cb)
{
	cb->reason = reason;
	cb->event = event;
	cb->x = w->axes.x;
	cb->y = w->axes.y;
	cb->width = w->axes.width;
	cb->height = w->axes.height;
	cb->x1beg = w->axes.x1beg;
	cb->x1end = w->axes.x1end;
	cb->x2beg = w->axes.x2beg;
	cb->x2end = w->axes.x2end;
	cb->p1beg = w->axes.p1beg;
	cb->p1end = w->axes.p1end;
	cb->p2beg = w->axes.p2beg;
	cb->p2end = w->axes.p2end;
	cb->style = w->axes.style;
}
	
/* action procedures */
static void inputAxes (XcwpAxesWidget w, XEvent *event, 
	char *args[], int nargs)
{
	XcwpAxesCallbackStruct cb;
		
	/* call callback */
	fillCallbackStruct(w,XcwpCR_INPUT,event,&cb);
	XtCallCallbacks (w,XtNinputCallback,&cb);
} 

/* public functions */
Boolean XcwpPointInAxesRectangle (XcwpAxesWidget w, Position x, Position y)
{
	Position xa=w->axes.x,ya=w->axes.y;
	Dimension wa=w->axes.width,ha=w->axes.height;
	
	return (x>=xa && x<=xa+wa && y>=ya && y<=ya+ha);
}
void XcwpSetAxesValues (XcwpAxesWidget w,
	float x1beg, float x1end, float x2beg, float x2end)
{
	w->axes.x1beg = x1beg;
	w->axes.x1end = x1end;
	w->axes.x2beg = x2beg;
	w->axes.x2end = x2end;
}
void XcwpSetAxesPads (XcwpAxesWidget w,
	float p1beg, float p1end, float p2beg, float p2end)
{
	w->axes.p1beg = p1beg;
	w->axes.p1end = p1end;
	w->axes.p2beg = p2beg;
	w->axes.p2end = p2end;
}

/* resource type converters */
void XcwpStringToAxesGrid (XrmValue *args, int *nargs, 
	XrmValue *fromVal, XrmValue *toVal)
{
	static int result;
	char *string=fromVal->addr;
	
	/* convert axes grid string in fromVal to int in toVal */
	if (strcmp(string,"none")==0)
		result = XcwpNONE;
	else if (strcmp(string,"dot")==0)
		result = XcwpDOT;
	else if (strcmp(string,"dash")==0)
		result = XcwpDASH;
	else if (strcmp(string,"solid")==0)
		result = XcwpSOLID;
	else {
		result = XcwpNONE;
		XtWarning("Invalid AxesGrid specification!");
	}
	toVal->size = sizeof(int);
	toVal->addr = (caddr_t)&result;
}
void XcwpStringToAxesStyle (XrmValue *args, int *nargs, 
	XrmValue *fromVal, XrmValue *toVal)
{
	static int result;
	char *string=fromVal->addr;
	
	/* convert axes style string in fromVal to int in toVal */
	if (strcmp(string,"normal")==0)
		result = XcwpNORMAL;
	else if (strcmp(string,"seismic")==0)
		result = XcwpSEISMIC;
	else {
		result = XcwpNORMAL;
		XtWarning("Invalid AxesStyle specification!");
	}
	toVal->size = sizeof(int);
	toVal->addr = (caddr_t)&result;
}

