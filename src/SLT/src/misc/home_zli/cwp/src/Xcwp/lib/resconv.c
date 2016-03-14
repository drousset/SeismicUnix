/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/******************************************************************************
resconv.c:  general purpose resource type converters
*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 08/28/90
******************************************************************************/

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

void XcwpStringToFloat (XrmValue *args, int *nargs, 
	XrmValue *fromVal, XrmValue *toVal)
{
	static float result;
	
	/* ensure number of arguments is zero */
	if (*nargs!=0)
		XtWarning("String to Float conversion needs no arguments!");
	
	/* convert string in fromVal to float in toVal */
	if (sscanf((char *)fromVal->addr,"%f",&result)==1) {

		/* toVal points to the result */
		toVal->size = sizeof(float); 
		toVal->addr = (caddr_t)&result;
	
	/* if sscanf fails */
	} else {
		XtStringConversionWarning((char *)fromVal->addr,"Float");
	}
}
