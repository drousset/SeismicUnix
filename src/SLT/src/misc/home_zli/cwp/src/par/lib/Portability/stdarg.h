/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/* @(#)stdarg.h	1.13  com/inc,3.1,9021 1/28/90 19:35:38 */
/*
 * COMPONENT_NAME: (INCSTD) Standard Include Files
 *
 * FUNCTIONS: 
 *
 * ORIGINS: 27
 *
 * (C) COPYRIGHT International Business Machines Corp. 1989
 * All Rights Reserved
 * Licensed Materials - Property of IBM
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 *
 *  Kludged up for PS2!!!
 */

#ifndef _H_STDARG
#define _H_STDARG


typedef char *	va_list;

#define va_start(list,parmN) list = (char *) (&(parmN) + 1)
#define va_end(list)
#define va_arg(list, mode) ((mode *)((list) += sizeof(mode)))[-1]

#endif /* _H_STDARG */
