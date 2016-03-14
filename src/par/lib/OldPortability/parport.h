/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* parport.h - include file for ANSI deficient systems */

#ifndef PARPORT_H
#define PARPORT_H


/* TYPEDEFS */
typedef long fpos_t;

/* DEFINES */
fgetpos(fp,pos)	{(pos) = ftell((fp));}
fsetpos(fp,pos)	{fseek((fp), (pos), SEEK_SET);}

#endif /* PARPORT_H */
