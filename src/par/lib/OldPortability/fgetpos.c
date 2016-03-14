/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "par.h"

typedef long fpos_t;

int fgetpos(FILE *stream, fpos_t *position)
{
	if (-1L == (long) *(position = &((fpos_t) ftell(stream)))) return 1;

	return position;
}


int fsetpos(FILE *stream, const fpos_t *position)
{
	
	if (fseek(stream, (long) *position, SEEK_SET)) return 1;

	return 0;
}
