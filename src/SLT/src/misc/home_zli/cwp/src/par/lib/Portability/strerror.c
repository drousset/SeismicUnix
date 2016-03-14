/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*----------------------------------------------------------------------
 * Copyright (c) Colorado School of Mines, 1989.
 * All rights reserved.
 *
 * This code is part of SU.  SU stands for Seismic Unix, a processing line
 * developed at the Colorado School of Mines, partially based on Stanford
 * Exploration Project (SEP) software.  Inquiries should be addressed to:
 *
 *  Jack K. Cohen, Center for Wave Phenomena, Colorado School of Mines,
 *  Golden, CO 80401  (jkc@dix.mines.colorado.edu)
 *----------------------------------------------------------------------
 */

#include <stdio.h>
#include <errno.h>

/* strerror - compatibility routine for non ANSI C
 *
 * Synopsis:
 *	char *strerror(int error)
 *	error - value of errno
 *
 * Test by:  cc -DTEST strerror.c ; a.out
 */

/* prototype */
char *strerror(int error);

extern int errno;
extern char *sys_errlist[];

char *strerror(int error)
{
	return sys_errlist[error];
}


#ifdef TEST
main()
{
	int error;

	printf("Here are the error messages for errno = 1 to 32:\n");
	for (error = 1; error < 33; ++error)
		printf("%d: %s\n", error, strerror(error));
}

#endif
