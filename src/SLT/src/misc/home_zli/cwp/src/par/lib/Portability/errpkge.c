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

/* #include <stdarg.h> */
#include "./stdarg.h"
#include "par.h"

/* errpkge - routines for reporting errors
 *
 * err     - print warning on application program error and die
 * warn    - print warning on application program error
 *
 * Returns:
 *	err  - void
 *	warn - void
 *
 * Synopsis:
 *	void err(format, args)
 *	format - a printf format string ("\n" not needed)
 *      args   - the variables referenced in the format string
 *
 *	void warn(format, args)
 *	format - a printf format string ("\n" not needed)
 *      args   - the variables referenced in the format string
 *
 *
 * Credits: 
 *	Kernighan and Pike, "The UNIX Programming Environment", page 207.
 *	Also Rochkind, "Advanced UNIX Programming", page 13.
 *	SEP: Jeff Thorson, Stew
 *	CWP: Shuki, Jack
 *
 * Examples:
 *	err("Cannot divide %f by %f", x, y);
 *	warn("fmax = %f exceeds half nyquist= %f", fmax, 0.25/dt);
 *
 *	if (-1 == (fd = open(xargv[1], O_RDONLY)))
 *		err("can't open %s", xargv[1]);
 *	...
 *	if (-1 == close(fd))
 *		err("close failed");
 *
 */


extern int errno;

void err(char *fmt, ...)
{
	va_list args;

 
	if (EOF == fflush(stdout)) {
		fprintf(stderr, "\nerr: fflush failed on stdout");
	}
	fprintf(stderr, "\n%s: ", xargv[0]);
	va_start(args,fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	if (errno) fprintf(stderr, " (%s)", strerror(errno));
	fprintf(stderr, "\n");
	exit(EXIT_FAILURE);
}



void warn(char *fmt, ...)
{
	va_list args;

	if (EOF == fflush(stdout)) {
		fprintf(stderr, "\nwarn: fflush failed on stdout");
	}
	fprintf(stderr, "\n%s: ", xargv[0]);
	va_start(args,fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
	return;
}




/* The following two codes are OBSOLESCENT */

void syserr(char *fmt, ...)
{
	va_list args;

	if (EOF == fflush(stdout)) {
		fprintf(stderr, "\nsyserr: fflush failed on stdout");
	}
	fprintf(stderr, "\n%s: ", xargv[0]);
	va_start(args,fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, " (%s)\n", strerror(errno));
	exit(EXIT_FAILURE);
}

void syswarn(char *fmt, ...)
{
	va_list args;

	if (EOF == fflush(stdout)) {
		fprintf(stderr, "\nsyswarn: fflush failed on stdout");
	}
	fprintf(stderr, "\n%s: ", xargv[0]);
	va_start(args,fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, " (%s)\n", strerror(errno));
	errno = 0;	/* having reported the error, reset errno */
	return;
}

#ifdef TEST
main(int argc, char **argv)
{
	int fd;
	float fmax, dt=0.004, nyq,  x=1.0, y=0.0;

	initargs(argc, argv);

	nyq = 1.0/(2.0*dt); fmax = nyq/2.0 + .01;

	fprintf(stderr, "\nTrue values:\n");
	fprintf(stderr, "fmax=%f, half nyquist=%f\n", fmax, 0.25/dt);
	fprintf(stderr, "filename=%s\n", xargv[1]);
	fprintf(stderr, "x=%f, y=%f\n\n", x, y);

 	warn("warn: fmax = %f exceeds half nyquist= %f", fmax, 0.25/dt);

 	warn("warn: Cannot divide x=%f by y=%f", x, y);
	
  	if (-1 == (fd = open(xargv[1], O_RDONLY)))
		err("err: can't open %s", xargv[1]);

	return EXIT_SUCCESS;
}
#endif
