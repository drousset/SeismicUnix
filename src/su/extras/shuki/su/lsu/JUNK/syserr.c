/* syserr - print warning on a system error and die
 *
 * Returns:
 *	void
 *
 * Synopsis:
 *	void syserr(format, args)
 *	format - a printf format string ("\n" not needed) usually
 *		 giving the name of the offending system call
 *      args   - the variables referenced in the format string, often
 *		 just an offending file name
 *
 * Notes:
 *	For portability, this routine uses varargs and the "format"
 *      and "args" are bundled as per vararg.h.
 *
 * Credits:
 *	CWP: Jack, Shuki
 *
 * Examples:
 *	if (-1 == (fd = open(xargv[1], 0)))
 *		syserr("can't open %s", xargv[1]);
 *	...
 *	if (-1 == close(fd))
 *		syserr("close failed");
 *
 */

#include <stdio.h>
#include <varargs.h>

extern int xargc;
extern char **xargv;

/* #include "../include/cwp.h" */


void syserr(va_alist)
va_dcl
{
	va_list args;
	char *format;
	extern int errno, sys_nerr;
	extern char *sys_errlist[];

	if (EOF == fflush(stdout)) {
		err("syswarn: fflush failed on stdout");
	}
	fprintf(stderr, "\n%s: ", xargv[0]);
	va_start(args);
	format = va_arg(args, char *);
	_doprnt(format, args, stderr);
	if (errno > 0 && errno < sys_nerr) { 
		fprintf(stderr, " (%s)", sys_errlist[errno]);
	}
	fprintf(stderr, "\n");
	exit(1);
}
