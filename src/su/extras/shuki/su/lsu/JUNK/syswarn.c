/* syswarn - print warning on a system error
 *
 * Returns:
 *	int: errno
 *
 * Synopsis:
 *	int syswarn( format, args)
 *	format - a printf format string ("\n" not needed) usually
 *		 giving the name of the offending system call
 *      args   - the variables referenced in the format string, often
 *		 just an offending file name
 *
 * Notes:
 *	For portability, this routine uses varargs and the "format"
 *      and "args" are bundled as per vararg.h.
 *
 * Example:
 *	...
 *	if (-1 == (fd = open(xargv[j], 0)))
 *		syswarn("can't open %s, processing next file",
 *			xargv[j]);
 *	continue;
 *	...
 *
 * Credits: 
 *	Kernighan and Pike, "The UNIX Programming Environment",
 *	page 207.  Also Rochkind, "Advanced UNIX Programming",
 *      page 13.
 *      CWP: Jack, Shuki
 *
 * This program belongs to the Center for Wave Phenomena
 * Colorado School of Mines
 *
 * $Author: jkcohen $
 * $Source: /src/segy/lib/RCS/syswarn.c,v $
 * $Revision: 1.15 $ ; $Date: 87/06/11 09:54:39 $
 * $State: Exp $
 */

/* Embed Revision Control System identifier strings */
static char progid[] =
	"   $Source: /src/segy/lib/RCS/syswarn.c,v $";
static char revid[] =
	"   $Revision: 1.15 $ ; $Date: 87/06/11 09:54:39 $";


#include "../include/cwp.h"
#include <varargs.h>

int syswarn(va_alist)
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
	return(errno);
}


/* For FORTRAN */
int syswarn_(va_alist)
va_dcl
{
	return(syswarn(va_alist));
}
