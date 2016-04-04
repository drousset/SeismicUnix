/* warn - print warning on application program error
 *
 * Returns:
 *	void
 *
 * Synopsis:
 *	void warn(format, args)
 *	format - a printf format string ("\n" not needed)
 *      args   - the variables referenced in the format string
 *
 * Notes:
 *	For portability, this routine uses varargs and the "format"
 *      and "args" are bundled as per vararg.h.
 *
 * Credits:
 *	SEP: Jeff Thorson, Stew
 *	CWP: Shuki, Jack
 *
 * Example:
 *	warn(fmax = %f exceeds half nyquist= %f", fmax, 0.25/dt);
 *
 * This program belongs to the Center for Wave Phenomena
 * Colorado School of Mines
 *
 * $Author: jkcohen $
 * $Source: /src/segy/lib/RCS/warn.c,v $
 * $Revision: 1.18 $ ; $Date: 87/06/11 09:54:10 $
 * $State: Exp $
*/

/* Embed Revision Control System identifier strings */
static char progid[] =
	"   $Source: /src/segy/lib/RCS/warn.c,v $";
static char revid[] =
	"   $Revision: 1.18 $ ; $Date: 87/06/11 09:54:10 $";


#include "../include/cwp.h"

#include <varargs.h>
void warn(va_alist)
va_dcl
{
	va_list args;
	char *format;

	if (EOF == fflush(stdout)) {
		fprintf(stderr, "\nwarn: fflush failed on stdout");
	}
	fprintf(stderr, "\n(WARN) %s: ", xargv[0]);
	va_start(args);
	format = va_arg(args, char *);
	_doprnt(format, args, stderr);
	fprintf(stderr, "\n");
}

/* For FORTRAN */
void warn_(va_alist)
va_dcl
{
	warn(va_alist);
}
