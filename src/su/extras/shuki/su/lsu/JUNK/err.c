/* err - print warning on application program error and die
 *
 * Returns:
 *	void
 *
 * Synopsis:
 *	void err(format, args)
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
 *	err("Cannot divide %f by %f", x, y);
 *
 * This program belongs to the Center for Wave Phenomena
 * Colorado School of Mines
 *
 * $Author: jkcohen $
 * $Source: /src/segy/lib/RCS/err.c,v $
 * $Revision: 1.15 $ ; $Date: 87/06/11 09:53:13 $
 * $State: Exp $
*/

/* Embed Revision Control System identifier strings */
static char progid[] =
	"   $Source: /src/segy/lib/RCS/err.c,v $";
static char revid[] =
	"   $Revision: 1.15 $ ; $Date: 87/06/11 09:53:13 $";


#include <stdio.h>

extern int xargc;
extern char **xargv;

#include <varargs.h>
void err(va_alist)
va_dcl
{
	va_list args;
	char *format;

	if (EOF == fflush(stdout)) {
		fprintf(stderr, "\nerr: fflush failed on stdout");
	}
	fprintf(stderr, "\n(ERR) %s: ", xargv[0]);
	va_start(args);
	format = va_arg(args, char *);
	_doprnt(format, args, stderr);
	fprintf(stderr, "\n");
	exit(1);
}

/* For FORTRAN */
void err_(va_alist)
va_dcl
{
	err(va_alist);
}


/* I don't know why the following does not work:
#include <varargs.h>

void err(va_alist)
va_dcl
{
	warn(va_alist);
	exit(1);
}
*/
