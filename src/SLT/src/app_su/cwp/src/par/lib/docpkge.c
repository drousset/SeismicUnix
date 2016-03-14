/* Copyright (c) Colorado School of Mines, 1990.*/
/* All rights reserved.                       */

void pagedoc();
void requestdoc(int flag);

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

#include "par.h"

/* The subroutines "requestdoc" and "pagedoc" in this file implement
 * the self-documentation facility.
 *
 *----------------------------------------------------------------------
 * requestdoc - give selfdoc on user request
 *
 * Synopsis:
 *      void requestdoc(flag);
 *      int flag;
 *
 * Credit:
 *      CWP: Shuki, Jack
 *      HRC: Lyle
 *
 * Note:
 *      In the usual case, stdin is used to pass in data.  However,
 *      some programs (eg. synthetic data generators) don't use stdin
 *      to pass in data and some programs require two or more arguments
 *      besides the command itself (eg. sudiff) and don't use stdin.
 *      In this last case, we give selfdoc whenever too few arguments
 *      are given, since these usages violate the usual SU syntax.
 *      In all cases, selfdoc can be requested by giving only the
 *      program name.
 *
 *      The flag argument distinguishes these cases:
 *              flag = 0; fully defaulted, no stdin
 *              flag = 1; usual case
 *              flag = n > 1; no stdin and n extra args required
 *
 *
 *----------------------------------------------------------------------
 * pagedoc - print self documentation string
 *
 * Returns:
 *      void
 *
 * Synopsis:
 *      void pagedoc()
 *
 * Credits:
 *      SEP: Einar, Stew
 *      CWP: Jack, Shuki
 *
 * Note: Intended to be called by pagedoc(), but conceivably could be
 * used directly as in:
 *      if (xargc != 3) selfdoc();
 */
 
void requestdoc(int flag)
{
        switch(flag) {
        case 1:
                if (xargc == 1 && isatty(STDIN)) pagedoc();
        break;
        case 0:
                if (xargc == 1 && isatty(STDIN) && isatty(STDOUT)) pagedoc();
        break;
        default:
                if (xargc <= flag) pagedoc();
        break;
        }
        return;
}


void pagedoc(void)
{
        extern char *sdoc[];
	char **p = sdoc;
        FILE *fp;

        fflush(stdout);
        fp = popen("more -22 1>&2", "w");
	while(*p) (void)fprintf(fp, "%s\n", *p++);
        pclose(fp);

        exit(EXIT_FAILURE);
}
/*----------------------End of Package--------------------------------*/
