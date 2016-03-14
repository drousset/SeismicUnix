/* TABPLOT: $Revision: 1.0 $ ; $Date: 2004/05/27 20:26:03 $	*/

/*----------------------------------------------------------------------
 * Copyright (c) Colorado School of Mines, 1989.
 * All rights reserved.
 *
 * This code is part of SU.  SU stands for Seismic Unix, a processing line
 * developed at the Colorado School of Mines, partially based on Stanford
 * Exploration Project (SEP) software.  Inquiries should be addressed to:
 *
 *  Jack K. Cohen, Center for Wave Phenomena, Colorado School of Mines,
 *  Golden, CO 80401  (isis!csm9a!jkcohen)
 *----------------------------------------------------------------------
 */

#include "su.h"
#include "segy.h"

/* tabplot - tabplot selected sample points on selected trace
 *
 * Returns:
 *	void
 *
 * Synopsis:
 *	void tabplot(tp, itmin, itmax)
 *	segy *tp;
 *	int itmin, itmax;
 *
 * Credits:
 *	CWP: Brian, Jack
 *
 *
 */


#define SCREENFUL	19
#define PLOTWIDTH	29

string str[] = {
	"-----------------------------|",
	" ----------------------------|",
	"  ---------------------------|",
	"   --------------------------|",
	"    -------------------------|",
	"     ------------------------|",
	"      -----------------------|",
	"       ----------------------|",
	"        ---------------------|",
	"         --------------------|",
	"          -------------------|",
	"           ------------------|",
	"            -----------------|",
	"             ----------------|",
	"              ---------------|",
	"               --------------|",
	"                -------------|",
	"                 ------------|",
	"                  -----------|",
	"                   ----------|",
	"                    ---------|",
	"                     --------|",
	"                      -------|",
	"                       ------|",
	"                        -----|",
	"                         ----|",
	"                          ---|",
	"                           --|",
	"                            -|",
      	"                             *",
      	"                             |+",
      	"                             |++",
      	"                             |+++",
      	"                             |++++",
      	"                             |+++++",
      	"                             |++++++",
      	"                             |+++++++",
      	"                             |++++++++",
      	"                             |+++++++++",
      	"                             |++++++++++",
      	"                             |+++++++++++",
      	"                             |++++++++++++",
      	"                             |+++++++++++++",
      	"                             |++++++++++++++",
      	"                             |+++++++++++++++",
      	"                             |++++++++++++++++",
      	"                             |+++++++++++++++++",
      	"                             |++++++++++++++++++",
      	"                             |+++++++++++++++++++",
      	"                             |++++++++++++++++++++",
      	"                             |+++++++++++++++++++++",
      	"                             |++++++++++++++++++++++",
      	"                             |+++++++++++++++++++++++",
      	"                             |++++++++++++++++++++++++",
      	"                             |+++++++++++++++++++++++++",
      	"                             |++++++++++++++++++++++++++",
      	"                             |+++++++++++++++++++++++++++",
      	"                             |++++++++++++++++++++++++++++",
      	"                             |+++++++++++++++++++++++++++++",
};


void tabplot(segy *tp, int itmin, int itmax)
{
	float amp;	/* largest abs(datum) in window		*/
	float val;	/* temp for data value			*/
	int plt;	/* scaled data value			*/
	int i;		/* counter				*/

	amp = ABS(tp->data[itmin]);
	for (i = itmin + 1; i <= itmax; i++) {
		amp = MAX(amp, ABS(tp->data[i]));
	}

	if (amp == 0.0) { /* data all zeroes, plot zero string */
		for (i = itmin; i <= itmax; i++) {
			val = 0.0;
			printf("%5d %11.4e%s\n", i + 1, val, str[PLOTWIDTH]);
		}
	} else { /* usual case, plot scaled data */
		for (i = itmin; i <= itmax; i++) {
			val = tp->data[i];
			plt = PLOTWIDTH * (val/amp + 1.0);
			printf("%5d %11.4e%s\n", i + 1, val, str[plt]);
		}
	}

	return;
}
