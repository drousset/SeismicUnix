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

#include "par.h"

/* initargs - make command line args available to subroutines
 *
 * Synopsis:
 *	void initargs(argc, argv);
 *
 * Credit:
 *	CWP: Shuki, Jack
 *
 *
 */



void initargs(int argc, char **argv)
{
	xargc = argc; xargv = argv;
	return;
}
