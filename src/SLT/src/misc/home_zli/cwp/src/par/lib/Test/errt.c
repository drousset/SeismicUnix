/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

#include "../include/par.h"

main(int argc, char **argv)
{
	initargs(argc, argv);

	warn("%s: Test of warn", __FILE__);
	err("%s: Test of err", __FILE__);

	return EXIT_SUCCESS;
}
