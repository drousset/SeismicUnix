/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

#include "par.h"

/* Exercise conversion routines */
main(int argc, char **argv)
{
	char s[BUFSIZ];
	int ni;
	long nl;
	unsigned long nv;
	double nd;


	initargs(argc, argv);


	/* Test code for estrtod */
	strcpy(s, "0.0");
	nd = estrtod(s, (char**) NULL);
	warn("estrtod(%s) = %e\n", s, nd);

	strcpy(s, "1.0e+37");
	nd = estrtod(s, (char**) NULL);
	warn("estrtod(%s) = %e\n", s, nd);

	strcpy(s, "-1.0e+37");
	nd = estrtod(s, (char**) NULL);
	warn("estrtod(%s) = %e\n", s, nd);

	strcpy(s, "1.0e+309");
	nd = estrtod(s, (char**) NULL);
	warn("estrtod(%s) = %e\n", s, nd);

	strcpy(s, "-1.0e+309");
	nd = estrtod(s, (char**) NULL);
	warn("estrtod(%s) = %e\n", s, nd);


	/* Test code for eatof */
	strcpy(s, "0.0");
	nd = eatof(s);
	warn("eatof(%s) = %e\n", s, nd);

	strcpy(s, "1.0e+37");
	nd = eatof(s);
	warn("eatof(%s) = %e\n", s, nd);

	strcpy(s, "-1.0e+37");
	nd = eatof(s);
	warn("eatof(%s) = %e\n", s, nd);


	/* Test code for estrtol */
	strcpy(s, "0");
	nl = estrtol(s, (char**) NULL, 10);
	warn("estrtol(%s, (char**) NULL, 10) = %ld\n", s, nl);

	strcpy(s, "2147483647");
	nl = estrtol(s, (char**) NULL, 10);
	warn("estrtol(%s, (char**) NULL, 10) = %ld\n", s, nl);

	strcpy(s, "-2147483648");
	nl = estrtol(s, (char**) NULL, 10);
	warn("estrtol(%s, (char**) NULL, 10) = %ld\n", s, nl);

	strcpy(s, "2147483648");
	nl = estrtol(s, (char**) NULL, 10);
	warn("estrtol(%s, (char**) NULL, 10) = %ld\n", s, nl);


	/* Test code for eatoi */
	strcpy(s, "0");
	ni = eatoi(s);
	warn("eatoi(%s) = %d\n", s, ni);

	strcpy(s, "2147483647");
	ni = eatoi(s);
	warn("eatoi(%s) = %d\n", s, ni);

	strcpy(s, "-2147483648");
	ni = eatoi(s);
	warn("eatoi(%s) = %d\n", s, ni);


	/* Test code for eatol */
	strcpy(s, "0");
	nl = eatol(s);
	warn("eatol(%s) = %ld\n", s, nl);

	strcpy(s, "2147483647");
	nl = eatol(s);
	warn("eatol(%s) = %ld\n", s, nl);

	strcpy(s, "-2147483648");
	nl = eatol(s);
	warn("eatol(%s) = %ld\n", s, nl);


	/* Test code for estrtoul */
	strcpy(s, "0");
	nv = estrtoul(s, (char**) NULL, 10);
	warn("estrtoul(%s, (char**) NULL, 10) = %u\n", s, nv);

	strcpy(s, "4294967295");
	nv = estrtoul(s, (char**) NULL, 10);
	warn("estrtoul(%s, (char**) NULL, 10) = %u\n", s, nv);

	return EXIT_SUCCESS;
}
