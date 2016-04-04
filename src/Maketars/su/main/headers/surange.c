/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SURANGE: $Revision: 1.18 $ ; $Date: 2011/11/16 22:10:29 $  */

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SURANGE - get max and min values for non-zero header entries	",
" 								",
" surange <stdin	 					",
"								",
" Optional parameters:						",
"	key=		Header key(s) to range (default=all)	",
" 								",
" Note: Gives partial results if interrupted			",
" 								",
" Output is: 							",
" number of traces 						",
" keyword min max (first - last) 				",
" 								",
NULL};

/* Credits:
 *	Geocon: Garry Perratt (output one header per line;
 *		option to specify headers to range;
 *		added first & last values where min<max)
 *	Based upon original by:
 *		SEP: Stew Levin
 *		CWP: Jack K. Cohen
 *
 * Note: the use of "signal" is inherited from BSD days and may
 *       break on some UNIXs.  It is dicy in that the responsibility
 *	 for program termination is lateraled back to the main.
 *
 */
/**************** end self doc ***********************************/


/* Prototypes */
void printrange(segy *tpmin, segy *tpmax, segy *tpfirst, segy *tplast);
static void closeinput(void);

segy tr, trmin, trmax, trfirst, trlast;

int
main(int argc, char **argv)
{
	int ntr;			/* number of traces		*/
	int nkeys=0;			/* number of keywords to range	*/
	Value val;			/* value of current keyword	*/
	Value valmin;			/* smallest seen so far		*/
	Value valmax;			/* largest seen so far		*/
	cwp_String type;		/* data type of keyword		*/
	cwp_String key[SU_NKEYS];	/* array of keywords		*/


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get "key" value */
	if ((nkeys=countparval("key"))!=0) {
		getparstringarray("key",key);
	}

        checkpars();

	/* Zero out values of trmin and trmax */
	memset((void *) &trmin, 0, sizeof(segy));
	memset( (void *) &trmax, 0, sizeof(segy));

	/* Set up closing commands */
	signal(SIGINT, (void (*) (int)) closeinput);
	signal(SIGTERM, (void (*) (int)) closeinput);

	/* Do first trace outside loop to initialize mins and maxs */
	if (!gettr(&tr)) err("can't get first trace");
	{	register int i;
		if (nkeys==0) {
   			for (i = 0; i < SU_NKEYS; ++i) {
 				gethval(&tr, i, &val);
 				puthval(&trmin, i, &val);
 				puthval(&trmax, i, &val);
 				puthval(&trfirst, i, &val);
			}
		} else	{
			register int j;
			for (i=0;i<nkeys;i++) {
				j = getindex(key[i]);
 				gethval(&tr, j, &val);
 				puthval(&trmin, j, &val);
 				puthval(&trmax, j, &val);
 				puthval(&trfirst, j, &val);
			}
		}
	}

	ntr = 1;
	while (gettr(&tr)) {
		register int i;
		if (nkeys==0) {
	       		for (i = 0; i < SU_NKEYS; ++i) {
				type = hdtype(getkey(i));
				gethval(&tr, i, &val);
				gethval(&trmin, i, &valmin);
				gethval(&trmax, i, &valmax);
				if (valcmp(type, val, valmin) < 0)
					puthval(&trmin, i, &val);
				if (valcmp(type, val, valmax) > 0)
					puthval(&trmax, i, &val);
 				puthval(&trlast, i, &val);
			}
		} else	{
			register int j;
			for (i=0;i<nkeys;i++) {
				type = hdtype(key[i]);
				j = getindex(key[i]);
				gethval(&tr, j, &val);
				gethval(&trmin, j, &valmin);
				gethval(&trmax, j, &valmax);
				if (valcmp(type, val, valmin) < 0)
					puthval(&trmin, j, &val);
				if (valcmp(type, val, valmax) > 0)
					puthval(&trmax, j, &val);
 				puthval(&trlast, j, &val);

			}
		}
		++ntr;
	}

	printf("%d traces:\n",ntr);
	printrange(&trmin, &trmax, &trfirst, &trlast);


	return(CWP_Exit());
}



/* printrange - print non-zero header values ranges	*/
void printrange(segy *tpmin, segy *tpmax, segy *tpfirst, segy *tplast)
{
	register int i = 0;
	Value valmin, valmax, valfirst, vallast;
	double dvalmin, dvalmax, dvalfirst, dvallast;
	cwp_String key;
	cwp_String type;
	int kmin = 0, kmax=SU_NKEYS;

	for (i = kmin; i < kmax; ++i) {
		key = getkey(i);
		type = hdtype(key);
		gethval(tpmin, i, &valmin);
		gethval(tpmax, i, &valmax);
		gethval(tpfirst, i, &valfirst);
		gethval(tplast, i, &vallast);
		dvalmin = vtod(type, valmin);
		dvalmax = vtod(type, valmax);
		dvalfirst = vtod(type, valfirst);
		dvallast = vtod(type, vallast);
		if (dvalmin || dvalmax) {
			if (dvalmin < dvalmax) {
				printf("%-8s ", key);
				printfval(type, valmin);
				printf(" ");
				printfval(type, valmax);
				printf(" (");
				printfval(type, valfirst);
				printf(" - ");
				printfval(type, vallast);
				printf(")");
			} else {
				printf("%-8s ", key);
				printfval(type, valmin);
			}
			putchar('\n');
		}
	}
	return;
}


static void closeinput(void) /* for graceful interrupt termination */
{
	/* Close stdin and open /dev/null in its place.  Now we are reading */
	/* from an empty file and the loops terminate in a normal fashion.  */

	efreopen("/dev/null", "r", stdin);
}
