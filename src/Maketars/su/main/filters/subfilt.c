/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUBFILT: $Revision: 1.21 $ ; $Date: 2011/11/12 00:09:00 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUBFILT - apply Butterworth bandpass filter 			",
" 								",
" subfilt <stdin >stdout [optional parameters]			",
" 							        ",
" Required parameters:						",
" 	if dt is not set in header, then dt is mandatory	",
" 							        ",
" Optional parameters: (nyquist calculated internally)		",
" 	zerophase=1		=0 for minimum phase filter 	",
" 	locut=1			=0 for no low cut filter 	",
" 	hicut=1			=0 for no high cut filter 	",
" 	fstoplo=0.10*(nyq)	freq(Hz) in low cut stop band	",
" 	astoplo=0.05		upper bound on amp at fstoplo 	",
" 	fpasslo=0.15*(nyq)	freq(Hz) in low cut pass band	",
" 	apasslo=0.95		lower bound on amp at fpasslo 	",
" 	fpasshi=0.40*(nyq)	freq(Hz) in high cut pass band	",
" 	apasshi=0.95		lower bound on amp at fpasshi 	",
" 	fstophi=0.55*(nyq)	freq(Hz) in high cut stop band	",
" 	astophi=0.05		upper bound on amp at fstophi 	",
" 	verbose=0		=1 for filter design info 	",
" 	dt = (from header)	time sampling interval (sec)	",
" 							        ",
NULL};

/* Credits:
 *	CWP: Dave for bf.c subs and test drivers
 *	CWP: Jack for su wrapper
 *
 * Caveat: zerophase will not do good if trace has a spike near
 *	   the end.  One could make a try at getting the "effective"
 *	   length of the causal filter, but padding the traces seems
 *	   painful in an already expensive algorithm.
 *
 * Trace header fields accessed: ns, dt, trid
 */
/**************** end self doc ***********************************/



segy tr;

int
main(int argc, char **argv)
{
	int zerophase;		/* flag for zero phase filtering	*/
	int locut;		/* flag for low cut filtering		*/
	int hicut;		/* flag for high cut filtering		*/
	float fstoplo;		/* left lower corner frequency		*/
	float fpasslo;		/* left upper corner frequency		*/
	float fpasshi;		/* right lower corner frequency		*/
	float fstophi;		/* right upper corner frequency		*/
	float astoplo;		/* amp at fstoplo			*/
	float apasslo;		/* amp at fpasslo			*/
	float apasshi;		/* amp at fpasshi			*/
	float astophi;		/* amp at fstophi			*/
	int npoleslo;		/* poles in low cut filter		*/
	int npoleshi;		/* poles in high cut filter		*/
	float f3dblo;		/* 3 db point of low cut filter		*/
	float f3dbhi;		/* 3 db point of high cut filter	*/
	float dt;		/* sample spacing			*/
	float nyq;		/* nyquist frequency			*/
	int nt;			/* number of points on input trace	*/
	int verbose;		/* design info flag 			*/
	cwp_Bool seismic;	/* is this seismic data?		*/

	
	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get info from first trace */ 
	if (!gettr(&tr))  err("can't get first trace");
	seismic = ISSEISMIC(tr.trid); 
		 
	if (!seismic)
		warn("input is not seismic data, trid=%d", tr.trid);
	nt = tr.ns;
	if (!getparfloat("dt", &dt))	dt = ((double) tr.dt)/1000000.0;
	if (!dt) err("dt field is zero and not getparred");
	nyq = 0.5/dt;


	/* Get design frequencies and amplitudes */
	if (!getparint("verbose", &verbose))	verbose = 0;
	if (!getparint("zerophase", &zerophase)) zerophase = 1;
	if (!getparint("locut", &locut))	locut = 1;
	if (!getparint("hicut", &hicut))	hicut = 1;
	if (!getparfloat("fstoplo", &fstoplo))	fstoplo = .10 * nyq;
	if (!getparfloat("fpasslo", &fpasslo))	fpasslo = .15 * nyq;
	if (!getparfloat("fpasshi", &fpasshi))	fpasshi = .40 * nyq;
	if (!getparfloat("fstophi", &fstophi))	fstophi = .55 * nyq;
	if (locut) {
		if (fstoplo <= 0.0)      err("fstoplo must be positive");
		if (fstoplo > fpasslo)  err("fstoplo must be < fpasslo");
	}
	if (hicut) {
		if (fpasshi > fstophi)  err("fpasshi must be < fstophi");
		if (fstophi > nyq)  err("fstophi must be < nyquist (%f)", nyq);
	}
	if (!getparfloat("astoplo", &astoplo))	astoplo = .05;
	if (!getparfloat("apasslo", &apasslo))	apasslo = .95;
	if (!getparfloat("apasshi", &apasshi))	apasshi = .95;
	if (!getparfloat("astophi", &astophi))	astophi = .05;
	if (astoplo > apasslo || apasshi < astophi)
		err("Bad amplitude parameters");
		
		
	/* Normalize frequencies to [0, 0.5] for bfdesign */
	fstoplo *= dt;
	fpasslo *= dt;
	fstophi *= dt;
	fpasshi *= dt;
	
	
	/* Adapt user frequencies if zerophase selected */
	if (zerophase) {	
		astoplo = sqrt(astoplo);
		apasslo = sqrt(apasslo);
		astophi = sqrt(astophi);
		apasshi = sqrt(apasshi);
	}

	
	/* Use bdesign to make low and high cut filters */
	if (locut) bfdesign(fpasslo,apasslo,fstoplo,astoplo,&npoleslo,&f3dblo);
	if (hicut) bfdesign(fpasshi,apasshi,fstophi,astophi,&npoleshi,&f3dbhi);


	/* Give verbose info if requested */
	if (verbose && locut) {
		if (zerophase) {
			warn("low-cut filter: npoles = %d, 3db point = %f(Hz)",
				2*npoleslo, f3dblo/dt);
		} else {
			warn("low-cut filter: npoles = %d, 3db point = %f(Hz)",
				npoleslo, f3dblo/dt);
		}
	}
	if (verbose && hicut) {
		if (zerophase) {
			warn("high-cut filter: npoles = %d, 3db point = %f(Hz)",
				2*npoleshi, f3dbhi/dt);
		} else {
			warn("high-cut filter: npoles = %d, 3db point = %f(Hz)",
				npoleshi, f3dbhi/dt);

		}
	}

	/* Main loop over traces */
	do {
		/* low-cut (high pass) filter */
		if (locut) {
		    bfhighpass(npoleslo,f3dblo,nt,tr.data,tr.data);
		    if (zerophase) {
			register int i;
		        for (i=0; i<nt/2; ++i) { /* reverse trace in place */
				register float tmp = tr.data[i];
				tr.data[i] = tr.data[nt-1 - i];
				tr.data[nt-1 - i] = tmp;
			}
		        bfhighpass(npoleslo,f3dblo,nt,tr.data,tr.data);
		        for (i=0; i<nt/2; ++i) { /* flip trace back */
				register float tmp = tr.data[i];
				tr.data[i] = tr.data[nt-1 - i];
				tr.data[nt-1 - i] = tmp;
			}
		    }
		}

		/* high-cut (low pass) filter */
		if (hicut) {
		    bflowpass(npoleshi,f3dbhi,nt,tr.data,tr.data);
		    if (zerophase) {
			register int i;
			for (i=0; i<nt/2; ++i) { /* reverse trace */
				register float tmp = tr.data[i];
				tr.data[i] = tr.data[nt-1 - i];
				tr.data[nt-1 - i] = tmp;
			}
			bflowpass(npoleshi,f3dbhi,nt,tr.data,tr.data);
		        for (i=0; i<nt/2; ++i) { /* flip trace back */
				register float tmp = tr.data[i];
				tr.data[i] = tr.data[nt-1 - i];
				tr.data[nt-1 - i] = tmp;
			}
		    }
		}
		
		puttr(&tr);
	} while (gettr(&tr));

	return(CWP_Exit());
}
