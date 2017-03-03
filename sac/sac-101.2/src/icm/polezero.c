/** \file
 * Handle Polezero Subtype Files for transfer
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "complex.h"
#include "proto.h"
#include "hdr.h"
#include "mach.h"

#define	MPOLES	30
#define	MZEROS	30

#define KEY_CONSTANT  "CONSTANT"
#define KEY_ZEROS     "ZEROS"
#define KEY_POLES     "POLES"
#define KEY_STAR      "*"

/** 
 * Handle a Pole Zero file \p subtyp for the transfer command.  Parse and return
 *   the appropriate variables.
 *
 *   Generic transfer function - user supplies poles and zeros
 *        Search for the polezero file.  Search order is:
 *         - (1) current directory.
 *         - (2) global polezero directory. 
 *
 *   Polezero understands Four different Key Words
 *      - CONSTANT real-number
 *          Scaling Value
 *      - ZEROS npoles
 *          Number of Zeros
 *          Location of Complex Zeros follow -->  Real Imaginary
 *      - POLES npoles
 *          Number of Poles
 *          Location of Complex Poles follow -->  Real Imaginary
 *      - * 
 *          Comment, rest of the line is ignored
 *
 * \param nfreq
 *     Input Number of frequencies.  Defined from the maximum number of points in the
 *     entire data file list: npts_max.  The next larger power of 2 is taken
 *     from npts_max to find the the number of points for the FFT: nfft.  Then
 *     nfft is divided by 2 and given one more point. 
 *     \see xtransfer
 * \param delfrq
 *     Input Frequency spacing, defined as 1 / ( nfft * dt ). 
 *     \see transfer
 * \param xre
 *     Output Real Part of the transfer function in the frequency domain.
 *     Length \p nfreq
 * \param xim
 *     Output Imaginary Part of the transer function in the frequency domain.
 *     Length \p nfreq
 * \param subtyp
 *     Input Name of file containing the Pole Zero Response
 * \param subtyp_s
 *     Input Length of string \p subtyp
 * \param nerr
 *     Error return flag
 *          - 0 on Success
 *          - Non-Zero on Error
 *
 * \see xtransfer
 *
 *
 * \date   970129:  Add parameter (0) to cnvatf and cnvati.  0 means that if
 *                  a string of digits is too long, let it slide by.  maf 
 * \date   071003   Documented/Reviewed
 * \date   071003   Fixed a bug with the length of a string key.  Was defined as
 *                  length 9, now automatically determined.
 */


void 
polezero(long int   nfreq, 
	 double     delfrq, 
	 double     xre[], 
	 double     xim[], 
	 char      *subtyp, 
	 int        subtyp_s, 
	 long int  *nerr)
{
	char kfile[MCPFN+1], kiline[MCMSG+1], kmcreq[9];
	char *key;
        char *kline;
	long lexist, lopen, lpoles, lzeros;
	long int i, idx, ic, ic1, ic2, ipoles, itype, izeros, nc, ncerr, 
	 npoles, nzeros, numsave;
        FILE *nun;
	float temp1, temp2;
	double const_ ;
	complexf poles[MPOLES], zeros[MZEROS];
	void zbasename();
        char *s1;


	complexf *const Poles = &poles[0] - 1;
	double *const Xim = &xim[0] - 1;
	double *const Xre = &xre[0] - 1;
	complexf *const Zeros = &zeros[0] - 1;


        for( idx = 0 ; idx < MCPFN ; idx++ )
            kfile[ idx ] = ' ' ;
        kfile[ MCPFN ] = '\0' ;


	/*     generic transfer function - user supplies poles and zeros */
	/* - Search for the polezero file.  Search order is:
	 *   (1) current directory.
	 *   (2) global polezero directory. */
	lopen = FALSE;
	fstrncpy( kfile, MCPFN, subtyp, strlen(subtyp));
	zinquire( kfile, &lexist );
	if( lexist )
		goto L_5000;

	/* Look in global polezero directory 
	   ${SACAUX}/polezeros
	   This code will not work as kmcreq is never set.
	   The idea behind this code is interesing
	   but possibly dangerous through the use of an 
	   uninitilized value.  -BKS
	 */
	/*
	zbasename( kfile,MCPFN+1 );
	crname( kfile,MCPFN+1, KSUBDL, "polezeros",10, nerr );
	if( *nerr != 0 )
		goto L_8888;
	crname( kfile,MCPFN+1, KDIRDL, kmcreq,9, nerr );
	if( *nerr != 0 )
		goto L_8888;
	zinquire( kfile, &lexist );
	if( lexist )
		goto L_5000;
	*/

	/* - Raise error condition if macro file does not exist. */

	*nerr = 108;
	setmsg( "ERROR", *nerr );
	apcmsg( subtyp,subtyp_s );
	goto L_8888;

	/* - Set default values for constant, poles, and zeros. */

L_5000:
	const_ = 1.0;

	for( i = 1; i <= MZEROS; i++ ){
		Zeros[i] = flttocmplx( 0.0, 0.0 );
		}

	for( i = 1; i <= MPOLES; i++ ){
		Poles[i] = flttocmplx( 0.0, 0.0 );
		}

	/* - Open file. */

	zopens( &nun, kfile,MCPFN+1, "ROTEXT",7, nerr );
	if( *nerr != 0 )
		goto L_8888;
	lopen = TRUE;

	/* - Read and decode lines in file. */

	lpoles = FALSE;
	lzeros = FALSE;
L_6000:
        if(fgets(kiline,MCMSG+1,nun)==NULL) {
          if(feof(nun)) goto L_7000;
          goto L_9000;
	}
	
        /* remove leading blanks. */
        kline = kiline;
        while( (*kline == ' ') || (*kline == '\n') || (*kline == '\t') ) kline++;

        if ( (numsave = strlen(kline)) == 0 ) goto L_6000;
        if( kline[numsave-1] == '\n' ) kline[numsave-1] = ' ';
 
        /* eliminate tabs in the input line */
        for ( i = 0; i < numsave; i++) if(kline[i] == '\t') kline[i] = ' ';
	nc = indexb( kline,MCMSG+1 );
	ic = 0;
	poptok( kline, nc, &ic, &ic1, &ic2, &itype );

	/* Make space for the possible KeyValue and truncate the string */
	key = strdup(kline);
	bzero(key, strlen(key));

	modcase( TRUE, kline+ic1 - 1, ic2 - ic1 + 1, key );

	if( strncmp(key, KEY_CONSTANT, strlen(KEY_CONSTANT) ) == 0 ){
		poptok( kline, nc, &ic, &ic1, &ic2, &itype );
                strncpy((s1=malloc(ic2-ic1+2)),kline+ic1 - 1,ic2-ic1+1);
/*                for( idx = 0 ; idx < ic2-ic1+2 ; idx++ )
		    s1[ idx ] = ' ' ;
		s1[ ic2-ic1+2 ] = '\0' ;
*/                s1[ic2-ic1+1] = '\0';

                const_ = atof( s1 ) ;
                if( const_ == 0 || const_ == HUGE_VAL || 
                    const_ == -HUGE_VAL || isnan( const_ ) ){
                        *nerr = 2118 ;
                        setmsg( "ERROR", *nerr ) ;
                        apcmsg( "Unrecognized Constant: ", 24 ) ;
                        apcmsg( s1 , strlen( s1 ) + 1 ) ;
			free(s1) ;
                        goto L_8888 ;
                }
		free(s1) ;
				
	}
	else if( strncmp(key,KEY_POLES, strlen(KEY_POLES)) == 0 ){
		poptok( kline, nc, &ic, &ic1, &ic2, &itype );
                strncpy((s1=malloc(ic2-ic1+2)),kline+ic1 - 1,ic2-ic1+1);
/*                for( idx = 0 ; idx < ic2-ic1+2 ; idx++ )
		    s1[ idx ] = ' ' ;
		s1[ ic2-ic1+2 ] = '\0' ;
*/                s1[ic2-ic1+1] = '\0';
		cnvati( s1, ic2-ic1 + 2, &npoles, 0, nerr ); /* add 0 before nerr. maf 970129 */
		free(s1);
		if( *nerr != 0 )
			goto L_8888;
		if( npoles > MPOLES ){
			*nerr = 2109;
			setmsg( "ERROR", *nerr );
			apcmsg( subtyp,subtyp_s );
			apimsg( MPOLES );
			goto L_8888;
			}
		lpoles = TRUE;
		lzeros = FALSE;
		ipoles = 0;
		}
	else if( strncmp(key,KEY_ZEROS, strlen(KEY_ZEROS)) == 0 ){
		poptok( kline, nc, &ic, &ic1, &ic2, &itype );
                strncpy((s1=malloc(ic2-ic1+2)),kline+ic1 - 1,ic2-ic1+1);
                s1[ic2-ic1+1] = '\0';
		cnvati( s1, ic2-ic1 + 2, &nzeros, 0, nerr ); /* add 0 before nerr. maf 970129 */
		free(s1);
		if( *nerr != 0 )
			goto L_8888;
		if( nzeros > MZEROS ){
			*nerr = 2109;
			setmsg( "ERROR", *nerr );
			apcmsg( subtyp,subtyp_s );
			apimsg( MZEROS );
			goto L_8888;
			}
		lpoles = FALSE;
		lzeros = TRUE;
		izeros = 0;
		}
	else if( strcmp(key,"*       ") == 0 ){
		}
	else if( lpoles ){
		if( ipoles < MPOLES ){
			ipoles = ipoles + 1;
                        strncpy((s1=malloc(ic2-ic1+2)),kline+ic1 - 1,ic2-ic1+1);
                        s1[ic2-ic1+1] = '\0';
			cnvatf( s1, ic2- ic1 + 2, &temp1, 0, nerr ); /* add 0 before nerr. maf 970129 */
			free(s1);
			if( *nerr != 0 )
				goto L_8888;
			poptok( kline, nc, &ic, &ic1, &ic2, &itype );
                        strncpy((s1=malloc(ic2-ic1+2)),kline+ic1 - 1,ic2-ic1+1);
                        s1[ic2-ic1+1] = '\0';
			cnvatf( s1, ic2- ic1 + 2, &temp2, 0, nerr ); /* add 0 before nerr. maf 970129 */
			free(s1);
			if( *nerr != 0 )
				goto L_8888;
			Poles[ipoles] = flttocmplx( temp1, temp2 );
			}
		else{
			*nerr = 2108;
			setmsg( "ERROR", *nerr );
			apcmsg( subtyp,subtyp_s );
			apimsg( MPOLES );
			goto L_8888;
			}
		}
	else if( lzeros ){
		if( izeros < MZEROS ){
			izeros = izeros + 1;
                        strncpy((s1=malloc(ic2-ic1+2)),kline+ic1 - 1,ic2-ic1+1);
                        s1[ic2-ic1+1] = '\0';
			cnvatf( s1, ic2- ic1 + 2, &temp1, 0, nerr ); /* add 0 before nerr. maf 970129 */
			free(s1);
			if( *nerr != 0 )
				goto L_8888;
			poptok( kline, nc, &ic, &ic1, &ic2, &itype );
                        strncpy((s1=malloc(ic2-ic1+2)),kline+ic1 - 1,ic2-ic1+1);
                        s1[ic2-ic1+1] = '\0';
			cnvatf( s1,ic2 - ic1 + 2, &temp2, 0, nerr ); /* add 0 before nerr. maf 970129 */
			free(s1);
			if( *nerr != 0 )
				goto L_8888;
			Zeros[izeros] = flttocmplx( temp1, temp2 );
			}
		else{
			*nerr = 2109;
			setmsg( "ERROR", *nerr );
			apcmsg( subtyp,subtyp_s );
			apimsg( MZEROS );
			goto L_8888;
			}
		}
	else{
		*nerr = 2110;
		setmsg( "ERROR", *nerr );
		apcmsg( subtyp,subtyp_s );
		apcmsg( key,9 );
		goto L_8888;
		}
	free(key);
	goto L_6000;

	/* - Compute transfer function. */

L_7000:
	printf(" Extracting polezero response for %s, %s...\n", kstnm, kcmpnm);

	getran( nfreq, delfrq, const_, nzeros, zeros, npoles, poles, xre, 
	 xim );

L_8888:
	if( lopen )
		zcloses( &nun, &ncerr );
	return;

L_9000:
	*nerr = 114;
	setmsg( "ERROR", *nerr );
	apcmsg( kfile,MCPFN+1 );
	goto L_8888;

}

