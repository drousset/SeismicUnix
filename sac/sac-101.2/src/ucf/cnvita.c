/** 
 * @file   cnvita.c
 * 
 * @brief  Convert an integer to a string
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

/** 
 * Convert an integer \p intgr into its ASCII equivalent
 * 
 * @param intgr 
 *    Integer to convert into a string
 * @param kintgr 
 *    Returned string
 *     If number cannot be converted then string is set to 'BADINPUT'
 * @param kintgr_s 
 *    Length of \p kintgr
 *
 * @date   800102:  Original version.
 *
 */
void 
cnvita(long int  intgr, 
       char     *kintgr, 
       int       kintgr_s)
{
	char kfmt[9];
	long int ncf, nck;
	void *_p0;
        char *s1;

	/* - Determine length of character variable. */
	nck = (kintgr_s - 1);

	/* - Create format statement. */

	strcpy( kfmt, "%" );
        ncf = 1;
	if( nck <= 9 ){
                sprintf(kfmt+ncf,"%1ld",nck);
                ncf++;
                		}
	else if( nck <= 99 ){
                sprintf(kfmt+ncf,"%2ld",nck);
                ncf += 2;
		}
	else{
                sprintf(kfmt+ncf,"%3ld",nck);
                ncf += 3;
		}
        strcpy(kfmt+ncf,"ld");

	/* - Encode integer into string. */

        if( sprintf(kintgr,kfmt,intgr) < 0 ) {
           fstrncpy(kintgr, kintgr_s - 1,"BADINPUT",8);
	}
	return;
} 

