/** 
 * @file   xxlab.c
 * 
 * @brief  Control the X Labeling
 * 
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "gem.h"

void 
xlabel_switch(int flag) {
  cmgem.lxlab = flag;  /* X Label Flag */
}

void
xlabel_label(char *c) {
  xlabel_switch(TRUE);
  strncpy(kmgem.kxlab, c, MCPTXT); /* kmgem.lxlab - Label for the X Axis */
  cmgem.nxlab = min(MCPTXT, strlen(c));
}

void
xlabel_location(char *c) {
  int i;
  for(i = 0; i < SAC_LABEL_LOCATIONS; i++) {
    if(strncasecmp(c, kmgem.ksides[i], min(strlen(c), SAC_KEY_SIZE_LENGTH)) == 0) {
      cmgem.ixlabp = i + 1;
      xlabel_switch(TRUE);
    }
  }
}


void
xlabel_size(char *c) {
  int i;
  for(i = 0; i < SAC_FONT_SIZES; i++) {
    if(strncasecmp(c, kmgem.ktxsiz[i], min(strlen(c), SAC_KEY_SIZE_LENGTH)) == 0) {
      cmgem.tsxlab = cmgem.dtxsiz[i];
    }
  }
}

/** 
 * Parse the command "xlabel" and control the X Labeling
 * 
 * @param nerr 
 *   Error Return Flag
 *   - 0 on Success
 *   - Non-Zero on Error
 *
 * @date   830818:  Changes due to new text size and angle attributes.
 * @date   820924:  Moved LCQUOT to top of parse loop.
 * @date   820614:  Original version.
 *
 */
void 
xxlab(long int *nerr) {
	long int ixlabs;
	float taxlab;

	*nerr = 0;

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- Turn titling on/off: */
		if( lclog( &cmgem.lxlab ) ){

			/* -- Define text of x label: */
			}
		else if( lcquot( MCPTXT, kmgem.kxlab,145, &cmgem.nxlab ) ){
			cmgem.lxlab = TRUE;

			/* -- Set x label size: */
			}
		else if( lklist( "S$",3, (char*)kmgem.ktxsiz,9, MTXSIZ, &ixlabs ) ){
			cmgem.tsxlab = Txsiz[ixlabs];

			/* -- Set location of x label: */
			}
		else if( lklist( "L$",3, (char*)kmgem.ksides,9, 4, &cmgem.ixlabp ) ){
			if( cmgem.ixlabp == cmgem.itop || cmgem.ixlabp == cmgem.ibot ){
				taxlab = cmgem.horz;
				}
			else{
				taxlab = cmgem.vert;
				}

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;
		}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

L_8888:
	return;

}

