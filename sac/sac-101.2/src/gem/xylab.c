/** 
 * @file   xylab.c
 * 
 * @brief  Control the Y Labeling
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "gem.h"

void 
ylabel_switch(int flag) {
  cmgem.lylab = flag;  /* X Label Flag */
}

void
ylabel_label(char *c) {
  ylabel_switch(TRUE);
  strncpy(kmgem.kylab, c, MCPTXT); /* kmgem.lxlab - Label for the X Axis */
  cmgem.nylab = min(MCPTXT, strlen(c));
}

void
ylabel_location(char *c) {
  int i;
  for(i = 0; i < SAC_LABEL_LOCATIONS; i++) {
    if(strncasecmp(c, kmgem.ksides[i], min(strlen(c), SAC_KEY_SIZE_LENGTH)) == 0) {
      cmgem.iylabp = i + 1;
      ylabel_switch(TRUE);
    }
  }
}


void
ylabel_size(char *c) {
  int i;
  for(i = 0; i < SAC_FONT_SIZES; i++) {
    if(strncasecmp(c, kmgem.ktxsiz[i], min(strlen(c), SAC_KEY_SIZE_LENGTH)) == 0) {
      cmgem.tsylab = cmgem.dtxsiz[i];
    }
  }
}

/** 
 *  Parse the command "ylabel" and control the Y Labeling 
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
xylab(long int *nerr)
{
	long int iylabs;
	float taylab;

	*nerr = 0;

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- Turn y labeling on/off: */
		if( lclog( &cmgem.lylab ) ){

			/* -- Define text of y label: */
			}
		else if( lcquot( MCPTXT, kmgem.kylab,145, &cmgem.nylab ) ){
			cmgem.lylab = TRUE;

			/* -- Set y label size: */
			}
		else if( lklist( "S$",3, (char*)kmgem.ktxsiz,9, MTXSIZ, &iylabs ) ){
			cmgem.tsylab = Txsiz[iylabs];

			/* -- Set location of y label: */
			}
		else if( lklist( "L$",3, (char*)kmgem.ksides,9, 4, &cmgem.iylabp ) ){
			if( cmgem.iylabp == cmgem.itop || cmgem.iylabp == cmgem.ibot ){
				taylab = cmgem.horz;
				}
			else{
				taylab = cmgem.vert;
				}

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;
		}

L_8888:
	return;
}

