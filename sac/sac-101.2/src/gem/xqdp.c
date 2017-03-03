/** 
 * @file   xqdp.c
 * 
 * @brief  Control Quick and Dirty Plot Option
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
qdp_switch(int flag) {
  cmgem.ltqdp = flag; /* Terminal */
  cmgem.lfqdp = flag; /* SGF      */
}

void
qdp_points(int n) {
  qdp_switch(TRUE);
  cmgem.ntqdp = n; /* Terminal */
  cmgem.nfqdp = n; /* SGF      */
}

/** 
 * Parse the parameter setting command "QDP"
 *    Controls the "quick and dirty plot" option
 * 
 * @param nerr 
 *    Error return flag
 *    - 0 on Success
 *    - Non-Zero on Error
 *
 * @date   850219:  Original version.
 *
 */
void 
xqdp(long int *nerr) {
  long ltemp;
  long int itemp;

  *nerr = 0;

  /* - Loop on each token in command: */
  
 L_1000:
  if( lcmore( nerr ) ){
    
    /* -- "ON/OFF/n":  change both terminal and SGF QDP parameters. */
    if( lclog( &ltemp ) ){
      qdp_switch(ltemp);
    }
    else if( lcint( &itemp ) ){
      qdp_switch(TRUE);
      cmgem.ntqdp = itemp;
      cmgem.nfqdp = itemp;
      
      /* -- "TERM ON/OFF/n":  change terminal QDP parameters only. */
    }
    else if( lklogi( "TERM$",6, &cmgem.ltqdp, &cmgem.ntqdp ) ){
      
      /* -- "SGF ON/OFF/n":  change SGF QDP parameters only. */
    }
    else if( lklogi( "SGF$",5, &cmgem.lfqdp, &cmgem.nfqdp ) ){
      
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

