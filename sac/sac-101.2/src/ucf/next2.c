/** 
 * @file   next2.c
 * 
 * @brief  Get the next power of 2
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "complex.h"
#include "proto.h"

#define  STARTING_POWER_OF_TWO   8

/** 
 * Get the next power of two just greater than \p num
 * 
 * @param num 
 *    Input number 
 * 
 * @return 
 *    Power of two greater than \p num
 *
 * @date    810000:  Original version.
 *
 */
long int 
next2(long int num) {
  long int next2_v;
  
  next2_v = STARTING_POWER_OF_TWO;
    
  while(next2_v < num) {
    next2_v *= 2;
  }
  
  return( next2_v );
}

