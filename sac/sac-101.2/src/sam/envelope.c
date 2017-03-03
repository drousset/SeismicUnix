/** 
 * @file   envelope.c
 * 
 * @brief  Envelope Function
 * 
 */

#include <stdio.h>
#include <stdlib.h>

#include "complex.h"
#include "proto.h"

/** 
 * Envelope Function using the Hilbert transform
 * 
 * @param n 
 *    Number of points in the signals \p in and \p out
 * @param in 
 *    Input signal of length \p n
 * @param out 
 *    Output signal of length \p n
 *
 * @date June 30, 2008 Original Version, B. Savage
 *
 */
void
envelope(int n, float *in, float *out) {
  int i;
  int n2;
  float *scratch;
  
  scratch = (float *)malloc(sizeof(float) * ( 5 * 1024 ) );
  if(scratch == NULL) {
    fprintf(stderr, 
	    "Error allocating space for hilbert transform: npts %ld\n", 
	    n2);
    return;
  }

  firtrn("HILBERT",  in, n, scratch, out);

  for(i = 0; i < n; i++) {
    out[i] = sqrt(in[i] * in[i] + out[i] * out[i]);
  }

  free(scratch);
  return;
}

/** 
 * Envelope Function
 *   Fortran Interface
 * 
 * @see envelope
 */
void
envelope_(int *n, float *in, float *out) {
  envelope(*n, in, out);
}

/** 
 * Envelope Function
 *   Fortran Interface
 * 
 * @see envelope
 */
void
envelope__(int *n, float *in, float *out) {
  envelope(*n, in, out);
}
