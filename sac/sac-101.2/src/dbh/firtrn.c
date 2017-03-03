/** 
 * @file   firtrn.c
 * 
 * @brief  Calculate the transform of a signal with a FIR Filter
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "complex.h"
#include "proto.h"

/** 
 * Calculate the Hilbert Transform or derivative of a signal
 *   with a FIR filter.  Currently uses a 201 point filter 
 *   constructed by windowing the ideal impulse response
 *   with a hamming window.
 * 
 * @param ftype 
 *    Desired Transform Type, character *
 *    - 'HILBERT'    Hilbert Transform
 *    - 'DERIVATIVE' Derivative
 * @param x 
 *    Input array containing signal to be filtered
 * @param n 
 *    Length of array \p x
 * @param buffer 
 *    Work array of length of at least 4297 
 * @param y 
 *    Output array containing transformed signal.  May be the same
 *    array as \p x for in-place calculation.
 *
 * @author  Dave Harris                                                       
 *
 * @date    851220  Last Modified
 * 
 */
void 
firtrn(char     *ftype, 
       float     x[], 
       long int  n, 
       float     buffer[], 
       float     y[])
{
	long int i, i_, ihlfsz, iptrb1, iptrb2, iptrf, iqrtsz;
	float c, pi, twopi;

	float *const Buffer = &buffer[0] - 1;
	float *const X = &x[0] - 1;
	float *const Y = &y[0] - 1;

	pi = 3.14159265;
	twopi = 2.*pi;
	iptrf = 1;
	iptrb1 = 201 + 1;
	iptrb2 = iptrb1 + 2*1024;
	ihlfsz = 201/2;
	iqrtsz = 201/4;

	/* Set up filter coefficients                                                    
	 * */
	zero( &Buffer[iptrf], 201 );
	if( memcmp(ftype,"HILBERT",7) == 0 ){
		for( i = 1; i <= iqrtsz; i++ ){
			i_ = i - 1;
			c = (2./(pi*(float)( 2*i - 1 )))*(.54 + .46*cos( twopi*
			 (float)( 2*i - 1 )/(float)( 201 ) ));
			Buffer[ihlfsz + 1 + (2*i - 1)] = c;
			Buffer[ihlfsz + 1 - (2*i - 1)] = -c;
			}

		}
	else if( memcmp(ftype,"DERIVATIVE",10) == 0 ){

		for( i = 1; i <= ihlfsz; i++ ){
			i_ = i - 1;
			c = (cos( pi*(float)( i ) )/(float)( i ))*(.54 + .46*cos( pi*
			 (float)( i )/(float)( ihlfsz ) ));
			Buffer[ihlfsz + 1 + i] = c;
			Buffer[ihlfsz + 1 - i] = -c;
			}

		}


	/* Filtering operation with overlap-save to implement transform                  
	 * */
	overlp( x, n, y, &Buffer[iptrf], 201, 1024, &Buffer[iptrb1], &Buffer[iptrb2] );

	/* Shift data to account for filtering delay                                     
	 * */
	zshft( y, n, -ihlfsz );

	/* Done                                                                          
	 * */
	return;
}

