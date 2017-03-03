/** 
 * @file   copy.c
 * 
 * @brief  Copy single precision variables 
 * 
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/** 
 * Copy single precision variable arrays within memory
 * 
 * @param srce 
 *    Source array, elements are copied from here
 * @param sink 
 *    Sink array, elements are copied to here
 * @param ncopy 
 *    Number of elements to copy
 *
 * @note Local Variables 
 *   - LSRCE:   Location of source array in memory. [i]
 *   - LSINK:   Location of sink array in memory. [i]
 *
 * @date   850315:  Now treating arrays as integer not real.
 * @date   841221:  Fixed bug in computing array overlap.
 * @date   831005:  Made independent by adding call to ZMEMAD.
 * @date   800822:  Fixed bug in the logic governing forward/reverse copying.
 * @date   800103:  Original Prime version.
 * @date   850109:  Documented/Reviewed
 *
 */
void 
copy(long int  srce[], 
     long int  sink[], 
     long int  ncopy)
{
	long int j, lsink, lsrce;

	long *const Sink = &sink[0] - 1;
	long *const Srce = &srce[0] - 1;

	/* - Use ZMEMAD to get memory locations of source and sink arrays.
	 *  (These locations are in 16-bit words or
	 *   half a floating point value.) */
	zmemad( &Srce[1], &lsrce );
	zmemad( &Sink[1], &lsink );

	/* - No-op if the source and sink are in same location or
	 *   NCOPY is non-positive. */

	if( lsrce == lsink || ncopy <= 0 )
	{ /* do nothing */ }

	/* - Copy arrays in normal order if the location of the source array
	 *   is in a higher memory location than the sink array OR
	 *   there is no overlap. */
	else if( lsrce > lsink || (lsrce + 2*ncopy) < lsink ){
		for( j = 1; j <= ncopy; j++ ){
			Sink[j] = Srce[j];
		}
	}

	/* - Copy arrays in reverse order if the location of the source array
	 *   is in a lower memory location than the sink array. */
	else{
		for( j = ncopy; j >= 1; j-- ){
			Sink[j] = Srce[j];
		}
	}
}
