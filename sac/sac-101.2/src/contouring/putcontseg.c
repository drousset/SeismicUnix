#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/mem.h"
#include "../../inc/contouring.h"
void /*FUNCTION*/ putcontseg(number, level, start, stop)
long int number, level, start, stop;
{

        long *Isacmem;

	/*=====================================================================
	 * PURPOSE:  To put (store) information about an existing contouring
	 *           line segment.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    number:  The segment number. [i]
	 *    level:   The contour level index number. [i]
	 *    start:   The index to the starting point of the segment. [i]
	 *    stop:    The index to the stopping point of the segment. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  contouring/5
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    contouring:  numsegments, indexlevels, indexstarts, indexstops
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    mem:         sacmem, isacmem
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900315:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900315
	 *===================================================================== */
	/* PROCEDURE: */
	if( number <= cmcontouring.numsegments ){
                Isacmem = (long*)cmmem.sacmem[cmcontouring.indexlevels];
		*(Isacmem + number - 1) = level;

                Isacmem = (long*)cmmem.sacmem[cmcontouring.indexstarts];
		*(Isacmem + number - 1) = start;

                Isacmem = (long*)cmmem.sacmem[cmcontouring.indexstops];
		*(Isacmem + number - 1) = stop;
		}
	else{
		fprintf( stdout, "Illegal segment number: %ld \n", number );
		}

L_8888:
	return;

} /* end of function */

