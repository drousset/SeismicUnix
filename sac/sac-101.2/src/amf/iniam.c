/** 
 * @file   iniam.c
 * 
 * @brief  Initialize the Array Manager
 * 
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"

/** \def MEMINIT
 *  Initial number of blocks allocated 
 */
#define MEMINIT 100

/** 
 * A Memory Manager initializer.
 * 
 * If the memory block has not been allocated, 
 * then the Memory structure is allocated with a value of MEMINIT
 *
 * If the memory block has alredy been allocated with some value, Not Zero, 
 * then All elements of the Memory structure is deallocted 
 * 
 * \param *memstruct
 *    Memory structure begin initialized or free'd
 *
 * \return  Nothing
 * \see     iniam allamb relamb reaamb
 *
 * \date   940127:  Original version.
 * \date   070606: Documented/Reviewed
 */
void 
iniam(struct t_cmmem *memstruct)
{
        long i;

        if ( memstruct->nallocated != 0 ){
	  /* not the first time in.  
	     release previously allocated memory and reinitialize 
	  */

          for (i=0; i<memstruct->nallocated; i++){
                  free(memstruct->sacmem[i]);
                  memstruct->sacmem[i] = NULL;
		}
	}
        else {
	  /* first time in. 
	     initialize the sacmem storage area.  
	     allocate block of pointers 
	  */
          if((memstruct->sacmem =(float **)malloc(MEMINIT*sizeof(float *))) == NULL){
            printf("Error allocating initial memory-iniam\n  quitting\n");
            exit(1);
	  }
          memstruct->nallocated = MEMINIT;
          for (i=0; i<MEMINIT; i++){
            memstruct->sacmem[i] = NULL;
	  }
	}

	return;
}

