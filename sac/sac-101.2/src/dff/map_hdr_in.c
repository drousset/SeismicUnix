#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
void /*FUNCTION*/ map_hdr_in(memarray,buffer,lswap)
float *memarray, *buffer;
long lswap ;
{
/*
  This routine maps the SAC file header into memory from
  a buffer containing it in disk file format.  The difference
  between the in-memory format and the disk file format
  is in the storage of the string fields (kmhdr.khdr).
  In the disk file (deriving from the FORTRAN version of
  SAC) these strings are essentially concatenated without
  null termination.  In memory, they are carried about as
  null terminated strings.  This routine picks out the 
  strings and null terminates them, storing them in the
  in memory working storage area.
*/
  char *ptr1, *ptr2;
  int idx;
/* First get the header values for the non character   */
/* fields fhdr, nhdr, ihdr and lhdr.  These are copied */
/* straight across.                                    */

  ptr1 = (char *)memarray;
  ptr2 = (char *)buffer;

  memcpy(ptr1,ptr2,MCMHDR*sizeof(float));

  /* byteswap numeric data if necessary. */
  if( lswap ){
    for( idx = 0 ; idx < MCMHDR ; idx++, ptr1 += 4 )
      byteswap( (void *)ptr1, 4 ) ;
  } /* end if( lswap ) */
  

/* Now copy the character variables into the memory    */
/* buffer, supplying the additional null termination   */
/* character.                                          */

  map_chdr_in(memarray+MCMHDR,buffer+MCMHDR);

  return;

}
