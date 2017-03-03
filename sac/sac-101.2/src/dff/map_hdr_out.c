#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
void /*FUNCTION*/ map_hdr_out(memarray,buffer, lswap)
float *memarray, *buffer;
int lswap;
{
/*
  This routine maps the in-memory SAC header to the file
  header format.  See map_header_in for details of the
  format differences.
*/
  char *ptr1, *ptr2;
  int i;
/* First get the header values for the non character   */
/* fields fhdr, nhdr, ihdr and lhdr.  These are copied */
/* straight across.                                    */

  ptr1 = (char *)memarray;
  ptr2 = (char *)buffer;

  memcpy(ptr2,ptr1,MCMHDR*sizeof(float));

  if(lswap == TRUE) {
    for(i = 0; i < MCMHDR; i++, ptr2 += 4) {
      byteswap((void *) ptr2, 4);
    }
  }
  
/* Now copy the character variables into the output    */
/* buffer, eliminating the null termination            */
/* character.                                          */

  map_chdr_out(memarray+MCMHDR,buffer+MCMHDR);

  return;

}
