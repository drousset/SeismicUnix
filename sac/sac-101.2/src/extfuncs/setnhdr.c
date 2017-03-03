#include <stdio.h>
#include <stdlib.h>
#include "../../inc/extfunc.h"


void setnhdr( header_in, fieldname, value, error )
sac_header *header_in;
char      *fieldname;
long           value;
long          *error;

{
  long i;

  *error = 0;

  for( i=0; i<MNHDR; i++){
    if(!strcmp(fieldname,long_hdr_fields[i]))break;
  }

  if( i<MNHDR ) header_in->ext_nhdr[i] = value;
  else *error = 1;

  return;


}
