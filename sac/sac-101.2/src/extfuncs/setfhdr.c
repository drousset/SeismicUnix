#include <stdio.h>
#include <stdlib.h>
#include "../../inc/extfunc.h"


void setfhdr( header_in, fieldname, value, error )
sac_header *header_in;
char      *fieldname;
float          value;
long          *error;

{
  long i;

  *error = 0;

  for( i=0; i<MFHDR; i++){
    if(!strcmp(fieldname,float_hdr_fields[i]))break;
  }

  if( i<MFHDR ) header_in->ext_fhdr[i] = value;
  else *error = 1;

  return;


}
