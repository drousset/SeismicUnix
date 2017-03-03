#include <stdio.h>
#include <stdlib.h>
#include "../../inc/extfunc.h"


void setehdr( header_in, fieldname, value, error )
sac_header *header_in;
char      *fieldname;
long           value;
long          *error;

{
  long i;

  *error = 0;

  for( i=0; i<MIHDR; i++){
    if(!strcmp(fieldname,enum_hdr_fields[i]))break;
  }

  if( i<MIHDR ) header_in->ext_ihdr[i] = value;
  else *error = 1;

  return;


}
