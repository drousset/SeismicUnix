#include <stdio.h>
#include <stdlib.h>
#include "../../inc/extfunc.h"


void setlhdr( header_in, fieldname, value, error )
sac_header *header_in;
char      *fieldname;
long           value;
long          *error;

{
  long i;

  *error = 0;

  for( i=0; i<MLHDR; i++){
    if(!strcmp(fieldname,log_hdr_fields[i]))break;
  }

  if( i<MLHDR ) header_in->ext_lhdr[i] = value;
  else *error = 1;

  return;


}
