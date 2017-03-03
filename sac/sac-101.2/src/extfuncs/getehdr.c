#include <stdio.h>
#include <stdlib.h>
#include "../../inc/extfunc.h"


long getehdr( header_in, fieldname, error )
sac_header *header_in;
char      *fieldname;
long          *error;

{
  long i;

  *error = 0;

  for( i=0; i<MIHDR; i++){
    if(!strcmp(fieldname,enum_hdr_fields[i]))break;
  }

  if( i<MIHDR ) return(header_in->ext_ihdr[i]);
  else {
    *error = 1;
    return (IUNDEF);
  }

}
