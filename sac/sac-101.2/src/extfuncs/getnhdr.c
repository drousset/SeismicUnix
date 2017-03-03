#include <stdio.h>
#include <stdlib.h>
#include "../../inc/extfunc.h"


long getnhdr( header_in, fieldname, error )
sac_header *header_in;
char      *fieldname;
long          *error;

{
  long i;

  *error = 0;

  for( i=0; i<MNHDR; i++){
    if(!strcmp(fieldname,long_hdr_fields[i]))break;
  }

  if( i<MNHDR ) return(header_in->ext_nhdr[i]);
  else {
    *error = 1;
    return (NUNDEF);
  }

}
