#include <stdio.h>
#include <stdlib.h>
#include "../../inc/extfunc.h"

char *getahdr( header_in, fieldname, error )
sac_header *header_in;
char      *fieldname;
long          *error;

{
  long i;

  *error = 0;

  for( i=0; i<MKHDR; i++){
    if(!strcmp(fieldname,char_hdr_fields[i]))break;
  }

  if( i<MKHDR ) return(&(header_in->ext_khdr[i][0]));
  else {
    *error = 1;
    return ((char *)NULL);
  }

}
