#include <string.h>

#include "../../inc/extfunc.h"

void fsetahdr_(hdr_index, fieldname, value, error, lenfield, lenvalue)
long *hdr_index;
char *fieldname;
char     *value;
long     *error;
long   lenfield;
long   lenvalue;
{
  char infield[8];
  char invalue[17];
  sac_header *hdr_in;

  hdr_in = indata->ext_hdrs[*hdr_index-1];

  memset(infield,' ',8);
  infield[8]='\0';
  memset(invalue,' ',17);
  invalue[17]='\0';

  getfield(fieldname, lenfield, infield);
  getfield(value, lenvalue, invalue);

  setahdr(hdr_in, infield, invalue, error);

  return;
  
}
