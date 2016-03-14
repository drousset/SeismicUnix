
#include "su.h"
#include "segy.h"
#include "header.h"

/* auxgethdr - get segy tape identification headers from
 *	       the auxiliary file by file pointer
 *
 *  input:
 *	fp         file pointer
 *  output:
 *	chdr       3200 bytes of segy character header
 *	bhdr        400 bytes of segy binary header
 *  zhiming li,        
 */
void auxgethdr(FILE *fp, segychdr *chdr, segybhdr *bhdr)
{
   	efread(chdr, 1, EBCBYTES, fp);
   	efread(bhdr, 1, BNYBYTES, fp);
}
