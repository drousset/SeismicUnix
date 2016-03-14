#include "su.h"
#include "segy.h"
#include "header.h"

/* auxputhdr - put segy tape identification headers to the auxiliary
 * file by file pointer
 *
 *  input:
 *	fp         file pointer
 *	chdr       3200 bytes of segy character header
 *	bhdr        400 bytes of segy binary header
 *  zhiming li,        
 */

void auxputhdr(FILE *fp, segychdr *chdr, segybhdr *bhdr)
{
	efwrite(chdr, 1, EBCBYTES, fp);
	efwrite(bhdr, 1, BNYBYTES, fp);
}
