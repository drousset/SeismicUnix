#include "su.h"
#include "segy.h"
#include "header.h"

/* fputhdr - put segy tape identification headers to the file by file pointer
 *
 *  input:
 *	fp         file pointer
 *	chdr       3200 bytes of segy character header
 *	bhdr        400 bytes of segy binary header
 *  zhiming li and jean dulac,         
 */


void fputhdr(FILE *fp, segychdr *chdr, segybhdr *bhdr)
{
	extern char SU_chdr[];
        extern char SU_bhdr[];
	efwrite(chdr, 1, EBCBYTES, fp);
	efwrite(bhdr, 1, BNYBYTES, fp);
	bzero(SU_chdr,EBCBYTES);
	bzero(SU_bhdr,BNYBYTES);
}
