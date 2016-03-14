
#include "su.h"
#include "segy.h"
#include "header.h"

/* fgethdr - get segy tape identification headers from
 *	     the program or from the file by file pointer
 *
 *  input:
 *	fp         file pointer
 *  output:
 *	chdr       3200 bytes of segy character header
 *	bhdr        400 bytes of segy binary header
 *  zhiming li  and j. dulac ,         
 */


void fgethdr(FILE *fp, segychdr *chdr, segybhdr *bhdr)
{
	extern char SU_chdr[];
	extern char SU_bhdr[];
	if (strncmp(SU_chdr, "C 1 CLIENT",10)==0 ) {
		memcpy((char*)chdr,SU_chdr,EBCBYTES);
		memcpy((char*)bhdr,SU_bhdr,BNYBYTES);
	}
	else {
	   	efread(chdr, 1, EBCBYTES, fp);
	   	efread(bhdr, 1, BNYBYTES, fp);
	}
}
