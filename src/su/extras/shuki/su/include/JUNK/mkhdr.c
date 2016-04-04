/* mkhdr - makes hdr.h file from output of prehdr and getoffs shells
 *
 * Credits:
 *	CWP: Jack, Shuki
 *
 * This program belongs to the Center for Wave Phenomena
 * Colorado School of Mines
 *
 * $Author: shuki $
 * $Source: /src/segy/include/RCS/mkhdr.c,v $
 * $Revision: 1.11 $ ; $Date: 87/10/05 06:58:07 $
 * $State: Exp $
*/

/* Embed Revision Control System identifier strings */
static char progid[] =
	"   $Source: /src/segy/include/RCS/mkhdr.c,v $";
static char revid[] =
	"   $Revision: 1.11 $ ; $Date: 87/10/05 06:58:07 $";

#include "segy.h"
#include "prehdr.h"

Segy tr;

main()
{
	int i;
	int SY_NK = sizeof(hdr)/sizeof(struct hdr_tag);
	int HDRBYTES = sizeof(Segy) - SY_NDAT*sizeof(float);
	int MAXSEGY = sizeof(Segy);
	char buff[16];


	/* hdr[].offs has been initialized to zero by the prehdr shell */

#include "offsets.h"

	/* Print out hdr.h on stdout */
	printf("/*\n * hdr.h - include file for segy offset array\n */\n\n");
	printf("#ifndef INCLUDE_HDR_H\n");
	printf("#define INCLUDE_HDR_H\n\n");
	printf("#define SY_NK\t\t%d\t/* Number of key header words */\n",
			SY_NK);
	printf("#define HDRBYTES\t%d\t/* Bytes in the trace header */\n",
			HDRBYTES);
	printf("#define	MAXSEGY\t\t%d\n\n", MAXSEGY);
	printf("static struct hdr_tag {\n");
	printf("\tchar *key;\tchar *type;\tint offs;\n");
	printf("} hdr[] = {\n");
	buff[0] = '"';			/* buff is for alligned output */
	for (i = 0; i < SY_NK; i++) {
		buff[1] = (char)0;
		strcpy(buff+1,hdr[i].key);
		strcat(buff,"\"");
		printf("\t%10s,\t\t\"%s\",\t\t%d,\n",
			buff, hdr[i].type, hdr[i].offs);
	}
	printf("};\n");
	printf("#endif INCLUDE_HDR_H\n");

	exit(0);
}
