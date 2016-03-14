/* create identification headers for segy data */
/* input:
* 	ns	-- 	number of samples per trace
*  output:
*	ch	--	c card header
*	bh	--	binary header
*
*  author:	zhiming li 	8-14-92		      
*/

#include "su.h"
#include "segy.h"
#include "header.h"


void idhdrs(segychdr *ch, segybhdr *bh, int ns) {

	char *cbuf;

	/* create 400 bytes of binary header */	
	bzero(bh,BNYBYTES);
	bh->hns = ns;
	bh->format  = 1;

	cbuf = emalloc(3200*sizeof(char));

	/* create 3200 bytes of c cards */
		
sprintf(cbuf, "%-79s\n",
"C 1 CLIENT                        COMPANY                       CREW NO     ");
sprintf(cbuf+80, "%-79s\n",
"C 2 LINE            AREA                       MAP ID                       ");
sprintf(cbuf+160, "%-79s\n",
"C 3 REEL NO           DAY-START OF REEL     YEAR      OBSERVER              ");
sprintf(cbuf+240, "%-79s\n",
"C 4 INSTRUMENT  MFG            MODEL            SERIAL NO                   ");
sprintf(cbuf+320, "%-79s\n",
"C 5 DATA TRACES/RECORD        AUXILIARY TRACES/RECORD         CDP FOLD      ");
sprintf(cbuf+400, "%-79s\n",
"C 6 SAMPLE INTERVAL         SAMPLES/TRACE       BITS/IN      BYTES/SAMPLE   ");
sprintf(cbuf+480, "%-79s\n",
"C 7 RECORDING FORMAT        FORMAT THIS REEL        MEASUREMENT SYSTEM      ");
sprintf(cbuf+560, "%-79s\n",
"C 8 SAMPLE CODE                                                             ");
sprintf(cbuf+640, "%-79s\n",
"C 9 GAIN  TYPE                                                              ");
sprintf(cbuf+720, "%-79s\n",
"C10 FILTERS                                                                 ");
sprintf(cbuf+800, "%-79s\n",
"C11 SOURCE  TYPE            NUMBER/POINT        POINT INTERVAL              ");
sprintf(cbuf+880, "%-79s\n",
"C12     PATTERN                            LENGTH        WIDTH              ");
sprintf(cbuf+960, "%-79s\n",
"C13 SWEEP  START     HZ  END     HZ  LENGTH      MS  CHANNEL NO     TYPE    ");
sprintf(cbuf+1040, "%-79s\n",
"C14 TAPER  START LENGTH       MS  END LENGTH       MS  TYPE                 ");
sprintf(cbuf+1120, "%-79s\n",
"C15 SPREAD  OFFSET        MAX DISTANCE        GROUP INTERVAL                ");
sprintf(cbuf+1200, "%-79s\n",
"C16 GEOPHONES  PER GROUP     SPACING     FREQUENCY     MFG          MODEL   ");
sprintf(cbuf+1280, "%-79s\n",
"C17      TYPE                              LENGTH        WIDTH              ");
sprintf(cbuf+1360, "%-79s\n",
"C18 TRACES SORTED BY               PROJECT                LINE ID           ");
sprintf(cbuf+1440, "%-79s\n",
"C19 AMPLITUDE RECOVERY                                                      ");
sprintf(cbuf+1520, "%-79s\n",
"C20 MAP PROJECTION                      ZONE ID       COORDINATE UNITS      ");
sprintf(cbuf+1600, "%-79s\n",
"C21 FIELD SUM       NAVIGATION SYSTEM               RECORDING PARTY         ");
sprintf(cbuf+1680, "%-79s\n",
"C22 CABLE TYPE                   DEPTH        SHOOTING DIRECTION            ");
sprintf(cbuf+1760, "%-79s\n",
"C23                                                                         ");
sprintf(cbuf+1840, "%-79s\n",
"C24                                                                         ");
sprintf(cbuf+1920, "%-79s\n",
"C25                                                                         ");
sprintf(cbuf+2000, "%-79s\n",
"C26                                                                         ");
sprintf(cbuf+2080, "%-79s\n",
"C27                                                                         ");
sprintf(cbuf+2160, "%-79s\n",
"C28                                                                         ");
sprintf(cbuf+2240, "%-79s\n",
"C29                                                                         ");
sprintf(cbuf+2320, "%-79s\n",
"C30                                                                         ");
sprintf(cbuf+2400, "%-79s\n",
"C31                                                                         ");
sprintf(cbuf+2480, "%-79s\n",
"C32                                                                         ");
sprintf(cbuf+2560, "%-79s\n",
"C33                                                                         ");
sprintf(cbuf+2640, "%-79s\n",
"C34                                                                         ");
sprintf(cbuf+2720, "%-79s\n",
"C35                                                                         ");
sprintf(cbuf+2800, "%-79s\n",
"C36                                                                         ");
sprintf(cbuf+2880, "%-79s\n",
"C37                                                                         ");
sprintf(cbuf+2960, "%-79s\n",
"C38                                                                         ");
sprintf(cbuf+3040, "%-79s\n",
"C39                                                                         ");
sprintf(cbuf+3120, "%-79s\n",
"C40 END EBCDIC                                                              ");


	bcopy(cbuf,(char*)ch,3200);

	free(cbuf);

}

