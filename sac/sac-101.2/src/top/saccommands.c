/** 
 * @file   saccommands.c
 * 
 * @brief  Process sac commands
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "exm.h"
#include "dfm.h"

#include "cnv.h"

#define SAC_BLACKBOARD_SACNFILES         "SACNFILES"
#define SAC_BLACKBOARD_SACNFILES_LENGTH  strlen(SAC_BLACKBOARD_SACNFILES)
#define SACNFILES_DIGITS                 11

/** 
 * Set the number of files to the blackboard variable SACNFILES
 *
 * Post the number of files to the black board before returning.
 * gets cmdfm.ndfl in ascii to post SACNFILES 
 *
 * @param nerr 
 *    Error Return Code
 *    - 0 on Succes
 *    - Non-Zero on Error
 * 
 * @see setbbv
 *
 * @date July 24, 2008 - Isolated from the saccommands function <savage@uri.edu>
 * @date October 10, 1996 - Original Version. MAF
 */
void
sac_report_files_in_memory(long int *nerr) {

  char kNumberOfFiles[SACNFILES_DIGITS] ; 
  
  snprintf ( kNumberOfFiles, SACNFILES_DIGITS, "%d", cmdfm.ndfl ) ;
  setbbv ( SAC_BLACKBOARD_SACNFILES, 
	   kNumberOfFiles, 
	   nerr, 
	   SAC_BLACKBOARD_SACNFILES_LENGTH,
	   strlen(kNumberOfFiles) );
}

/** 
 * Execute one or more SAC commands in a message
 * 
 * - Load Messages onto the command stack -- pcmsg()
 * - Get the next command off the command stack -- gc()
 * - If a command exists, continue, otherwise exit 
 * - Convert command name to upper case -- modcase()
 * - Echo command if requested -- wrcom()
 * - Find the command module and index number -- findcommand()
 * - Execute command if the command was found -- executecommand()
 * - If not found, assume it is a system command -- zsysop()
 * - Loop until the command stack is empty
 * - Report the number of sac files in memory
 *
 * @param kinmsg 
 *    Message to execute containing SAC commands
 * @param kinmsg_s 
 *    Length of message \p kinmsg
 * @param nerr 
 *    Error Return Code
 *    - 0 on Success
 *    - Non-Zero on Error
 *
 * @date   961010:  Posts number of files to the black board after each
 *                  command. maf
 * @date   910624:  Changed error handling logic around after executecommand
 *                  to get the system command error to print (wct).
 * @date   891005:  Deleted "itpmsg" argument and changed "nerr" argument.
 * @date   890110:  Added call to "tracereport".
 * @date   870722:  Modified ISTAT output to include execution error reporting.
 * @date   860206:  Added call to REPERR to report current error status.
 * @date   820901:  Major modification due to new command parsing logic.
 * @date   811125:  Added call to ZSHSG.
 * @date   810429:  Original version.
 *
 */
void 
saccommands(char     *kinmsg, 
	    int       kinmsg_s, 
	    long int *nerr)
{
	char kcommand[9] = "        " ;
	long lexist, lfound;
	long int index, module, notused, nchar;
        long memerr;
        char *temp;
       
	*nerr = 0;
	kcommand[0] = '\0' ; /* fix an access violation, maf 980507 */


	if ( SeisMgrCode ( kinmsg , nerr ) ) 
	    goto L_8888 ;

        memerr = -1;

	/* - Load message into command stack. */
	pcmsg( kinmsg,kinmsg_s, nerr );
	if( *nerr != 0 )
		goto L_8888;


L_1000:
        /* Reset the Conversion Error Flag to a Non-Error - 0 */
        cmicnv.icnver = 0;

	/* - Get next command off command stack. */
	gc( &lexist, nerr );
	if( *nerr != 0 ){
	  reperr( *nerr );
	  if( *nerr != 0 )
	    proerr( nerr );
	  goto L_1000;
	}
	

	/* - If a command exists in the command stack: */

	if( lexist ){
	  /* -- Get command name and convert to uppercase. */
	  lcchar( MCPW, kcommand,9, &notused );
	  modcase( TRUE, kcommand, MCPW, kcommand );

	  /* -- Echo command if requested. */
	  if( cmexm.lecho && strcmp(kcommand,"ECHO    ") != 0 )
	    wrcom();
	  
	  
	  /* -- Validate command name and find module and index number. */
	  findcommand( kcommand, &lfound, &module, &index );
	  
	  /* -- If valid, execute command.
	   *    Report any errors. */
	  if( lfound ){
	    executecommand( module, index, nerr );
	    reperr( *nerr );
	    if( *nerr != 0 ){
	      setmsg( "ERROR", *nerr );
	      proerr( nerr );
	    }
	    if( cmexm.ntraces > 0 )
	      tracereport( nerr );
	  }
	  else{
	    nchar = indexb(kinmsg,kinmsg_s);
	    if(nchar > 0){
	      temp = kinmsg;
	      while ( (*temp == ' ') || (*temp == '\t') ) temp++;
	      /* make sure that the first char is not something like *, and */
	      /* disable the dangerous rm command.                          */
	      if ( isalpha ((int)*temp) || (*temp == '/'))
		if(strncmp(temp,"rm ",3) != 0){
		  zsysop(kinmsg,(long)0,&nchar,nerr);
		} else {
		  *nerr = 1106;
		}
	    }
	    if(*nerr != 0 ) {
	      *nerr = 1106;
	      setmsg( "ERROR", *nerr );
	      reperr( *nerr );
	      if( *nerr != 0 )
		proerr( nerr );
	    }
	    goto L_8888;
	  }
	  
	  /* -- Loop until command stack is empty. */
	  goto L_1000;
	  
	}
	
 L_8888:
	sac_report_files_in_memory(nerr);
	return;	
}

