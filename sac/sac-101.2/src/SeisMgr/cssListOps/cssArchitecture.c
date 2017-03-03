#include <stdio.h>
#include <unistd.h>

/* prototype */
void archErr( char *message ) ; /* handles errors for SetIndianSize() */

static int big = 1 ;

/* Accessor */
int  IsBigendin()
{ return big ; }


/* SetIndianSize is the mutator for big.
   This routine writes an int to disc, and reads it back to determine the 
   byte order.  If there is an error, it will set big to -1, which will be
   considerred TRUE in conditionals, and a message will print to the screen
   informing the user that byte order could not be determined, and that
   SeisMgr will presume bigendin.
*/
void SetIndianSize()
{

#ifdef ENDIAN_CHECK_USE_TMP_DIRECTORY
   /* Declarations and Initialization. */
   /* 31 changed to 64 on next line.  070307 */
   char fileName[ 64 ] ;
   char inBytes[ 4 ] ; /* this will be treated as bytes, not characters. */ 
   long int outInt = 1 ;
   FILE* ONE ;
   FILE* JUNK ;

   /* File name is 'IndianCheck_' followed by the process ID, followed by 
      'ThrowMeAway'. */
   sprintf( fileName , "/tmp/IndianCheck_%d.ThrowMeAway", getpid() ) ;

   /* Attempt to open file. */
   ONE = fopen( fileName, "wb" ) ;
   if( !ONE ){
      /* Probably permission problem. */
      sprintf( fileName, "IndianCheck_%d.ThrowMeAway", getpid() ) ;
      ONE = fopen( fileName , "wb" ) ;
      if( !ONE ){
         archErr( "Cannot open file to write." ) ;
         return ;
      }
   }


   /* Write to file. */
   fwrite( &outInt , 4, 1, ONE ) ;

   fclose( ONE ) ;


   /* Read from file. */
   ONE = fopen( fileName, "rb" ) ;
   if( !ONE ){
      archErr( "Cannot open file to read." ) ;
      return ;
   }

   fread( inBytes, 1, 4, ONE ) ;

   fclose( ONE ) ;
   unlink( fileName ) ;

   /* Check the value. */
   if( inBytes[ 0 ] == 0 &&
       inBytes[ 1 ] == 0 &&
       inBytes[ 2 ] == 0 &&
       inBytes[ 3 ] == 1 )    big = 1 ;  /* BIGENDIN */

   else if(
       inBytes[ 0 ] == 1 &&
       inBytes[ 1 ] == 0 &&
       inBytes[ 2 ] == 0 &&
       inBytes[ 3 ] == 0 )    big = 0 ;  /* LITTLENDIN */

   else
      archErr( "Failed tests for both bigendin and littlendin." ) ;
#endif  /* ENDIAN_CHECK_USE_TMP_DIRECTORY */

   big = CheckByteOrder();
   /*   fprintf(stdout, "Using CSS Byte Order: %s\n", IsBigendin() ? "BIG ENDIAN" : "LITTLE ENDIAN"); */
}



void archErr( char *message )
{
   printf( "Error determining byte order\n%s\n", message ) ;
   printf("Will assume machine is bigendin when reading CSS waveforms\n" );
   big = -1 ;
}
