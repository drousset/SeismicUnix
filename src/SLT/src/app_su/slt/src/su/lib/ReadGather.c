
/*====================================================================*\
   readGather() gets a collection of traces in an arbitrary sort
   order. It allocates space as needed using realloc(3c) in chunks
   of 10 traces. The caller is expected to pass the name of an integer
   sort order header field and to maintain the count of the maximum
   fold seen so far. No attempt is made to free space as doing so 
   would be unlikely to help matters in seismic processing and might
   cause serious resource allocation problems.
   Reginald H. Beardsley                            rhb@acm.org
\*====================================================================*/


#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "su.h"
#include "segy.h"

int readGather ( FILE* Fp ,segy** Gather ,int* MaxFold ,char* Header ){

   Value currentHeaderValue;
   Value previousHeaderValue;

   int i;
   int index;

   segy* gather = *Gather;

   static int first=1;
   static segy savedTrace;

   index = getindex(Header);

   /*---------------*/
   /* check for EOF */
   /*---------------*/

   if( feof( Fp ) ){
      return( 0 );
   }

   /*------------------*/
   /* copy saved trace */
   /*------------------*/

   if( !first ){
      memcpy( (char*) &(gather[0]) ,(char*)&savedTrace ,sizeof(segy ) );
   }else{
      first=0;
   }

   /*--------------------------------*/
   /* read data into existing buffer */
   /*--------------------------------*/

   
   gethval(&(gather[0]), getindex(Header) , &previousHeaderValue );

   for( i=1; i<*MaxFold; i++ ){
 
      if( !fgettr( Fp ,&(gather[i]) ) ){
         return( i );
      }

         gethval(&(gather[i]), index , &currentHeaderValue );
         if( currentHeaderValue.i != previousHeaderValue.i ){
         memcpy( (char*) &savedTrace ,(char*) &gather[i] ,sizeof(segy ) );
         return( i );
      }

   }

   /*-------------------------------------------------------------*/
   /* allocate additional space until entire gather has been read */
   /*-------------------------------------------------------------*/

   while( 1 ){

      *MaxFold += 10;

      if( !(gather = realloc( gather ,*MaxFold*sizeof(segy))) ){
         err( "realloc failed: %s %d" ,__FILE__ ,__LINE__ );
      }

      *Gather = gather;

      for( i=*MaxFold-10; i<*MaxFold; i++ ){ 
   
         if( !fgettr( Fp ,&(gather[i]) ) ){
            return( i );
         }
   
         gethval(&(gather[i]), index , &currentHeaderValue );
         if( currentHeaderValue.i != previousHeaderValue.i ){
            memcpy( (char*) &savedTrace ,(char*) &gather[i] ,sizeof(segy ) );

            return( i );
         }
   
      }    

   }

}
