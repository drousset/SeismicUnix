
/*--------------------------------------------------------------------*\
   Print velocity function in Landmark format.

   Reginald H. Beardsley                            rhb@acm.org
\*--------------------------------------------------------------------*/

#include "velo.h"
#include "par.h"


void printavf(
    int cdp
   ,double x 
   ,double y 
   ,int ntv
   ,float *tout
   ,float *vout
   ,FILE *outfp
   ){

   int i;

   fprintf( outfp ,"%15d "   ,cdp);
   fprintf( outfp ,"%15.1f " ,x         );
   fprintf( outfp ,"%15.1f " ,y         );
   fprintf( outfp ,"%10.2f " ,tout[0]   );
   if( vout[0] > 0.0 ){
      fprintf( outfp ,"%10.2f " ,vout[0]   );
   }else{
      fprintf( outfp ,"%10.2f " ,vout[1]   );

   }
   fprintf( outfp ,"\n"                 );

   for( i=1; i<ntv; i++ ){


      fprintf( outfp ,"%15d "   ,cdp);
      fprintf( outfp ,"%15.1f " ,x         );
      fprintf( outfp ,"%15.1f " ,y         );
      fprintf( outfp ,"%10.2f " ,tout[i]   );
      fprintf( outfp ,"%10.2f " ,vout[i]   );
      fprintf( outfp ,"\n"                 );
   
   }

}

