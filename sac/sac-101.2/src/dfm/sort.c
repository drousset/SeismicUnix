#include <stdio.h>


/* Comparison operators for strings */
int LessThanStrg(void *a1, void *a2)
{
   char **a1i = (char**)a1;
   char **a2i = (char**)a2;
   if( strcmp(a1i[0], a2i[0]) < 0 ) return 1;
   return 0;
}

int GreaterThanStrg(void *a1, void *a2)
{
    char **a1i = (char**)a1;
    char **a2i = (char**)a2;
    if( strcmp(a1i[0], a2i[0]) > 0 ) return 1;
    return 0 ;
}

/* ------------------------------------------------------------- */


int isNaN( float a1 )
{
    DEPRECATED("isNaN()", "isnan()");
    if( !( a1 < 1.0 ) && !( a1 > 1.0 ) && !( a1 == 1.0 ) )
	return 1 ;
    return 0 ;
}

/* Comparison operators for floats */
int LessThanFloat(void *a1, void *a2)
{
   float a1i = *( (float*)a1);
   float a2i = *( (float*)a2);
   if( a1i < a2i ) return 1;
   if( isnan( a1i ) ) {
      if( !isnan( a2i ) )
         return 1 ;
   }
   return 0;
}

int GreaterThanFloat(void *a1, void *a2)
{
   float a1i = *( (float*)a1);
   float a2i = *( (float*)a2);
   if( a1i > a2i ) return 1;
   if( isnan( a2i ) ) {
      if( !isnan( a1i ) )
         return 1 ;
   }
   return 0;
} 

/* ------------------------------------------------------------- */




/* Comparison operators for ints */
int LessThanLong(void *a1, void *a2)
{
   long a1i = *( (long*)a1);
   long a2i = *( (long*)a2);
   if( a1i < a2i ) return 1;
   return 0;
}

int GreaterThanLong(void *a1, void *a2)
{
   long a1i = *( (long*)a1);
   long a2i = *( (long*)a2);
   if( a1i > a2i ) return 1;
   return 0;
}

/* ------------------------------------------------------------- */





/* Sorting function that produces a permutation index that will put
   input array (a) into sorted order, either ascending or descending.
   The sort is stable, meaning that the relative order of equal keys
   is not changed. The sorting method is straight selection
   (N-squared order).

   The array to be sorted (a) is left unchanged and should be cast to void*.

   N is the number of elements in the array.

   size is the size of one element.

   fp1 is a pointer to a user-supplied function with a prototype like:
      int fp1(<Type T> *a1, <Type T> *a2);
   for  ascending order, fp1 should return true if a1 is less than a2.
   for descending order, fp1 should return true if a1 is greater than a2.

   index is an integer array which on output contains the permutation index.

*/

void Index(char *a, int N, int size, int (*fp1)(), int *index)
{
   int i, j;
   char *v;
   
   for(i=0; i< N;i++)
      index[i] = i;
   
   for(i=1;i<N; i++){
      v = a + index[i] * size;
      j = i;
      while(j > 0 && fp1(v, a +  index[j-1] * size ) ){
         index[j] = index[j-1];
         j--;
      }
      index[j] = i;
   }


}

/* void Index(char *a, int N, int size, int (*fp1)(), int * index )
{
   int i, j, min;
   int tmp;

   for(i=0; i< N;i++)
      index[i] = i;

   for(i=0; i< N-1;i++){
      min = i;
      for(j=i+1; j < N; j++)
         if( fp1(a + index[j] * size, a + index[min] * size) )
            min = j;

      tmp = index[min];
      index[min] = index[i];
      index[i] = tmp;
   }

} */
/* ------------------------------------------------------ */

