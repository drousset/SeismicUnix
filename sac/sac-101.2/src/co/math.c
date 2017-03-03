#include <stdlib.h>
#include <math.h>

#include "config.h"

long min(a,b)
long a, b;
{

   return ( a < b ? a : b );
}

long max(a,b)
long a, b;
{

  return ( a > b ? a : b );
}

/* fmin, fmax, and labs are defined in the Standard C library on Mac OS X. */

#if MISSING_FUNC_FMIN
double fmin(a,b)
double a, b;
{

  return ( a < b ? a : b );
}
#endif

#if MISSING_FUNC_FMAX
double fmax(a,b)
double a, b;
{

  return ( a > b ? a : b );
}
#endif

#if MISSING_FUNC_LROUND
long int
lround(double z) {
  double min;
  if(z >= 0.0) {
    min = floor(z);
    if(z - min >= 0.5) {
      min += 1.0;
    }
    return (long int) min;
  } else {
    min = floor(-z);
    if(-z - min  >= 0.5) {
      min += 1.0;
    }
    min = min * -1.0;
    return (long int) min;
  }
}
#endif 
/*
long labs(a)
long a;
{

  return ( a >= 0L ? a : -a );
}
*/

long isign(a,b)
long a, b;
{

  return ( b >= 0L ? labs(a) : -(labs(a)) );
}

double sign(a,b)
double a, b;
{

  return ( b >= 0.0 ? fabs(a) : -(fabs(a)) );
}

double powi(b,x)
double b;
long x;

{
  double temp;
  long i;


  if ( b == 0.0 ) return( (double) 0.0 );
  if ( x == 0L ) return( (double) 1.0 ) ;

  if ( x > 0L ) {
    temp = b;
    for ( i = x-1; i > 0L; i-- ) temp *= b;
    return temp;
  }

  if ( x < 0L ) {
    temp = 1.0 / b;
    for ( i = x+1; i < 0L; i++ ) temp *= (1.0/b);
    return temp;
  }
}

long ipow(b,x)
long b, x;

{
  long temp, i;


  if ( b == 0L ) return 0L;
  if ( b == 1L ) return 1L;

  if ( x < 0L ) return 0L;
  if ( x == 0L ) return 1L;
  if ( x == 1L ) return b;

  temp = b;
  for ( i = x-1; i > 0L; i--) temp *= b;
  
  return temp;
}

