/** 
 * @file   rsac1.c
 * 
 * @brief  Read an evenly spaced SAC file
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "hdr.h"

int byteswap( void* swappee, int Nbytes ) ;

/** 
 * Read in sac data from file \p nun into \p yarray of length \p npts an swap
 *   if necessary
 * 
 * @param nun 
 *    Logical file unit retured from zopen_sac
 * @param yarray 
 *    Output array where data is to be stored
 * @param npts 
 *    Length of array \p yarray
 @ @param comp
 *    Which Component to read
 * @param lswap 
 *    Swap the data if true
 * @param nerr 
 *    Error Return Flag
 *    - SAC_OK on Success
 *    - Non-Zero on Error
 *
 * @date July 01, 2008 - Initial Version  -- B. Savage
 *
 */
void
sac_data_read(int nun, float *yarray, int npts, int comp, int lswap, int *nerr) {
  int i;
  float *ptr;
  int word;

  *nerr = SAC_OK;

  word = SAC_FIRST_DATA_POINT_WORD + ((comp - 1) * npts);
  zrabs(&nun, (char *)yarray, npts, &word, nerr);
  if(*nerr != SAC_OK) 
    return;

  if(lswap) {
    for(i = 0, ptr = yarray; i < npts; i++, ptr++) {
      byteswap((void*)ptr, SAC_HEADER_SIZEOF_NUMBER);
    }
  }
}


/** 
 * Read an evenly spaced SAC file
 * 
 * @param kname 
 *    Name of disk file to read
 * @param yarray 
 *    Output data from the disk file
 * @param nlen 
 *    Number of data points to read, should be less than or equal to \p max_
 * @param beg 
 *    Beginning Time, B Header Value
 * @param del 
 *    Sampling Interval, Delta Header Value
 * @param max_ 
 *    Size of \o yarray
 * @param nerr 
 *    Error Return Flag
 *    - 0 on Success
 *    - Non-Zero on Error
 *    - 801 if file is not evenly spaced
 *    - 803 if number of points in file is greater than \p max_
 *          In this case the first \p max points are read
 *    - 1317 Error Determining SAC file Type, Not a SAC file
 * @param kname_s 
 *    Length of \p kname
 *
 * @date   Aug  4 2008 B. Savage <savage_at_uri.edu>
 *                     Changed wrtmsg() to outmsg()
 * @date   Mar 18 2007 B. Savage <savage_at_uri.edu>
 *                     Updated routines for better interpability with C
 *                     and Fortran when used as an external library
 * @date   870902:  Added calls to INILHF and INIMSG as part of initialization.
 * @date   870513:  Changed call to wrtxtd to wrtmsg.
 * @date   830125:  Changes due to modified header common block.
 * @date   810212:  Changed to output message retrieval from disk.
 * @date   800919:  Original version.
 * @date   870902:  Documented/Reviewed
 *
 */
void 
rsac1(char      *kname, 
      float      yarray[], 
      long int  *nlen, 
      float     *beg, 
      float     *del, 
      long int  *max_, 
      long int  *nerr, 
      long int   kname_s)
{
  long int  ncerr, nlcdsk, nun;
  long idx, *hdrVer, lswap;
  int truncated;
  
  *nerr     = 0;
  lswap     = FALSE;
  truncated = FALSE;

  /* - Initialize some common blocks if not already done. */
  sacio_initialize_common();
  
  /* - Open the file. */
  zopen_sac( &nun, kname,kname_s, "RODATA",7, nerr );
  if( *nerr != SAC_OK )
    goto ERROR;
  
  lswap = sac_header_read(nun, nerr);
  if( *nerr != SAC_OK )
    goto ERROR;
  
  /* - Make sure file is evenly spaced. */
  if( *leven ){
    if( *npts <= *max_ ){
      *nlen = *npts;
    }
    else{
      *nlen = *max_;
      truncated = TRUE;
    }
    *beg = *begin;
    *del = *delta;
  }
  else{
    *nerr = SAC_ERROR_FILE_NOT_EVENLY_SPACED;
    setmsg( "ERROR", *nerr );
    apcmsg( kname,kname_s );
    outmsg();
    clrmsg();
    goto ERROR;
  }
  
  /* - Read in the data. */
  sac_data_read(nun, yarray, *nlen, SAC_FIRST_COMPONENT, lswap, (int *)nerr);
  if(nerr != SAC_OK) 
    goto ERROR;
  
  /* - Adjust several header fields. */
  *npts = *nlen;
  *ennd = *begin + (*npts - 1)**delta;
  
 ERROR:
  *nerr = ( *nerr == SAC_OK && truncated == TRUE) ? -SAC_ERROR_DATA_TRUNCATED_ON_READ : SAC_OK;

  zclose( &nun, &ncerr );
  return;
}


/** 
 * Read an evenly spaced SAC file
 *   (Fortran Interface)
 * 
 * @param kname 
 *    Name of disk file to read
 * @param yarray 
 *    Output data from the disk file
 * @param nlen 
 *    Number of data points to read, should be less than or equal to \p max_
 * @param beg 
 *    Beginning Time, B Header Value
 * @param del 
 *    Sampling Interval, Delta Header Value
 * @param max_ 
 *    Size of \o yarray
 * @param nerr 
 *    Error Return Flag
 *    - 0 on Success
 *    - Non-Zero on Error
 *    - 801 if file is not evenly spaced
 *    - 803 if number of points in file is greater than \p max_
 *          In this case the first \p max points are read
 *    - 1317 Error Determining SAC file Type, Not a SAC file
 * @param kname_s 
 *    Length of \p kname
 *
 * @see rsac1
 *
 */
void
rsac1_ (char      *kname, 
	float      yarray[], 
	long int  *nlen, 
	float     *beg, 
	float     *del, 
	long int  *max_, 
	long int  *nerr, 
	long int   kname_s) {
  rsac1 ( kname , yarray , nlen , beg , del , max_ , nerr , kname_s ) ;
}


/** 
 * Read an evenly spaced SAC file
 *   (Fortran Interface)
 * 
 * @param kname 
 *    Name of disk file to read
 * @param yarray 
 *    Output data from the disk file
 * @param nlen 
 *    Number of data points to read, should be less than or equal to \p max_
 * @param beg 
 *    Beginning Time, B Header Value
 * @param del 
 *    Sampling Interval, Delta Header Value
 * @param max_ 
 *    Size of \o yarray
 * @param nerr 
 *    Error Return Flag
 *    - 0 on Success
 *    - Non-Zero on Error
 *    - 801 if file is not evenly spaced
 *    - 803 if number of points in file is greater than \p max_
 *          In this case the first \p max points are read
 *    - 1317 Error Determining SAC file Type, Not a SAC file
 * @param kname_s 
 *    Length of \p kname
 *
 * @see rsac1
 *
 */
void 
rsac1__ (char      *kname, 
	 float      yarray[], 
	 long int  *nlen, 
	 float     *beg, 
	 float     *del, 
	 long int  *max_, 
	 long int  *nerr, 
	 long int   kname_s) {
  rsac1 ( kname , yarray , nlen , beg , del , max_ , nerr , kname_s ) ;
}
