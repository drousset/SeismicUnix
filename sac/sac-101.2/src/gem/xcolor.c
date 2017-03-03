/** 
 * @file   xcolor.c
 * 
 * @brief  Color Handling 
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "gem.h"


int color_on()                 { return cmgem.lcol;   }
int color_foreground()         { return cmgem.icol;   }
int color_background()         { return cmgem.ibacol; }
int color_foreground_default() { return MLARGE; }
int color_background_default() { return 0; }

/* Color on or off */
void 
color_set          (int value) { 
  cmgem.lcol  = value;  
}

void 
color_increment_set(int value) { 
  color_set(TRUE);
  cmgem.licol = value;  
  cmgem.jicol = 0;
}

/* Background Color */
int 
color_background_set(long int color) {
  if(color >= 0) {
    cmgem.ibacol = color;
    color_set(TRUE);
    return TRUE;
  }
  return FALSE;
}

int 
color_background_set_by_name(char *color) {
  long int icolor;
  convcolorname(color, &icolor);
  return color_background_set(icolor);
}

/* Data Color */
int
color_data_set(long int color) {
  if(color >= 0) {
    cmgem.icol = color;
    color_set(TRUE);
    return TRUE;
  }
  return FALSE;
}

int 
color_data_set_by_name(char *color) {
  long int icolor;
  convcolorname(color, &icolor);
  return color_data_set(icolor);
}

/* Skeleton Color */
int
color_skeleton_set(long int color) {
  if(color >= 0) {
    cmgem.iskcol = color;
    color_set(TRUE);
    return TRUE;
  }
  return FALSE;
}
int
color_skeleton_set_by_name(char *color) {
  long int icolor;
  convcolorname(color, &icolor);
  return color_skeleton_set(icolor);
}

/* Foreground Color (Data + Skeleton) */
int 
color_foreground_set(int color) {
  return color_skeleton_set(color) && color_data_set(color);
}

int 
color_foreground_set_by_name(char* color) {
  long int icolor;
  convcolorname(color, &icolor);
  return color_foreground_set(icolor);
}

/** 
 * parse the parameter-setting command COLOR.
 *    COLOR controls the color display attributes.
 * 
 * @param nerr 
 *   Error return Flag
 *   - 0 on Success
 *   - Non-Zero on Error
 *
 * @bug Not sending error message if user attempts to create a
 *      too large a color list.  The last color in list is changed.
 *
 * @date   821221:  Added ability to change color list.
 * @date   820809:  Changed to newest set of parsing and checking functions.
 * @date   820305:  Original version.
 *
 */
void 
xcolor(long int *nerr)
{
	char ktok[9];
	long lnum, log;
	long int inum;
	float rnum;

	*nerr = 0;

	/* - Parse position-dependent tokens: */

	if( lclog( &cmgem.lcol ) )
        { /* do nothing */ }
	else if( lctok( ktok,9, &lnum, &rnum ) ){
	    inum = (long)( rnum + 0.1 );
	    if( lnum && color_data_set(inum) ){
              ictok( 1 );
	    } else {
              if(color_data_set_by_name(ktok)) {
                ictok( 1 );
              }
	    }
	}

	/* - Parse position-independent tokens: */

	while( lcmore( nerr ) ){

	    /* -- "SKELETON color/int":  change skeleton color. */
	    if( lckey( "SK$",4 ) ){
		log = lctok( ktok,9, &lnum, &rnum );
		inum = (long)( rnum + 0.1 );
		if( lnum && color_skeleton_set(inum) ) {
                  ictok( 1 );
		} else {
                  if(color_skeleton_set_by_name(ktok)) {
                    ictok( 1 );
                  }
                  else{
                    cfmt( "NEED NAME OF A COLOR:$",23 );
                    cresp();
                    ictok( -1 );
                  }
		}
	    }

	    /* -- "BACKGROUND color/int":  change the background color. */
	    else if( lckey( "BA$",4 ) ){
		log = lctok( ktok,9, &lnum, &rnum );
		inum = (long)( rnum + 0.1 );
                if(lnum && color_background_set(inum)) {
                  ictok( 1 );
		} else {
                  if(color_background_set_by_name(ktok)) {
                    ictok(1);
                  } else {
                    cfmt( "NEED NAME OF A COLOR:$",23 );
                    cresp();
                    ictok( -1 );
                  }
		}
	    }

	    /* -- "LIST STANDARD/colorlist":  change the color list. */
	    else if( lckey( "L$",3 ) ){
		if( lckey( "S$",3 ) ){
		    inicol( cmgem.iicol, &cmgem.nicol );
		}
		else{
		    cmgem.nicol = 0;
		    while( lctok( ktok,9, &lnum, &rnum ) ){
			inum = (long)( rnum + 0.1 );
			if( lnum ){
			    if( cmgem.nicol < MICOL )
				cmgem.nicol = cmgem.nicol + 1;
			    Iicol[cmgem.nicol] = inum;
			    ictok( 1 );
			}
			else{
			    convcolorname( ktok, &inum );
			    if( inum >= 0 ){
				if( cmgem.nicol < MICOL )
				    cmgem.nicol = cmgem.nicol + 1;
				Iicol[cmgem.nicol] = inum;
				ictok( 1 );
			    }
			}
		    }
		    if( cmgem.nicol <= 0 )
			inicol( cmgem.iicol, &cmgem.nicol );
		    cmgem.icol = Iicol[1];
                    color_set(TRUE);
		    cmgem.jicol = 0;
		}
	    }

	    /* -- "INCREMENT ON/OFF":  increment color after each file or not */
	    else if( lklog( "I$",3, &cmgem.licol ) ){
                color_set(TRUE);
		cmgem.jicol = 0;
	    }

	    /* -- Bad syntax. */
	    else{
		cfmt( "ILLEGAL OPTION:$",17 );
		cresp();

	    }
	}  /* end while( lcmore( nerr ) ) */

}

