#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "msg.h"
#include "select.h"

#include "config.h"

/* Report version information about SAC2000 */

char *env_on[]  = {"on",  "true",  "yes", "1" };
char *env_off[] = {"off", "false", "no",  "0" };

int
env_bool(char *env, int def) {
  int i;
  int n;
  char *env_string = getenv(env);
  if(env_string != NULL) {
    n = strlen(env_string);
    for(i = 0; i < sizeof(env_on)/sizeof(char *); i++) {
      if(strncasecmp(env_string, env_on[i], min(n, strlen(env_on[i]))) == 0) {
        return TRUE;
      }
    }
    for(i = 0; i < sizeof(env_off)/sizeof(char *); i++) {
      if(strncasecmp(env_string, env_off[i], min(n, strlen(env_off[i]))) == 0) {
        return FALSE;
      }
    }
  }
  return def;
}

int
display_copyright(int getset) {
  static int virgin = TRUE;
  static int show_copyright = TRUE;
  if(getset == OPTION_ON || getset == OPTION_OFF) {
    show_copyright = getset;
  }
  if(virgin) {
    virgin = FALSE;
    show_copyright = env_bool(SAC_DISPLAY_COPYRIGHT, show_copyright);
  }
  return show_copyright;
}

int
use_database(int getset) {
  static int virgin;
  int use_db = TRUE;
  if(getset == OPTION_ON || getset == OPTION_OFF) {
    use_db = getset;
  }
  if(virgin) {
    virgin = FALSE;
    use_db = env_bool(SAC_USE_DATABASE, use_db);
  }
  return use_db;
}

void xabout ( ) 
{
    char kvdate[200];
    /* char fmt[] = "SEISMIC ANALYSIS CODE [%s (Version 00.59.49)]"; */
    char fmt[] = "SEISMIC ANALYSIS CODE [%s (Version %s)]";
    char kcopyr[] = "Copyright 1995 Regents of the University of California\n" ;

    if(! display_copyright(OPTION_GET)) {
      return;
    }
    sprintf( kvdate, fmt, BUILD_DATE, PACKAGE_VERSION );
    setmsg( "OUTPUT", 99 );
    apcmsg( kvdate, strlen ( kvdate ) + 1 );
    aplmsg( kcopyr, strlen ( kcopyr ) + 1 );
    outmsg();
    clrmsg();
}

