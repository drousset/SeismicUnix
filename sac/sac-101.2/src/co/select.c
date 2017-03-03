

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <sys/select.h>
#include <sys/time.h>
#include <limits.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>
#include <errno.h>
#include "config.h"

#ifdef SOLARIS
#define __sun__
#endif /* SOLARIS */
#include <readline/readline.h>
#include <X11/Xlib.h>

#include "complex.h"
#include "proto.h"
#include "select.h"
#include "mach.h"
#include "gd3.x11.h"
#include "gdm.h"

#define SAC_HISTORY_MAX 1024

static int sac_history_size = SAC_HISTORY_MAX;

void
history_size_set(int value) {
  if (value <= 0) {
    sac_history_size = INT_MAX;
  } else {
    sac_history_size = value;
  }
  return;
}

int
history_size() {
  return sac_history_size;
}

int
select_loop_message(char *p, int len) {
  int i;
  static char *str = NULL;

  if(len <= 0) {
    if(str) {
      free(str);
      str = NULL;
    }
    str = strdup(p);
    return(strlen(str));
  }
  
  len = len - 2; /* 1 for \0 and 1 for a space */
  len = (strlen(str) > len) ? len : strlen(str);

  strncpy(p, str, len);
  p[len++] = ' ';  /* Space */
  p[len++] = '\0'; /* String Teminator */

  return(len);
}


int
select_loop_continue(int w) {
  static int flag;
  if(w == SELECT_ON || w == SELECT_OFF) {
    flag = w;
  }
  return(flag);
}

int
CheckByteOrder() {
  static int byte_order = ENDIAN_UNKNOWN;
  short int word = 0x0001;
  char *byte = (char *) &word;
  if(byte_order == ENDIAN_UNKNOWN) {
    byte_order = (! byte[0]) ? ENDIAN_BIG : ENDIAN_LITTLE;
  }
  return byte_order;
}


int
use_tty() {
  static int use = -1;
  
  if(use == -1) {
    struct termios t;
    FILE *rl_instream = stdin;
    if(tcgetattr(fileno(rl_instream), &t) == -1) {
      /*      perror("tcgetattr warning:");*/
      use = 0;
    } else {
      use = 1;
    }
  }
  return(use);
}

void
timeval_fix(struct timeval *t) {
  if(t) {
    t->tv_sec  = t->tv_sec  + floor(t->tv_usec /(1000 * 1000));
    t->tv_usec = t->tv_usec - t->tv_sec * (1000 * 1000);
  }
}

static int
input(int i, fd_set *fd) {
  if(i >= 0) {
    return(FD_ISSET(i, fd));
  }
  return(0);
}

int
show_prompt_without_tty() {
  static int flag = -1;
  char *c;
  if(flag == -1) {
    flag = FALSE;
    if((c = getenv("SAC_SCRIPT_PROMPT_DISPLAY")) != NULL) {
      if(strcmp(c,"1") != 0 && strcmp(c,"0") != 0) {
	fprintf(stderr, 
		"SAC warning: SAC_SCRIPT_PROMPT_DISPLAY must be 0 or 1\n");
	flag = FALSE;
      } else{
	flag = atoi(c);
      }
    }
  }
  return flag;
}

static char *sac_history_filename = NULL;
static int sac_history_loaded = FALSE;

void
sac_history_file_set(char *name) {
  int len;
  char *sachistory;
  char *home;
  if (sac_history_filename) {
    /* Free previous, if any */
    free(sac_history_filename);
    sac_history_filename = NULL;
  }
  if (name) {
    /* Duplicate for save -- never know whether static area pointed at! */
    sac_history_filename = strdup(name);
  } else {
    /* Null name signifies default */
    home = getenv("HOME");
    if(home) {
      len = strlen(home) + strlen(SAC_HISTORY_FILE) + 1;
      sachistory = (char *)malloc(sizeof(char) * len);
      sprintf(sachistory,"%s/%s", home, SAC_HISTORY_FILE);
      sachistory[len] = '\0';
    } else {
      sachistory = NULL;
    }
    sac_history_filename = sachistory;
  }
}

char *
sac_history_file() {
  return sac_history_filename;
}

void
sac_history_load(char *where) {
  int n;
  stifle_history( history_size() );
  if(where) {
    read_history(where);
  }
  sac_history_loaded = TRUE;
}

int
select_loop(char *prmt, long prmtlen, 
	    char *msg, long msglen, 
	    struct timeval *timeout, 
	    VFunction *func) {

  int i;	/* index for prefilling string w/ NULLs */
  
  int retval;
  fd_set fd;
  int max_fd, stdin_fd, x11_fd;
  long int nerr;
  char *c;
  char kprmt[128];

  if(!sac_history_loaded)
    sac_history_load(sac_history_file());

  /* Show the Prompt */
  i = 0;
  while(prmt[i] != '$') {
    kprmt[i] = prmt[i];
    i++;
  }
  kprmt[i] = '\0';

  rl_callback_handler_install(kprmt, func);
  fflush(stdout);
  /* Take care of printing the prompt when there is no tty
   *    This normally happends during script processing 
   */
  if(!use_tty() && show_prompt_without_tty()) { 
    fprintf(stdout, "%s", kprmt);
    fflush(stdout);
  }

  max_fd   = -1;
  x11_fd   = -1;
  stdin_fd = -1;

  /* Loop until we encounter a newline */
  select_loop_continue(SELECT_ON);

  timeval_fix(timeout);

  while(select_loop_continue(SELECT_QUERY)) {
    max_fd = -1;
    FD_ZERO(&fd);

    /* Add STDIN to the File Descriptor Set (FD_SET) */
    if(!use_tty() && timeout) { /* This is here due to co/xpause.c and co/zsleep.c */
      max_fd = -1;
    } else {
      stdin_fd =  0;
      FD_SET(stdin_fd, &fd);
      max_fd = stdin_fd;
    }
    /* Add X11 to the File Descriptor Set (FD_SET) */
    if(display3 && Lgdon[3] ) {
      x11_fd = ConnectionNumber(display3);
      FD_SET(x11_fd, &fd);
      max_fd = x11_fd;
    } 
    /* Wait until we get life from one the File Descriptors, then act */
    retval = select(max_fd+1, &fd, NULL, NULL, timeout);
    switch(retval) {
    case -1: /* Error Condition */
      if(errno != EINTR) {
	perror("SAC: Select Error");
	exit(-1);
      }
      break;
    case 0: /* Timeout Expired */
	break;
    default:
      if(input(stdin_fd, &fd)) {

	if(!use_tty()) { rl_callback_handler_remove(); }

	rl_callback_read_char();

	if(!use_tty() && select_loop_continue(SELECT_QUERY)) {
	  /* This assumes the the entire line is read in at once and processline is called
	     for each entry into rl_callback_read_char().  This will probably break on
	     some machine, some where, probably when using the GNU readline library. 
	  */
	  fprintf(stderr, "SAC Error: EOF/Quit\n");
	  fprintf(stderr, "     SAC executed from a script: quit command missing\n");
	  fprintf(stderr, "     Please add a quit to the script to avoid this message\n");
	  fprintf(stderr, "     If you think you got this message in error, \n");
	  fprintf(stderr, "     please report it to: %s\n", PACKAGE_BUGREPORT);
	  select_loop_continue(SELECT_OFF);
	  select_loop_message("quit", -1);
	}
      }
      if(Lgdon[3] && display3 && input(x11_fd, &fd)) {
	dispatchevent3(&nerr);
      }
    }

    if(timeout) {
      return(0);
    }
  }
  msglen = select_loop_message(msg, msglen);
  select_loop_continue(SELECT_OFF);
  return(0);
}

