
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void
DEPRECATED(char *old_func, char *new_func) {
  fprintf(stderr, "%s has been deprecated. Inform the developer or use %s\n",
	  old_func, new_func);
  return;
}

/* Find Frist of a series of Trailing Whitespace (Spaces) at the 
 * end of a string and the truncate '\0' the string there.
 */ 
char *
fstrtrim(char *s) {
  char *p;
  p = s + strlen(s) - 1;
  while(*p == ' ' && p != s) {
    p--;
  }
  s[p-s+1] = 0;
  if(strlen(s) == 1 && *s == ' ') {
    s[0] = 0;
  }
  return s;
}


char *
fstrdup(char *s, long int n) {
  char *q;
  if(n < 0) {
    n = strlen(s);
  }
  /* Length of string plus terminator */
  q = (char *)malloc(sizeof(char) * (n + 1)); 
  q[0] = '\0';
  q = strncpy(q,s,n);
  /* Null - Terminate String */
  q[n] = '\0'; 
  /* Find First of All Remaining Whitespace and Terminate There */
  q = fstrtrim(q);
  return q; 
}

char *
fstrset(char *in, char *out, long int n) {
  memset(out, ' ', n);
  strncpy(out, in, strlen(in));
  return out;
}

#ifdef __TESTING_F2C__

void
cfunc(char *s, long int len) {
  char *q = fstrdup(s,len);
  fprintf(stderr, "q: <%s>\n", q);
  free(q);
  return;
}

void cfunc_ (char *s, long int len) { cfunc(s, len); }
void cfunc__(char *s, long int len) { cfunc(s, len); }


void
ccall_() {
  char *a;
  a = strdup("hello");
  cfunc(a, -1);
  a = strdup("hello    ");
  cfunc(a, -1);
  a = strdup("hel lo ");
  cfunc(a, -1);
  a = strdup(" hel lo   ");
  cfunc(a, -1);
  cfunc("hello", -1);
  cfunc(" hello", -1);
  cfunc("hello ", -1);
  cfunc("he llo", -1);
  cfunc(" he llo", -1);
  cfunc("he llo ", -1);
  cfunc(" he llo ", -1);
  cfunc(" he llo fjfjdj  jfjdjj j jfjdj j j jfjdkfkdk j j jfjd jj j    ", -1);
  cfunc("jfdkjfdksjfdkslfjdskfjdslkfjdslkfjsdlkfjsdlfkjsdlfkdjsl", -1);
}
#endif /* __TESTING_F2C__  */

