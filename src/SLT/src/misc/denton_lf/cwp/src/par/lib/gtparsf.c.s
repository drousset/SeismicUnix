
#include <par.h>

/* fortran callable getparstring(), getparint() and getparfloat() */
/* author: J. Dulac	6-8-93 */

extern int xargc ;
extern char ** xargv ;

static int _args_init = 0 ;

#if defined(sun)
extern int      p_xargc;
extern char     ** p_xargv;

void getparinit_() {
    xargc = p_xargc;
    xargv = p_xargv;
    _args_init = 1 ;
}

#elif defined(convex)

void getparinit_() {
    _args_init = 1 ;
}

#else
void getparinit_() {
}
#endif

int getparstring_(char *name,char *val,int ln,int lv) {
    char * parm, cname[256] ;
    int ret;

    if( !_args_init ) getparinit_() ;

    strncpy(cname,name,ln) ;
    cname[ln] = '\0' ;

    if( (ret=getparstring(cname,&parm)) ) {
       strncpy(val,parm,lv) ; /* copy up to val=='\0' or up to lv */
       return ret;
    }
    return 0 ;
}

int getparfloat_(char *name,float *v,int ln) {
    char cname[256] ;
    if( !_args_init ) getparinit_() ;
    strncpy(cname,name,ln) ;
    cname[ln] = '\0' ;
    return getparfloat(cname,v) ;
}

int getparint_(char *name,int *v,int ln) {
    char cname[256] ;
    if( !_args_init ) getparinit_() ;
    strncpy(cname,name,ln) ;
    cname[ln] = '\0' ;
    return getparint(cname,v) ;
}
