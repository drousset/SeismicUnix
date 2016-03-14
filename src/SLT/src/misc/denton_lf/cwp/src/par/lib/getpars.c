/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*----------------------------------------------------------------------
 * Copyright (c) Colorado School of Mines, 1989.
 * All rights reserved.
 *
 * This code is part of SU.  SU stands for Seismic Unix, a processing line
 * developed at the Colorado School of Mines, partially based on Stanford
 * Exploration Project (SEP) software.  Inquiries should be addressed to:
 *
 *  Jack K. Cohen, Center for Wave Phenomena, Colorado School of Mines,
 *  Golden, CO 80401  (jkc@dix.mines.colorado.edu)
 *
 *  J.C. Dulac,       , Add auxgetpar.
 *----------------------------------------------------------------------
 */
 
/*****************************************************************************
Functions to get parameters from the command line.  Numeric parameters may
be single values or arrays of int, uint, short, ushort, long, ulong, float, 
or double.  Single character strings (type string or char *) may also be
gotten.  (Arrays of strings may not be gotten.)  The functions are:

int getpar<type> (char *name, <type> *ptr);
	Gets values for the last occurence of a parameter, where <type>
	denotes one of the parameter types listed above.  Returns the
	number of parameters gotten, zero if parameter not specified.

int getpar(char *aux,char *name, char *type, <type>*ptr);
	Gets values for the last occurence of a parameter, where <type>
	denotes one of the parameter types listed above.  Returns the
	number of parameters gotten, zero if parameter not specified.
	type=i:int, p:unsigned int, h:short, u:unsigned short, l:long,
	     v:unsigned long, f:float, d:double, s:string

int auxgetpar<type>(char *aux,char *name,<type>*ptr);
	Gets values for the last occurence of a parameter in an auxiliary
	input file where aux is the name of the parameter pointing to the file,
	where <type> denotes one of the parameter types listed above.
	Returns the number of parameters gotten, zero if parameter not specified.

int auxgetpar(char *aux,char *name, char *type, <type>*ptr);
	Gets values for the last occurence of a parameter in an auxiliary
	input file where aux is the name of the parameter pointing to the file,
	where <type> denotes one of the parameter types listed above.
	Returns the number of parameters gotten, zero if parameter not specified.
	type=i:int, p:unsigned int, h:short, u:unsigned short, l:long,
	     v:unsigned long, f:float, d:double, s:string

int getnpar<type> (int n, char *name, <type> *ptr);
	Gets values for the n'th occurence of a parameter, where <type>
	denotes one of the parameter types listed above.  Returns the
	number of parameters gotten, zero if n'th occurence not found.

int countparname (char *name);
	Returns the number of occurences of a parameter name.

int countparval (char *name);
	Returns the number of values in the last occurence of a parameter.

int countnparval (char *name);
	Returns the number of values in the n'th occurence of a parameter.
******************************************************************************	
Example:
	... if integer n not specified, then default to zero. 
	if (!getparint("n", &n)) n = 0;
	
	... if array of floats vx is specified, then
	if (nx=countparval("vx")) {
		... allocate space for array
		vx = ealloc1float(nx);
		... and get the floats
		getparfloat("vx",vx);
	}
	
The command line for the above examples might look like:
	progname n=35 vx=3.21,4,9.5

More examples are provided in the DTEST code at the end of this file.
******************************************************************************	
Notes:
The functions: eatoh, eatou, eatol, eatov, eatoi, eatop used
below are versions of atoi that check for overflow.  The source
file for these functions is atopkge.c.
******************************************************************************	
Authors:
Rob Clayton & Jon Claerbout, Stanford University, 1979-1985
Shuki Ronen & Jack Cohen, Colorado School of Mines, 1985-1990
Dave Hale, Colorado School of Mines, 05/29/90
*****************************************************************************/	

#include "par.h" 

/* parameter table */
typedef struct {
	char *name;		/* external name of parameter	*/
	char *asciival;		/* ascii value of parameter	*/
} ArgStruct;

/* global variables declared and used internally */
static ArgStruct *argtbl;	/* parameter table		*/
static int nargs;		/* number of args that parse	*/
static bool tabled = false;	/* true when parameters tabled 	*/

/* functions declared and used internally */
static int getparindex (int n, char *name);
static int getparinit(void);
static int tabulate (int argc, char **argv);
static char *getpfname (void);
static int white2null (char *str, int len);
static int ccount (char c, char *s);

/* functions to get values for the last occurence of a parameter name */
int getparint(char *pn, int *v) { return getnpar(0,pn,"i",v); }
int getparuint(char *pn, unsigned int *v){ return getnpar(0,pn,"p",v);}
int getparshort(char *pn, short *v) { return getnpar(0,pn,"h",v); }
int getparushort(char *pn, unsigned short *v) { return getnpar(0,pn,"u",v); }
int getparlong(char *pn, long *v) { return getnpar(0,pn,"l",v); }
int getparulong(char *pn, unsigned long *v) { return getnpar(0,pn,"v",v); }
int getparfloat(char *pn, float *v) { return getnpar(0,pn,"f",v); }
int getpardouble(char *pn, double *v) { return getnpar(0,pn,"d",v); }
int getparstring(char *pn, char **v) { return getnpar(0,pn,"s",v); }
int getparstringarray (char *name, char **ptr) { return getnpar(0,name,"a",ptr) ; }

/* functions to get values for the n'th occurence of a parameter pn */
int getnparint(int n, char *pn, int *v) { return getnpar(n,pn,"i",v); }
int getnparuint(int n, char *pn, unsigned int *v){ return getnpar(n,pn,"p",v); }
int getnparshort(int n,char *pn,short *v) { return getnpar(n,pn,"h",v); }
int getnparushort(int n,char *pn,unsigned short *v){return getnpar(n,pn,"u",v);}
int getnparlong(int n,char *pn,long *v) { return getnpar(n,pn,"l",v); }
int getnparulong(int n,char *pn,unsigned long *v){ return getnpar(n,pn,"v",v); }
int getnparfloat(int n,char *pn,float *v){ return getnpar(n,pn,"f",v); }
int getnpardouble(int n,char *pn,double *v){ return getnpar(n,pn,"d",v); }
int getnparstring(int n,char *pn,char **v){ return getnpar(n,pn,"s",v); }
int getnparstringarray (int n, char *name, char **ptr) { return getnpar(n,name,"a",ptr); }

int getpar(char *pn,char *type,void *v) { return getnpar(0,pn,type,v); }

int auxgetpar(char *aux, char *name, char *type, void *ptr)
{
     int nval ;
     bool getauxtbl(char *aux), initauxtbl(char *aux) ;
     void addauxtbl(char *aux) ;

     ArgStruct *s_argtbl = argtbl ;	/* save parameter table	*/
     int s_nargs = nargs ;		/* save number of args */

     if( !getauxtbl(aux) ) {            /* look inside already read auxfiles */
	  if( !initauxtbl(aux) ) {      /* read auxiliary file */
	     argtbl = 0 ;
	     nargs = 0 ;
	  } 
          addauxtbl(aux) ;              /* add to the tabled auxfiles */
     }

     nval = getnpar(0,name,type,ptr) ;

     argtbl = s_argtbl ;                /* restore default parameter table */
     nargs = s_nargs ;                  /* restore number of arguments */

     return nval ;
}

int auxgetparint(char *a,char*p,int *v) { return auxgetpar(a,p,"i",v); }
int auxgetparuint(char*a,char*p,unsigned int*v){ return auxgetpar(a,p,"p",v);}
int auxgetparshort(char *a,char *p,short *v) { return auxgetpar(a,p,"h",v); }
int auxgetparushort(char*a,char*p,unsigned short*v){return auxgetpar(a,p,"u",v);}
int auxgetparlong(char *a,char *p,long *v) { return auxgetpar(a,p,"l",v); }
int auxgetparulong(char*a,char*p,unsigned long*v){return auxgetpar(a,p,"v",v);}
int auxgetparfloat(char *a,char *p,float *v){ return auxgetpar(a,p,"f",v); }
int auxgetpardouble(char *a,char *p,double *v){ return auxgetpar(a,p,"d",v); }
int auxgetparstring(char *a,char *p,char **v){ return auxgetpar(a,p,"s",v); }

int getnpar(int n, char *name, char *type, void *ptr)
{
	int i;			/* index of name in symbol table	*/
	int nval;		/* number of parameter values found	*/
	char *aval;		/* ascii field of symbol		*/

	if (xargc == 1) return 0;
	if (!tabled) getparinit();/* Tabulate command line and parfile */
	i = getparindex(n,name);/* Get parameter index */
	if (i < 0) return 0;	/* Not there */
	
	/* 
	 * handle string type as a special case, since a string 
	 * may contain commas.  Therefore, no arrays of strings.
	 */
	if (type[0]=='s') {
		*((char**)ptr) = argtbl[i].asciival;
		return 1;
	}

	/* convert vector of ascii values to numeric values */
	for (nval=0,aval=argtbl[i].asciival; *aval; nval++) {
		switch (type[0]) {
			case 'i':
				*(int*)ptr = eatoi(aval);
				ptr = (int*)ptr+1;
				break;
			case 'p':
				*(unsigned int*)ptr = eatop(aval);
				ptr = (unsigned int*)ptr+1;
				break;
			case 'h':
				*(short*)ptr = eatoh(aval);
				ptr = (short*)ptr+1;
				break;
			case 'u':
				*(unsigned short*)ptr = eatou(aval);
				ptr = (unsigned short*)ptr+1;
				break;
			case 'l':
				*(long*)ptr = eatol(aval);
				ptr = (long*)ptr+1;
				break;
			case 'v':
				*(unsigned long*)ptr = eatov(aval);
				ptr = (unsigned long*)ptr+1;
				break;
			case 'f':
				*(float*)ptr = eatof(aval);
				ptr = (float*)ptr+1;
				break;
			case 'd':
				*(double*)ptr = eatod(aval);
				ptr = (double*)ptr+1;
				break;
			default:
				err("%s: invalid parameter type = %s",
					__FILE__,type);
		}
		while (*aval++ != ',') {
			if (!*aval) break;
		}
	}
	return nval;
}

/* return number of occurences of parameter name */
int countparname (char *name)
{
	int i,nname;

	if (xargc == 1) return 0;
	if (!tabled) getparinit();
	for (i=0,nname=0; i<nargs; ++i)
		if (!strcmp(name,argtbl[i].name)) ++nname;
	return nname;
}

/* return number of values in n'th occurence of parameter name */
int countnparval (int n, char *name)
{
	int i;

	if (xargc == 1) return 0;
	if (!tabled) getparinit();
	i = getparindex(n,name);
	if (i>=0) 
		return ccount(',',argtbl[i].asciival) + 1;
	else
		return 0;
}

/* return number of values in last occurence of parameter name */
int countparval (char *name)
{
	return countnparval(0,name);
}

/* obsolute functions - provided here for compatibility */
int igetpar (char *name, int *ptr) { return getnpar(0,name,"i",ptr); }
int pgetpar (char *name, unsigned int *ptr) { return getnpar(0,name,"p",ptr); }
int hgetpar (char *name, short *ptr) { return getnpar(0,name,"h",ptr); }
int ugetpar (char *name, unsigned short *ptr) { return getnpar(0,name,"u",ptr);}
int lgetpar (char *name, long *ptr) { return getnpar(0,name,"l",ptr); }
int vgetpar (char *name, unsigned long *ptr) { return getnpar(0,name,"v",ptr); }
int fgetpar (char *name, float *ptr) { return getnpar(0,name,"f",ptr); }
int dgetpar (char *name, double *ptr) { return getnpar(0,name,"d",ptr); }
int sgetpar (char *name, char **ptr) { return getnpar(0,name,"s",ptr); }

typedef struct {
     char *name;	     /* parameter name of auxiliary file */
     ArgStruct * args ;      /* arguments */
     int nargs ;             /* number of arguments */
} AuxStruct ;

#define MAX_AUXFILE 256 
static int naux = 0 ;
static AuxStruct aux_table[MAX_AUXFILE] ;
    
static bool getauxtbl(char *aux) {
     int i ;
     for( i=0; i < naux; i++ ) {
	  if( !strcmp(aux,aux_table[i].name) ) {
	       argtbl = aux_table[i].args ;
	       nargs = aux_table[i].nargs ;
	       return 1 ;
	  }
     }
     return 0 ;
}

static void addauxtbl(char *aux) {
     if( naux == MAX_AUXFILE ) {
	 err("to many auxiliary files") ; 
	 return ;
     }
     aux_table[naux].name = strdup(aux) ;
     aux_table[naux].args = argtbl ;
     aux_table[naux].nargs = nargs ;
     naux++ ;
}

static bool initauxtbl(char *aux) {
     char *filename;
     FILE * fp ;
     int pflen, nread, pfargc, targc, i, j ;
     char *argstr, **targv ;

     if( !getnpar(0,aux,"s",&filename) ) {
	fprintf(stderr,"Can't get auxiliary input %s definition\n",aux) ;
        return 0 ;
     }
     fp = fopen(filename, "r") ;
     if( fp == NULL ) {
        fprintf(stderr,"Can't open auxiliary input file %s=%s\n",aux,filename);
        return 0 ;
     }

     /* Get the length */
     efseek(fp, 0, SEEK_END);
     pflen = eftell(fp);
     rewind(fp);
     argstr = (char *) ealloc1(1+pflen+1, 1);

     /* Read the parfile */
     nread = efread(argstr+1, 1, pflen, fp);
     if (nread != pflen) {
   	  err("%s: fread only %d bytes out of %d from %s",
	      __FILE__,nread,pflen,filename);
     }
     efclose(fp);

     /* Zap whites in parfile to help in parsing */
     argstr[0] = '\0' ;
     pfargc = white2null(argstr, pflen);
     targc = pfargc ;

     /* Allocate space for total arg pointers */
     targv = (char **) ealloc1(targc, sizeof(char*));

     /* Parse the parfile.  Skip over multiple NULLs */
     for( j=1, i=0; j < pflen; j++) {
	if( argstr[j] && !argstr[j-1] ) {
	       targv[i++] = argstr + j;
	}
     }

     /* Allocate space for the pointer table */
     argtbl = (ArgStruct*) ealloc1(targc, sizeof(ArgStruct));

      /* Tabulate targv */
     tabulate(targc, targv);
     return 1 ;
}

/*
 * Return the index of the n'th occurence of a parameter name, 
 * except if n==0, return the index of the last occurence.
 * Return -1 if the specified occurence does not exist.
 */
static int getparindex (int n, char *name)
{
	int i;
	if (n==0) {
		for (i=nargs-1; i>=0; --i)
			if (!strcmp(name,argtbl[i].name)) break;
		return i;
	} else {
		for (i=0; i<nargs; ++i)
			if (!strcmp(name,argtbl[i].name))
				if (--n==0) break;
		if (i<nargs)
			return i;
		else
			return -1;
	}
}

/* Initialize getpar */
static int getparinit (void)
{
	static int targc;	/* total number of args			*/
	static char **targv;	/* pointer to arg strings		*/
	static char *pfname;	/* name of parameter file		*/
	FILE *pffd;		/* file id of parameter file		*/
	int pflen;		/* length of parameter file in bytes	*/ 
	static int pfargc;	/* arg count from parameter file	*/
	bool parfile;		/* parfile existence flag		*/
	int argstrlen;
	char *argstr, *pargstr;	/* storage for command line and
						parameter file args	*/
	int nread;		/* bytes fread				*/
	int i, j;		/* counters				*/
	char *getpfname();	/* return name of parameter file	*/
	int white2null();	/* deliminate arg strings from parameter
				   file with (possibly multiple) NULLs
				   and return a count of the strings	*/
	int tabulate();		/* install symbol table			*/


	tabled = true;		/* remember table is built		*/

	/* Check if xargc was initiated */
	if(!xargc)
		err("%s: xargc=%d -- not initiated in main", __FILE__, xargc);

	/* Space needed for command lines */
	for (i = 1, argstrlen = 0; i < xargc; i++) {
		argstrlen += strlen(xargv[i]) + 1;
	}

	/* Get parfile name if there is one */
	/* parfile = (pfname = getpfname()) ? true : false; */
	if (pfname = getpfname()) {
		parfile = true;
	} else {
		parfile = false;
	}

	if (parfile) {
	 	pffd = efopen(pfname, "r");

		/* Get the length */
		efseek(pffd, 0, SEEK_END);
		pflen = eftell(pffd);
		rewind(pffd);
		argstrlen += pflen;
	} else {
		pflen = 0;
	}

	/* Allocate space for command line and parameter file
		plus nulls at the ends to help with parsing. */
	/* argstr = (char *) calloc((size_t) (1+argstrlen+1), 1); */
	argstr = (char *) ealloc1(1+argstrlen+1, 1);

	if (parfile) {
		/* Read the parfile */
		nread = efread(argstr + 1, 1, pflen, pffd);
  		if (nread != pflen) {
  	 	    err("%s: fread only %d bytes out of %d from %s",
  					__FILE__,  nread, pflen, pfname);
		}
		efclose(pffd);

		/* Zap whites in parfile to help in parsing */
		argstr[0] = '\0' ;
		pfargc = white2null(argstr, pflen);

	} else {
		pfargc = 0;
	}

	/* Total arg count */
	targc = pfargc + xargc - 1;

	/* Allocate space for total arg pointers */
	targv = (char **) ealloc1(targc, sizeof(char*));

	if (parfile) {
		/* Parse the parfile.  Skip over multiple NULLs */
		for (j = 1, i = 0; j < pflen; j++) {
			if (argstr[j] && !argstr[j-1]) {
			       targv[i++] = argstr + j;
			}
		}
	} else {
		i = 0;
	}

	/* Copy command line arguments */
	for (j = 1, pargstr = argstr + pflen + 2; j < xargc; j++) {
		strcpy(pargstr,xargv[j]);
		targv[i++] = pargstr;
		pargstr += strlen(xargv[j]) + 1;
	}

	/* Allocate space for the pointer table */
	argtbl = (ArgStruct*) ealloc1(targc, sizeof(ArgStruct));

	/* Tabulate targv */
	tabulate(targc, targv);
}

#define PFNAME "par="
static char *getpfname (void)
{
	int i;
	int pfnamelen;

	pfnamelen = strlen(PFNAME);
	for (i = xargc-1 ; i > 0 ; i--) {
		if(!strncmp(PFNAME, xargv[i], pfnamelen)
		    && strlen(xargv[i]) != pfnamelen) {
			return xargv[i] + pfnamelen;
		}	
	}
	return NULL;
}

#define iswhite(c)	((c) == ' ' || (c) == '\t' || (c) == '\n')

/* 
 * Replace the whites by nulls.  If we see a non-white and the previous
 * char is a null, this signals the start of a string and we bump the count.
 */
static int white2null (char *str, int len)
{
	int i;
	int count;
	bool inquote = false;

	for (i = 1, count = 0; i < len; i++) {
		if (str[i]=='"') inquote=(inquote==true)?false:true;
		if (!inquote) {
			if (iswhite(str[i])) { /* Is this a new word ? */
				str[i] = '\0';
			} else if (!str[i-1]) { /* multiple whites */
				count++;
			}
		}
	}
	for (i = 1, inquote=false; i < len; i++) {
		if (str[i]=='"') inquote=(inquote==true)?false:true;
		if (inquote) {
			if (str[i+1]!='"') {
				str[i] = str[i+1];
			} else {
				str[i] = '\0';
				str[i+1] = '\0';
				inquote = false;
			}
		}
	}
	str[len] = '\0';
	return count;
}

/* Tabulate parameters */
static int tabulate (int argc, char **argv)
{
	int i;
	char *eqptr;

	for (i = 0, nargs = 0 ; i < argc; i++) {
		eqptr = strchr(argv[i], '=');
		if (eqptr) {
			argtbl[nargs].name = argv[i];
			argtbl[nargs].asciival = eqptr + 1;
			*eqptr = (char)0;

			/* Debugging dump */
/* 			fprintf(stderr, */
/* 			"argtbl[%d]: name=%s asciival=%s\n", */
/* 			nargs,argtbl[nargs].name,argtbl[nargs].asciival); */

			nargs++;
		}
	}
}

/* Count characters in a string */
static int ccount (char c, char *s)
{
	int i, count;
	for (i = 0, count = 0; s[i] != 0; i++)
		if(s[i] == c) count++;
	return count;
}


#ifdef TEST
#define N 100
main(int argc, char **argv)
{
	char *s;
	short h, vh[N];
	unsigned short u, vu[N];
	long l, vl[N];
	unsigned long v, vv[N];
	int i, vi[N], ipar, npar, nval;
	unsigned int p, vp[N];
	float f, vf[N];
	double d, vd[N];

	initargs(argc, argv);

	/* int parameters */
	npar = countparname("i");
	printf("\nnumber of i pars = %d\n",npar);
	for (ipar=1; ipar<=npar; ++ipar) {
		getnparint(ipar,"i",&i);
		printf("occurence %d of i=%d\n",ipar,i);
	}
	if (getparint("i", &i))	
		printf("last occurence of i=%d\n",i);
	npar = countparname("vi");
	printf("number of vi pars = %d\n",npar);
	for (ipar=1; ipar<=npar; ++ipar) {
		nval = countnparval(ipar,"vi");
		printf("occurence %d has %d values\n",ipar,nval);
		nval = getnparint(ipar,"vi",vi);
		printf("vi=");
		for (i=0; i<nval; i++)
			printf("%d%c",vi[i],i==nval-1?'\n':',');
	}
	if (npar>0) {
		nval = countparval("vi");
		printf("last occurence has %d values\n",nval);
		getparint("vi",vi);
		printf("vi=");
		for (i=0; i<nval; i++)
			printf("%d%c",vi[i],i==nval-1?'\n':',');
	}

	/* float parameters */
	npar = countparname("f");
	printf("\nnumber of f pars = %d\n",npar);
	for (ipar=1; ipar<=npar; ++ipar) {
		getnparfloat(ipar,"f",&f);
		printf("occurence %d of f=%g\n",ipar,f);
	}
	if (getparfloat("f", &f))	
		printf("last occurence of f=%g\n",f);
	npar = countparname("vf");
	printf("number of vf pars = %d\n",npar);
	for (ipar=1; ipar<=npar; ++ipar) {
		nval = countnparval(ipar,"vf");
		printf("occurence %d has %d values\n",ipar,nval);
		nval = getnparfloat(ipar,"vf",vf);
		printf("vf=");
		for (i=0; i<nval; i++)
			printf("%g%c",vf[i],i==nval-1?'\n':',');
	}
	if (npar>0) {
		nval = countparval("vf");
		printf("last occurence has %d values\n",nval);
		getparfloat("vf",vf);
		printf("vf=");
		for (i=0; i<nval; i++)
			printf("%g%c",vf[i],i==nval-1?'\n':',');
	}

	/* string parameters */
	npar = countparname("s");
	printf("\nnumber of s pars = %d\n",npar);
	for (ipar=1; ipar<=npar; ++ipar) {
		getnparstring(ipar,"s",&s);
		printf("occurence %d of s=%s\n",ipar,s);
	}
	if (getparstring("s", &s))	
		printf("last occurence of s=%s\n",s);
	
	if( auxgetpar("aux","i","i",&i) ) 
		printf("in aux last occurence of i=%d\n",i);
	if( auxgetpar("aux","f","f",&f) )
		printf("in aux last occurence of f=%f\n",f);
	if( auxgetpar("aux","s","s",&s) ) 
		printf("in aux last occurence of s=%s\n",s);

	if (getparint("i", &i))	
		printf("last occurence of i=%d\n",i);
	if( auxgetpar("aux","junk","s",&s) ) 
		printf("in aux last occurence of junk=%s\n",s);

	return EXIT_SUCCESS;
}
#endif
