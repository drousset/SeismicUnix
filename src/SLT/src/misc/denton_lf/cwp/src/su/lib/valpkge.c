/* VALPKGE: $Revision: 1.8 $ ; $Date: 92/02/18 12:33:31 $	*/

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
 *----------------------------------------------------------------------
 */

#include "su.h"
#include "segy.h"
#include "header.h"

/* valpkge - routines to handle variables of type Value
 *
 * vtoi       - cast Value variable as an int
 * vtol       - cast Value variable as a long
 * vtof       - cast Value variable as a float
 * vtod       - cast Value variable as a double
 * atoval     - convert ascii to Value
 * valtoabs   - take absolute value of a Value variable
 * valcmp     - compare Value variables
 * getparval  - getpar a Value variable
 * printfval  - printf a Value variable
 * fprintfval - fprintf a Value variable
 * scanfval   - scanf a Value variable
 * printftype - printf for the type of a segy header word
 * printheader- display non-null header field values
 *
 *
 * Credits:
 *
 *	CWP: Jack, Shuki
 */


int vtoi(register String type, Value val)
{
	switch(*type) {
		case 's': return (int) val.s[0];
		case 'h': return (int) val.h;
		case 'u': return (int) val.u;
		case 'l': return (int) val.l;
		case 'v': return (int) val.v;
		case 'i': return       val.i;
		case 'p': return (int) val.p;
		case 'f': return (int) val.f;
		case 'd': return (int) val.d;
		default: err("vtoi: unknown type %s", type);
			 return 0;	/* for lint */
	}
}

long vtol(register String type, Value val)
{
	switch(*type) {
		case 's': return (long) val.s[0];
		case 'h': return (long) val.h;
		case 'u': return (long) val.u;
		case 'l': return        val.l;
		case 'v': return (long) val.v;
		case 'i': return (long) val.i;
		case 'p': return (long) val.p;
		case 'f': return (long) val.f;
		case 'd': return (long) val.d;
		default: err("vtol: unknown type %s", type);
			 return 0L;	/* for lint */
	}
}


float vtof(register String type, Value val)
{
	switch(*type) {
		case 's': return (float) val.s[0];
		case 'h': return (float) val.h;
		case 'u': return (float) val.u;
		case 'l': return (float) val.l;
		case 'v': return (float) val.v;
		case 'i': return (float) val.i;
		case 'p': return (float) val.p;
		case 'f': return         val.f;
		case 'd': return (float) val.d;
		default: err("vtof: unknown type %s", type);
			 return 0.0;	/* for lint */
	}
}

double vtod(register String type, Value val)
{
	switch(*type) {
		case 's': return (double) val.s[0];
		case 'h': return (double) val.h;
		case 'u': return (double) val.u;
		case 'l': return (double) val.l;
		case 'v': return (double) val.v;
		case 'i': return (double) val.i;
		case 'p': return (double) val.p;
		case 'f': return (double) val.f;
		case 'd': return          val.d;
		default: err("vtod: unknown type %s", type);
			 return 0.0;	/* for lint */
	}
}


int valcmp(register String type, Value val1, Value val2)
{
	switch(*type) {
		case 's': return strcmp(val1.s, val2.s);
		case 'h':
			if      ( val1.h < val2.h ) return -1;
			else if ( val1.h > val2.h ) return  1;
			else                        return  0;
		case 'u':
			if      ( val1.u < val2.u ) return -1;
			else if ( val1.u > val2.u ) return  1;
			else                        return  0;
		case 'l':
			if      ( val1.l < val2.l ) return -1;
			else if ( val1.l > val2.l ) return  1;
			else                        return  0;
		case 'v':
			if      ( val1.v < val2.v ) return -1;
			else if ( val1.v > val2.v ) return  1;
			else                        return  0;
		case 'i':
			if      ( val1.i < val2.i ) return -1;
			else if ( val1.i > val2.i ) return  1;
			else                        return  0;
		case 'f':
			if      ( val1.f < val2.f ) return -1;
			else if ( val1.f > val2.f ) return  1;
			else                        return  0;
		case 'd':
			if      ( val1.d < val2.d ) return -1;
			else if ( val1.d > val2.d ) return  1;
			else                        return  0;
		default: err("valcmp: unknown type %s", type);
			 return 0;	/* for lint */
	}
}


void printfval(register String type, Value val)
{
	switch(*type) {
	case 's':
		(void) printf("%s", val.s);
	break;
	case 'h':
		(void) printf("%d", val.h);
	break;
	case 'u':
		(void) printf("%d", val.u);
	break;
	case 'l':
		(void) printf("%ld", val.l);
	break;
	case 'v':
		(void) printf("%ld", val.v);
	break;
	case 'i':
		(void) printf("%d", val.i);
	break;
	case 'f':
		(void) printf("%f", val.f);
	break;
	case 'd':
		(void) printf("%f", val.d);
	break;
	default:
		err("printfval: unknown type %s", type);
	}

	return;
}


void fprintfval(FILE *stream, register String type, Value val)
{
	switch(*type) {
	case 's':
		(void) fprintf(stream, "%s", val.s);
	break;
	case 'h':
		(void) fprintf(stream, "%d", val.h);
	break;
	case 'u':
		(void) fprintf(stream, "%d", val.u);
	break;
	case 'l':
		(void) fprintf(stream, "%ld", val.l);
	break;
	case 'v':
		(void) fprintf(stream, "%ld", val.v);
	break;
	case 'i':
		(void) fprintf(stream, "%d", val.i);
	break;
	case 'f':
		(void) fprintf(stream, "%f", val.f);
	break;
	case 'd':
		(void) fprintf(stream, "%f", val.d);
	break;
	default:
		err("fprintfval: unknown type %s", type);
	}

	return;
}


void scanfval(register String type, Value *valp)
{
	switch(*type) {
	case 's':
		(void) scanf("%s", valp);
	break;
	case 'h':
	case 'u':
		(void) scanf("%hd", valp);
	break;
	case 'l':
	case 'v':
		(void) scanf("%ld", valp);
	break;
	case 'f':
		(void) scanf("%f", valp);
	break;
	case 'd':
		(void) scanf("%lf", valp);
	break;
	default:
		err("scanfval: unknown type %s", type);
	}

	return;
}


void printftype(register String key)
{
	switch(*hdtype(key)) {
	case 's':
		(void) printf("char\n");
	break;
	case 'h':
		(void) printf("short\n");
	break;
	case 'u':
		(void) printf("ushort\n");
	break;
	case 'l':
		(void) printf("long\n");
	break;
	case 'v':
		(void) printf("ulong\n");
	break;
	case 'i':
		(void) printf("int\n");
	break;
	case 'f':
		(void) printf("float\n");
	break;
	case 'd':
		(void) printf("double\n");
	break;
	default:
		err("printftype: unknown type %s", hdtype(key));
	break;
	}
	return;
}


/* Display non-null header field values */
void printheader(segy *tp)
{
	int i;			/* index over header fields		*/
	int j;			/* index over non-null header fields	*/
	Value val;		/* value in header field		*/
	String type;		/* ... its data type			*/
	String key;		/* ... the name of the header field	*/
	Value zeroval;		 /* zero value to compare with		*/

	zeroval.l = 0;
	j = 0;
	for (i = 0; i < SU_NKEYS; i++) {
		gethval(tp, i, &val);
		key = getkey(i);
		type = hdtype(key);
		if (valcmp(type, val, zeroval)) { /* not equal to zero */
			(void) printf(" %s=", key);
			printfval(type, val);
			if ((++j % 6) == 0) putchar('\n');
		}
	}
	putchar('\n');

	return;
}


/* Convert ascii to Value according to type of keyword */
void atoval(
String type,	/* type of header keyword		*/
String keyval,	/* value of header keyword as ascii 	*/
Value *valp	/* pointer to converted value		*/
)
{
	switch(*type) {
	case 's':
		(void) strcpy(valp->s, keyval);
	break;
	case 'h':
		valp->h = eatoh(keyval); 
	break;
	case 'u':
		valp->u = eatou(keyval); 
	break;
	case 'l':
		valp->l = eatol(keyval); 
	break;
	case 'v':
		valp->v = eatov(keyval); 
	break;
	case 'f':
		valp->f = eatof(keyval); 
	break;
	case 'd':
		valp->d = eatod(keyval); 
	break;
	default:
		err("%s: %s: mysterious data type: %s",
					__FILE__, __LINE__, keyval);
	break;
	}

	return;
}

/* Value getpar -- omitted string type for now */
void getparval(String name, String type, int n, Value *valp)
{
        register int k;
	short *h;
	unsigned short *u;
	long *l;
	unsigned long *v;
	int *i;
	unsigned int *p;
	float *f;
	double *d;
	
	switch(*type) {
        case 'h':
		h = (short*) ealloc1(n, sizeof(short));
		getparshort(name, h);  
		for (k = 0; k < n; ++k) valp[k].h = h[k];
	break;
        case 'u':
		u = (unsigned short*) ealloc1(n, sizeof(unsigned short));
		getparushort(name, u);  
		for (k = 0; k < n; ++k) valp[k].u = u[k];
	break;
        case 'l':
		l = (long*) ealloc1(n, sizeof(long));
		getparlong(name, l);  
		for (k = 0; k < n; ++k) valp[k].l = l[k];
	break;
        case 'v':
		v = (unsigned long*) ealloc1(n, sizeof(unsigned long));
		getparulong(name, v);  
		for (k = 0; k < n; ++k) valp[k].v = v[k];
	break;
        case 'i':
		i = (int*) ealloc1(n, sizeof(int));
		getparint(name, i);  
		for (k = 0; k < n; ++k) valp[k].i = i[k];
	break;  
        case 'p':
		p = (unsigned int*) ealloc1(n, sizeof(unsigned int));
		getparuint(name, p);  
		for (k = 0; k < n; ++k) valp[k].p = p[k];
	break;
        case 'f':
		f = (float*) ealloc1(n, sizeof(float));
		getparfloat(name, f);  
		for (k = 0; k < n; ++k) valp[k].f = f[k];
	break;  
        case 'd':
		d = (double*) ealloc1(n, sizeof(double));
		getpardouble(name, d);  
		for (k = 0; k < n; ++k) valp[k].d = d[k];
	break;  
        default:
                err("getparval: %d: mysterious type %s", __LINE__, type);
        }
}


/* Get absolute value for type value variable */
Value valtoabs(String type, Value val)
{
        switch(*type) {
        case 'u':       /* do nothing if unsigned */
        case 'v':
        case 'p':
        break;
        case 'h':
                val.h = ABS(val.h);
        break;
        case 'l':
                val.l = ABS(val.l);
        break;
        case 'i':
                val.i = ABS(val.i);
        break;
        case 'f':
                val.f = ABS(val.f);
        break;
        case 'd':
                val.d = ABS(val.d);
        break;
        default:
                err("valtoabs: %d: mysterious type %s", __LINE__, type);
        }

        return val;
}

