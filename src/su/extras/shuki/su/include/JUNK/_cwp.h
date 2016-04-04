/* cwp.h - include file for CWP C programs
 *
 * $Author: shuki $
 * $Source: /src/segy/include/RCS/cwp.h,v $
 * $Revision: 1.56 $ ; $Date: 87/09/12 08:58:25 $
 * $State: Exp $
 */

/* #include <stdio.h> */
/* #include <math.h> */
/* #include <sys/file.h>		/* Includes <fcntl.h> */

#define		TRUE		1
#define		FALSE		0
#define		YES		1
#define		NO		0
#define		STDIN		0
#define		STDOUT		1
#define		STDERR		2

#define SGN(x)		( (x) < 0 ? -1.0 : 1.0)
#define ABS(x)		( (x) < 0 ? -(x) : (x) )
#define	MAX(x,y)	( (x) > (y) ? (x) : (y) )
#define	MIN(x,y)	( (x) < (y) ? (x) : (y) )

/* #define MUSTGETPAR(x,y,z) if(!getpar(x,y,z)) err("need %s=",x) */
#define MUSTIGETPAR(x,y) if(!igetpar(x,y)) err("need %s=",x)
#define MUSTFGETPAR(x,y) if(!fgetpar(x,y)) err("need %s=",x)
#define MUSTZGETPAR(x,y) if(!zgetpar(x,y)) err("need %s=",x)
#define MUSTHGETPAR(x,y) if(!hgetpar(x,y)) err("need %s=",x)
#define MUSTLGETPAR(x,y) if(!lgetpar(x,y)) err("need %s=",x)
#define MUSTSGETPAR(x,y) if(!sgetpar(x,y)) err("need %s=",x)

#define		LOWBYTE(w)	((w) & 0xFF)
#define 	HIGHBYTE(w)	LOWBYTE( (w) >>8)
#define		ISNEGCHAR(c)	((c) & 0x80)
#define		SIGNEXTEND(c)	(~0xFF | (int) (c))

typedef enum {false, true} bool;
typedef enum {BADFILETYPE = -1, TTY, DISK, DIRECTORY, TAPE, PIPE} filetype;

typedef union {
	char		*s;	/* Char pointer			*/
	short		*h;	/* Short int pointer		*/
	unsigned short	*u;	/* Unsigned short integer	*/
	long		*l;	/* Long int pointer 		*/
	unsigned long	*v;	/* Unsigned long integer 	*/
	int		*d;	/* Integer (whatever it is)	*/
	float		*f;	/* Float pointer 		*/
	double		*z;	/* Double pointer 		*/
} mixed;

typedef union {
	char		s[8];
	short		h;
	unsigned short	u;
	long		l;
	unsigned long	v;
	int		d;
	float		f;
	double		z;
} value;

int xargc; char **xargv;

extern int errno;

extern void syserr(), err(), warn(), selfdoc();
extern void puthdval(), gethdval(), gname();
extern void printfval(), fprintfval(), scanfval(), printftype();
extern void vsadd(), vsmul(), vsadd2(), vsmul2();
extern void vadd(), vsub(), vmul(), vdiv();
extern void vadd2(), vsub2(), vmul2(), vdiv2();
extern void vsqrt(), vabs();
extern void cvabs();
extern void dvsadd(), dvsmul(), dvsadd2(), dvsmul2();
extern void dvadd(), dvsub(), dvmul(), dvdiv();
extern void dvadd2(), dvsub2(), dvmul2(), dvdiv2();
extern void dvsqrt(), dvabs();
extern void ivsadd(), ivsmul(), ivsadd2(), ivsmul2();
extern void ivadd(), ivsub(), ivmul(), ivdiv();
extern void ivadd2(), ivsub2(), ivmul2(), ivdiv2();
extern void ivsqrt(), ivabs();
extern unsigned maxgetpar();
extern int syswarn(), getpar(), gettr(), puttr(), getindex();
extern int vtoi(), valcmp(), ivmax(), ivmin(), ivabsmx();
extern int ivsum(), ivl1(), ivl2(), ivprod();
extern long vtol();
extern float vtof(), vmax(), vmin(), vabsmx();
extern float vsum(), vl1(), vl2(), vprod();
extern double vtoz(), dvmax(), dvmin(), dvabsmax();
extern double dvsum(), dvl1(), dvl2(), dvprod();
extern char *getkey(), *hdtype();
extern filetype statfil();
