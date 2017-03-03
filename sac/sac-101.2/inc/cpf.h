/* ../../inc/cpf.h */

#define	MNUMERICABBREVS	5
#define	MNUMERICFUNCS	22
#define	MSTRINGFUNCS	10



struct t_kmcpf {		/* these vars are initialized in ncfp/inicpf.c */
	char	knoval[9];	/* designates that a particular macro keyword has
				   no current value */

	byte	kbb,		/* blackboard variable key */
		khdr,		/* header variable key */
		karg,		/* argument key */
		kescape,	/* escape character */
		kfunctionbegin,	/* beginning of inline function */
		kfunctionend;	/* end of inline function */

	char	kstringfuncs[MSTRINGFUNCS][9],
		knumericfuncs[MNUMERICFUNCS][9], 
		knumericabbrevs[MNUMERICABBREVS][9],
		kvarsname[9];

        char	kinputline[MCMSG+1];	/* processed input line */
	}   kmcpf;

struct t_cmcpf {
	long int nstringfuncs, nstringargs[MSTRINGFUNCS], nnumericfuncs, 
	 nnumericargs[MNUMERICFUNCS], nnumericabbrevs, inumericabbrevs[MNUMERICABBREVS], 
	 nmacrolevel;
	long lmacrostatus;
	}	cmcpf;


#ifdef DOINITS
long *const Inumericabbrevs = &cmcpf.inumericabbrevs[0] - 1;
long *const Nnumericargs = &cmcpf.nnumericargs[0] - 1;
long *const Nstringargs = &cmcpf.nstringargs[0] - 1;
#else
extern long *const Inumericabbrevs;
extern long *const Nnumericargs;
extern long *const Nstringargs;
#endif

