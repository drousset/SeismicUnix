/* HDRPKGE: $Revision: 1.1 $ ; $Date: 2004/11/12 17:00:46 $	*/

/*----------------------------------------------------------------------
 * Copyright (c) Colorado School of Mines, 1989.
 * All rights reserved.
 *
 * This code is part of SU.  SU stands for Seismic Unix, a processing line
 * developed at the Colorado School of Mines, partially based on Stanford
 * Exploration Project (SEP) software.  Inquiries should be addressed to:
 *
 *  Jack K. Cohen, Center for Wave Phenomena, Colorado School of Mines,
 *  Golden, CO 80401  (jkc@dix.mines.colorado)
 *----------------------------------------------------------------------
 */

#include "su.h"
#include "segy.h"
#include "header.h"
#include "hdr.h"

/* hdrpkge - routines to access the SEGY header via the hdr structure.
 *
 * gethval - get a header word by index
 * puthval - put a header word by index
 * gethdval - get a header word by name
 * puthdval - put a header word by name
 * hdtype   - get the data type of a header word by name
 * getkey   - get the name of a header from its index
 * getindex - get the index from the name
 *
 * Returns:
 *	gethval : void
 *	puthval : void
 *	gethdval: void
 *	puthdval: void
 *	hdtype  : pointer to a string designating the data type
 *	getkey  : pointer to a string with the name of the keyword (or NULL)
 *	getindex: integer index
 *
 * Synopsis:
 *	void gethval(trace_ptr, index, val_ptr)
 *	char *trace_ptr;	pointer to segy
 *	int index;		index of key name in SEGY header
 *	Value *val_ptr;		union of simple data types
 *
 *	void puthval(trace_ptr, index, val_ptr)
 *	char *trace_ptr;	pointer to segy
 *	int index;		index of key name in SEGY header
 *	Value *val_ptr;		union of simple data types
 *
 *	void gethdval(trace_ptr, key, val_ptr)
 *	char *trace_ptr;	pointer to segy
 *	char *key;		key name in SEGY header
 *	Value *val_ptr;		union of simple data types
 *
 *	void puthdval(trace_ptr, key, val_ptr)
 *	char *trace_ptr;	pointer to segy
 *	char *key;		key name in SEGY header
 *	Value *val_ptr;		union of simple data types
 *
 *	char *hdtype(key)
 *	char *key;		key name in SEGY header
 *
 *	char *getkey(index)
 *	int index;		zero-based index of keyword
 *
 *	int getindex(key)
 *	char *key;		key name in SEGY header
 *
 * Credits:
 *	SEP: Einar
 *	CWP: Jack, Shuki
 *
 * Notes:
 *	This package includes only those routines that directly access
 *	the "hdr" (formerly "kdat") structure.  In particular, it does
 *	not include routines such as printfval, printftype, printfhead
 *	that use the routines in this package to indirectly access the
 *	hdr structure.
 *
 *      Note that while gethdval and puthdval are more convenient to use
 *	than gethval and puthval, they incur an inefficiency in the
 *	common case of iterating code over a set of traces with a fixed
 *      key or keys.  In such cases, it is advisable to set the index
 *      or indices outside the loop using getindex.
 *
 *	Some history:
 *	In the original SEP package, it was necessary to call lookey
 *	(the old name for getindex) before using other elements of the
 *	package since lookey had the job of initializing the table of
 *	offsets (even though lookey, itself, did not use the table).
 *
 *	The SEP documentation and code often implied that lookey
 *	and gkey used the offset in bytes in the segy header, actually
 *	they dealt with the index.
 *
 *	The shared "hdr" structure was formerly called "kdat" and the
 *	associated array was called "kd".
 *
 */


void gethval(segy *tr, int index, Value *valp)
{
	char *tp = (char*) tr;

	switch(*(hdr[index].type)) {
	case 's': (void) strcpy(valp->s, tp + hdr[index].offs);  break;
	case 'h': valp->h = *((short*)  (tp + hdr[index].offs)); break;
	case 'u': valp->u = *((ushort*) (tp + hdr[index].offs)); break;
	case 'l': valp->l = *((int*)    (tp + hdr[index].offs)); break;
	case 'v': valp->v = *((uint*)   (tp + hdr[index].offs)); break;
	case 'f': valp->f = *((float*)  (tp + hdr[index].offs)); break;
	case 'd': valp->d = *((double*) (tp + hdr[index].offs)); break;
	default: err("%s: %s: mysterious data type", __FILE__, __LINE__); break;
	}

	return;
}


void puthval(segy *tr, int index, Value *valp)
{
	char *tp = (char*) tr;

	switch(*(hdr[index].type)) {
	case 's': (void) strcpy(tp + hdr[index].offs, valp->s);  break;
	case 'h': *((short*)  (tp + hdr[index].offs)) = valp->h; break;
	case 'u': *((ushort*) (tp + hdr[index].offs)) = valp->u; break;
	case 'l': *((int*)    (tp + hdr[index].offs)) = valp->l; break;
	case 'v': *((uint*)   (tp + hdr[index].offs)) = valp->v; break;
	case 'f': *((float*)  (tp + hdr[index].offs)) = valp->f; break;
	case 'd': *((double*) (tp + hdr[index].offs)) = valp->d; break;
	default: err("%s: %s: mysterious data type", __FILE__, __LINE__);
	break;
	}

	return;
}


void gethdval(segy *tr, char *key, Value *valp)
{
	int index = getindex(key);
	char *tp = (char*) tr;

	if ( -1 == (index))
		err("%s: key word not in segy.h: '%s'", __FILE__, key);

	switch(*(hdr[index].type)) {
	case 's': (void) strcpy(valp->s, tp + hdr[index].offs);  break;
	case 'h': valp->h = *((short*)  (tp + hdr[index].offs)); break;
	case 'u': valp->u = *((ushort*) (tp + hdr[index].offs)); break;
	case 'l': valp->l = *((int*)    (tp + hdr[index].offs)); break;
	case 'v': valp->v = *((uint*)   (tp + hdr[index].offs)); break;
	case 'f': valp->f = *((float*)  (tp + hdr[index].offs)); break;
	case 'd': valp->d = *((double*) (tp + hdr[index].offs)); break;
	default: err("%s: %s: mysterious data type", __FILE__, __LINE__);
	break;
	}

	return;
}


void puthdval(segy *tr, char *key, Value *valp)
{
	int index = getindex(key);
	char *tp = (char*) tr;

	if ( -1 == (index))
		err("%s: key word not in segy.h: '%s'", __FILE__, key);

	switch(*(hdr[index].type)) {
	case 's': (void) strcpy(tp + hdr[index].offs, valp->s);  break;
	case 'h': *((short*)  (tp + hdr[index].offs)) = valp->h; break;
	case 'u': *((ushort*) (tp + hdr[index].offs)) = valp->u; break;
	case 'l': *((int*)    (tp + hdr[index].offs)) = valp->l; break;
	case 'v': *((uint*)   (tp + hdr[index].offs)) = valp->v; break;
	case 'f': *((float*)  (tp + hdr[index].offs)) = valp->f; break;
	case 'd': *((double*) (tp + hdr[index].offs)) = valp->d; break;
	default: err("%s: %s: mysterious data type", __FILE__, __LINE__);
	break;
	}

	return;
}


char *hdtype(char *key)
{
	int index = getindex(key);

	if (-1 == (index))
		err("%s: key word not in segy.h: '%s'", __FILE__, key);

	return hdr[index].type;
}


char *getkey(int index)
{
	return (index < SU_NKEYS && index >= 0) ? hdr[index].key : NULL;
}


int getindex(char *key)	/* get index for this key */
{
	register int i;

	for (i = 0; i < SU_NKEYS; i++)
		if (STREQ(hdr[i].key, key))
			return i;	/* key found */

	/* not found */
	return -1;
}
