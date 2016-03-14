/* BHDRPKGE: */

/*----------------------------------------------------------------------
 * Copyright (c)       , 1991.
 * All rights reserved.
 *
 *  Zhiming Li,       
 *----------------------------------------------------------------------
 */

#include "su.h"
#include "segy.h"
#include "header.h"
#include "bhdr.h"

/* bhdrpkge - routines to access the SEGY BINARY header via the bhdr structure.
 *
 * getbhval - get a header word by index
 * putbhval - put a header word by index
 * getbhdval - get a header word by name
 * putbhdval - put a header word by name
 * bhdtype   - get the data type of a header word by name
 * getbkey   - get the name of a header from its index
 * getbindex - get the index from the name
 *
 * Returns:
 *	getbhval : void
 *	putbhval : void
 *	getbhdval: void
 *	putbhdval: void
 *	bhdtype  : pointer to a string designating the data type
 *	getbkey  : pointer to a string with the name of the keyword (or NULL)
 *	getbindex: integer index
 *
 * Synopsis:
 *	void getbhval(trace_ptr, index, val_ptr)
 *	char *trace_ptr;	pointer to SU_bhed
 *	int index;		index of key name in SEGY binary header
 *	Value *val_ptr;		union of simple data types
 *
 *	void putbhval(trace_ptr, index, val_ptr)
 *	char *trace_ptr;	pointer to SU_bhed
 *	int index;		index of key name in SEGY binary header
 *	Value *val_ptr;		union of simple data types
 *
 *	void getbhdval(trace_ptr, key, val_ptr)
 *	char *trace_ptr;	pointer to SU_bhed
 *	char *key;		key name in SEGY binary header
 *	Value *val_ptr;		union of simple data types
 *
 *	void putbhdval(trace_ptr, key, val_ptr)
 *	char *trace_ptr;	pointer to SU_bhed
 *	char *key;		key name in SEGY binary header
 *	Value *val_ptr;		union of simple data types
 *
 *	char *bhdtype(key)
 *	char *key;		key name in SEGY binary header
 *
 *	char *getbkey(index)
 *	int index;		zero-based index of keyword
 *
 *	int getbindex(key)
 *	char *key;		key name in SEGY binary header
 *
 * Notes:
 *	This package includes only those routines that directly access
 *	the "bhdr" structure.  
 *
 */


void getbhval(SU_bhed *tr, int index, Value *valp)
{
	char *tp = (char*) tr;

	switch(*(bhdr[index].type)) {
	case 's': (void) strcpy(valp->s, tp + bhdr[index].offs);  break;
	case 'h': valp->h = *((short*)  (tp + bhdr[index].offs)); break;
	case 'u': valp->u = *((ushort*) (tp + bhdr[index].offs)); break;
	case 'l': valp->l = *((long*)   (tp + bhdr[index].offs)); break;
	case 'v': valp->v = *((ulong*)  (tp + bhdr[index].offs)); break;
	case 'f': valp->f = *((float*)  (tp + bhdr[index].offs)); break;
	case 'd': valp->d = *((double*) (tp + bhdr[index].offs)); break;
	default: err("%s: %s: mysterious data type", __FILE__, __LINE__); break;
	}

	return;
}


void putbhval(SU_bhed *tr, int index, Value *valp)
{
	char *tp = (char*) tr;

	switch(*(bhdr[index].type)) {
	case 's': (void) strcpy(tp + bhdr[index].offs, valp->s);  break;
	case 'h': *((short*)  (tp + bhdr[index].offs)) = valp->h; break;
	case 'u': *((ushort*) (tp + bhdr[index].offs)) = valp->u; break;
	case 'l': *((long*)   (tp + bhdr[index].offs)) = valp->l; break;
	case 'v': *((ulong*)  (tp + bhdr[index].offs)) = valp->v; break;
	case 'f': *((float*)  (tp + bhdr[index].offs)) = valp->f; break;
	case 'd': *((double*) (tp + bhdr[index].offs)) = valp->d; break;
	default: err("%s: %s: mysterious data type", __FILE__, __LINE__);
	break;
	}

	return;
}


void getbhdval(SU_bhed *tr, char *key, Value *valp)
{
	int index = getbindex(key);
	char *tp = (char*) tr;

	if ( -1 == (index))
		err("%s: key word not in segy.h: '%s'", __FILE__, key);

	switch(*(bhdr[index].type)) {
	case 's': (void) strcpy(valp->s, tp + bhdr[index].offs);  break;
	case 'h': valp->h = *((short*)  (tp + bhdr[index].offs)); break;
	case 'u': valp->u = *((ushort*) (tp + bhdr[index].offs)); break;
	case 'l': valp->l = *((long*)   (tp + bhdr[index].offs)); break;
	case 'v': valp->v = *((ulong*)  (tp + bhdr[index].offs)); break;
	case 'f': valp->f = *((float*)  (tp + bhdr[index].offs)); break;
	case 'd': valp->d = *((double*) (tp + bhdr[index].offs)); break;
	default: err("%s: %s: mysterious data type", __FILE__, __LINE__);
	break;
	}

	return;
}


void putbhdval(SU_bhed *tr, char *key, Value *valp)
{
	int index = getbindex(key);
	char *tp = (char*) tr;

	if ( -1 == (index))
		err("%s: key word not in segy.h: '%s'", __FILE__, key);

	switch(*(bhdr[index].type)) {
	case 's': (void) strcpy(tp + bhdr[index].offs, valp->s);  break;
	case 'h': *((short*)  (tp + bhdr[index].offs)) = valp->h; break;
	case 'u': *((ushort*) (tp + bhdr[index].offs)) = valp->u; break;
	case 'l': *((long*)   (tp + bhdr[index].offs)) = valp->l; break;
	case 'v': *((ulong*)  (tp + bhdr[index].offs)) = valp->v; break;
	case 'f': *((float*)  (tp + bhdr[index].offs)) = valp->f; break;
	case 'd': *((double*) (tp + bhdr[index].offs)) = valp->d; break;
	default: err("%s: %s: mysterious data type", __FILE__, __LINE__);
	break;
	}

	return;
}


char *bhdtype(char *key)
{
	int index = getbindex(key);

	if (-1 == (index))
		err("%s: key word not in segy.h: '%s'", __FILE__, key);

	return bhdr[index].type;
}


char *getbkey(int index)
{
	return (index < SU_BNKEYS && index >= 0) ? bhdr[index].key : NULL;
}


int getbindex(char *key)	/* get index for this key */
{
	register int i;

	for (i = 0; i < SU_BNKEYS; i++)
		if (STREQ(bhdr[i].key, key))
			return i;	/* key found */

	/* not found */
	return -1;
}
