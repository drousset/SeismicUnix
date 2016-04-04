#include <stdio.h>
#include "../include/su.h"
talloc(abh,atr)
Subhed *abh;
Sutrace *atr;
{
	atr->data = (float*)malloc( (unsigned)(abh->ns*sizeof(float)) );
	if(atr->data==NULL) err("can't dalloc (ns=%d)",abh->ns);
}
