
/* dump data to psgraph plot */
/*	d	---   input data array of (x,y) pairs
*  	n(m)	---   number of points of each curve
*	m	---   number of curves 
*	title	---   title of the plot 
*	label1	---   lable 1 of graph		
*	label2	---   lable 2 of graph		
*       style   ---   normal(x horizontal, y vertical)
* 		      seismic (x vertical, y horizontal)
*/
/* author:	Zhiming Li         	2/22/93	*/	

#include "psplot.h"
#include "par.h"

void dump2psgraph(float *d,int *n,int m,char *title,
		char *label1,char *label2,char *style) {
     	char cmd[2048];
    	FILE *cmpfp;
	int nps,i,ilen,ipos;
	float a,b;
	/* set up psplot command */
     	sprintf(cmd,"psgraph n=");
	nps = 0;
	ipos = 10;
	for(i=0;i<m;i++) {
		sprintf(&cmd[ipos],"%d,",n[i]);
		a = n[i];
		b = log10(a);
		ilen = b + 1; 
		ipos = ipos + ilen + 1;
		nps = nps + n[i];
	}
     	sprintf(&cmd[ipos],
		" title=\"%s\" label1=\"%s\" label2=\"%s\" style=\"%s\" | lpr",
		title,label1,label2,style); 
	/* open pipe */
	cmpfp = epopen(cmd,"w");
	efwrite(d,sizeof(float),nps*2,cmpfp);
	efclose(cmpfp);
}

/* fortran callable */
void dump2psgraph_(float *d,int *n,int *m,char *title,
		char *label1,char *label2,char *style) {
	dump2psgraph(d,n,*m,title,label1,label2,style);
}
