/* dump data to ximage (or xwigb) plot */
/*	d	---   input data array of n1 by n2 elements
*  	n1	---   number of points in 1st dimension
*	n2	---   number of points in 2nd dimension
*	dtype	---   display type (0=ximage otherwise=xwigb)
*	title	---   title of the plot 
*/
/* author:	J.D. Claude & Zhiming Li         	9/20/91	*/	

#include "xplot.h"
#include "par.h"

void dump2xplot(float *d,int n1,int n2,int dtype, char *title) {
     	char cmd[1024] ;
    	FILE *cmpfp;
	/* set up xplot command */
	if ( dtype == 0 ) {
     		sprintf(cmd,"ximage n1=%d n2=%d title=\"%s\"",n1,n2,title) ; 
	} else {
     		sprintf(cmd,"xwigb n1=%d n2=%d title=\"%s\"",n1,n2,title) ; 
	}
	/* open pipe */
	cmpfp = epopen(cmd,"w");
	efwrite(d,sizeof(float),n1*n2,cmpfp);
	efclose(cmpfp);
}
/* fortran callable */
void dump2xplot_(float *d,int *n1,int *n2,int *dtype,char *title) {
	dump2xplot(d,*n1,*n2,*dtype,title);
}


/* with axis labeling and grids */
void dump2xplotn(float *d,int n1,int n2,int dtype,char *title,
float f1, float f2, float d1, float d2, char *label1, char *label2,
char *grid1, char *grid2) {
     	char cmd[2048] ;
    	FILE *cmpfp;

	/* set up xplot command */
	if ( dtype == 0 ) {
     		sprintf(cmd,"ximage n1=%d n2=%d title=\"%s\" f1=%g f2=%g d1=%g d2=%g label1=\"%s\" label2=\"%s\" grid1=\"%s\" grid2=\"%s\" \n",
			n1,n2,title,f1,f2,d1,d2,label1,label2,grid1,grid2); 
	} else {
     		sprintf(cmd,"xwigb n1=%d n2=%d title=\"%s\" f1=%g f2=%g d1=%g d2=%g label1=\"%s\" label2=\"%s\" grid1=\"%s\" grid2=\"%s\"",
			n1,n2,title,f1,f2,d1,d2,label1,label2,grid1,grid2) ; 
	}

	/* open pipe */
	cmpfp = epopen(cmd,"w");
	efwrite(d,sizeof(float),n1*n2,cmpfp);
	efclose(cmpfp);
}
/* fortran callable */
void dump2xplotn_(float *d,int *n1,int *n2,int *dtype,char *title, 
float *f1, float *f2, float *d1, float *d2, char *label1, char *label2,
char *grid1, char *grid2) {
	dump2xplotn(d,*n1,*n2,*dtype,title,
		*f1,*f2,*d1,*d2,label1,label2,grid1,grid2);
}
