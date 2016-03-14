#include "usgridhd.h"
#include "gridhd.h"
#include "ghdr.h"
#include "grid.h"
#include "usgrid.h"
#include "par.h"

void ghed2usghed(ghed *gh, usghed *usgh) {
/*  convert scaled grid header to unscaled grid header 
  input:
	gh	--	scaled grid header 
  output:
	usgh	--	unscale grid header 
  author:
	Zhiming Li, 	11/4/92		      
*/
	
	float scale, tmp;
	int dtype;
	int n1, n2, n3, n4, n5;

	scale = gh->scale;
	if(scale==0.) {
		warn(" warning: gh.scale = 0.; 1 used \n");
		scale = 1.;
	}
	usgh->scale = scale;
	tmp = gh->dtype / scale + 0.5;
	dtype = (int) tmp;
	if(dtype!=1 && dtype!=2 && dtype!=4 && dtype!=8) {
		warn(" warning: invalid dtype=%d; dtype=4 used \n",dtype);
		dtype = 4;
	} 
	usgh->dtype = dtype;

	tmp  = gh->n1 / scale + 0.5;
	n1 = (int) tmp;
	if(n1==0) n1 = 1; 
	tmp  = gh->n2 / scale + 0.5;
	n2 = (int) tmp;
	if(n2==0) n2 = 1;
	tmp  = gh->n3 / scale + 0.5;
	n3 = (int) tmp;
	if(n3==0) n3 = 1;
	tmp  = gh->n4 / scale + 0.5;
	n4 = (int) tmp;
	if(n4==0) n4=1;
	tmp  = gh->n5 / scale + 0.5;
	n5 = (int) tmp;
	if(n5==0) n5 = 1;

	usgh->n1 = n1;
	usgh->n2 = n2;
	usgh->n3 = n3;
	usgh->n4 = n4;
	usgh->n5 = n5;

	usgh->o1 = gh->o1 / scale;
	usgh->o2 = gh->o2 / scale;
	usgh->o3 = gh->o3 / scale;
	usgh->o4 = gh->o4 / scale;
	usgh->o5 = gh->o5 / scale;

	usgh->d1 = gh->d1 / scale;
	usgh->d2 = gh->d2 / scale;
	usgh->d3 = gh->d3 / scale;
	usgh->d4 = gh->d4 / scale;
	usgh->d5 = gh->d5 / scale;

	usgh->dcdp2 = gh->dcdp2 / scale;
	usgh->ocdp2 = gh->ocdp2 / scale;
	usgh->dline3 = gh->dline3 / scale;
	usgh->oline3 = gh->oline3 / scale;

	usgh->gmin = gh->gmin / scale;
	usgh->gmax = gh->gmax / scale;

	tmp  = gh->orient / scale + 0.5;
	usgh->orient = (int) tmp;

	tmp  = gh->gtype / scale + 0.5;
	usgh->gtype = (int) tmp;

}

void usghed2ghed(usghed *usgh, ghed *gh) {
/*  convert unscaled grid header to scaled grid header 
  input:
	usgh	--	unscale grid header 
  output:
	gh	--	scaled grid header 
  author:
	Zhiming Li, 	11/4/92		      
*/
	
	float scale;

	scale = usgh->scale;
	if(scale==0.) {
		warn(" warning: gh.scale = 0.; 1 used \n");
		scale = 1.;
	}
	gh->scale = scale;
	gh->dtype = scale * usgh->dtype;
	gh->n1 = scale * usgh->n1;
	gh->n2 = scale * usgh->n2;
	gh->n3 = scale * usgh->n3;
	gh->n4 = scale * usgh->n4;
	gh->n5 = scale * usgh->n5;
	gh->o1 = scale * usgh->o1;
	gh->o2 = scale * usgh->o2;
	gh->o3 = scale * usgh->o3;
	gh->o4 = scale * usgh->o4;
	gh->o5 = scale * usgh->o5;
	gh->d1 = scale * usgh->d1;
	gh->d2 = scale * usgh->d2;
	gh->d3 = scale * usgh->d3;
	gh->d4 = scale * usgh->d4;
	gh->d5 = scale * usgh->d5;
	gh->dcdp2 = scale * usgh->dcdp2;
	gh->ocdp2 = scale * usgh->ocdp2;
	gh->dline3 = scale * usgh->dline3;
	gh->oline3 = scale * usgh->oline3;
	gh->gmin = scale * usgh->gmin;
	gh->gmax = scale * usgh->gmax;
	gh->orient = scale * usgh->orient;
	gh->gtype = scale * usgh->gtype;
}


int fgetusghdr(FILE *gfp, usghed *usgh) {
/* get unscaled grid header from a grid file pointer 
  input:
	gfp	--	grid file pointer 
  output:
	usgh	--	unscaled grid header 
  return:
	0	--	no error
	1	--	error 
  author:
	Zhiming Li, 	11/4/92		      
*/

	ghed gh; 

	int ierr;
	ierr = 0;
	ierr = fgetghdr(gfp,&gh);
	if (ierr==0) ghed2usghed(&gh,usgh);
	return ierr;
}		

int fputusghdr(FILE *gfp, usghed *usgh) {
/* convert unscaled grid header and output to a grid file
  input:
	gfp	--	grid file pointer 
	usgh	--	unscaled grid header 
  return:
	0	--	no error
	1	--	error 
  author:
	Zhiming Li, 	11/4/92		      
*/

	ghed gh; 

	int ierr;

	usghed2ghed(usgh,&gh);
	ierr = fputghdr(gfp,&gh);
	return ierr;
}		
