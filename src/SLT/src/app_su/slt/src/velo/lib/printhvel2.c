
#include "velo.h"
#include "par.h"

/* print second type of handvel cards */
/* cdplbl = line*10000+xline */
/* zhiming li */

void printhvel2(float line,float xline,int ntv,
	float *tout,float *vout,FILE *outfp){

	int ic, ip, nc;
	float p1, p2;
	int cdplbl, iy, ix;

	iy = (int) (line+0.5);
	ix = (int) (xline+0.5); 

	cdplbl = iy * 10000 + ix;

	/*
	if(ntv>0) fprintf(outfp,"HANDVEL %9d%9d\n",
		(int)(line+0.5),(int)(xline+0.5));
	*/

	if(ntv>0) fprintf(outfp,"HANDVEL %d\n",cdplbl);
	for(ic=0;ic<ntv;ic=ic+4) {

                nc = 4;
                if(ic+nc>ntv) nc = ntv - ic;

                for (ip=0;ip<nc;ip++) {
                    	p1 = tout[ic+ip];
                    	p2 = vout[ic+ip];
                        fprintf(outfp,"%8.1f%8.1f",p1,p2);
                }
                fprintf(outfp,"\n");
       	}
}

