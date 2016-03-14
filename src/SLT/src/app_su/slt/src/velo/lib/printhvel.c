
#include "velo.h"
#include "par.h"

/* print handvel cards */
/* zhiming li */

void printhvel(int cdp, int ntv, float *tout, float *vout, FILE *outfp) {

	int ic, ip, nc;
	float p1, p2;

	if(ntv>0) fprintf(outfp,"HANDVEL %d\n",cdp);
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

