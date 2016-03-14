
#include "velo.h"
#include "par.h"

/* print sc3d cards */
/* zhiming li */

void printsc3d(float s,float l,int nvt,float *times,float *scales,FILE *outfp) {

	int jc,ip,npout;
	float p1,p2; 


	for (jc=0;jc<(nvt+2)/3;jc++) {
       		if( (jc+1)*3 < nvt ) {
           		npout = 3;
           	} else {
              		npout = nvt - jc*3;
           	}
		if(jc==0) {
           		if(fabs(s)>=99999999.0 || fabs(l)>=99999999.0) { 
				fprintf(outfp, "SC3D    %8.3g%8.3g",s,l);
			} else if(fabs(s)>=999999.9 || fabs(l)>=999999.9) { 
				fprintf(outfp, "SC3D    %8d%8d",(int)s,(int)l);
			} else {
           			fprintf(outfp, "SC3D    %8.1f%8.1f",s,l);
			}
		} else {
           		fprintf(outfp, "SC3D                    ");
		}
           	for(ip=0;ip<npout;ip++) {
               		p1 = times[jc*3+ip];
               		p2 = scales[jc*3+ip];
               		fprintf(outfp,"%8.2f%8.3f",p1,p2);
           	}
           	fprintf(outfp,"\n");
       	}

}

