#include "velo.h"
#include "par.h"
/* read in velocity picks from input file pointer infp (HANDVEL card dataset) */
/* input:                                                               */
/*      infp                    file pointer for VELF card dataset      */
/*      maxnp                   maximum number of (t,v) pairs per cdp 	*/
/*      maxncdp                 maximum number of cdps 			*/
/* ouput:                                                               */
/*      cdp[maxncdp]            cdp of HANDVEL cards 	               	*/
/*      ts[maxncdp][maxnp]      time of picks (ms)                      */
/*      vs[maxncdp][maxnp]      velocity of picks                       */
/*      ncdp                    total number of cdp's found in HANVEL	*/
/*      nps[maxncdp]            number t-v pairs per cdp                */
/*                                                                      */
/* author:      Zhiming Li              10/94                           */

void hvelread(FILE *infp, int *cdp, float *ts, float *vs,
        int *ncdp, int *nps, int maxnp, int maxncdp) {

        int it, i, nxin, jv, icdpnow, icdp;
        int icmax,ic,nvt,ivelo;
        float *time4, *velo4;
	float *times, *velos;
	char *cbuf;

        icmax = maxncdp * maxnp;
        icdpnow = 0;
        nxin = 0;
        cbuf = (char*)malloc(81*sizeof(char));
        times = (float*)malloc(maxnp*sizeof(float));
        velos = (float*)malloc(maxnp*sizeof(float));
        time4 = (float*)malloc(4*sizeof(float));
        velo4 = (float*)malloc(4*sizeof(float));
        jv = 0;


    for (ic=0;ic<icmax;ic++) {
       	if (feof(infp) !=0 ) break;
	bzero(cbuf,81);
	fgets(cbuf,81,infp);
       	if ( cbuf[0]=='H' && cbuf[1]=='A' && cbuf[2]=='N' && cbuf[3]=='D' 
		&& cbuf[4]=='V' && cbuf[5]=='E' && cbuf[6]=='L' )  {
		icdp = 0;
		sscanf(cbuf+8,"%d",&icdp);
		if(icdp==0) err("cdp number can not be zero");
	  	if (icdpnow == 0 ) icdpnow = icdp;
	} else if(icdpnow!=0 && cbuf[0]!='*' ) { 
	  	if (icdp != icdpnow) {
			if(nxin>=maxncdp) 
			err(" maximum number of functions exceeded ");
			if(jv>maxnp) 
			err(" maximum number of t-v pairs exceeded ");
			nps[nxin] = jv;
			cdp[nxin] = icdpnow;
	      		nvt = jv;
			for(it=0;it<nvt;it++) {
				ts[it+nxin*maxnp] = times[it];
				vs[it+nxin*maxnp] = velos[it];
			}	
	      		nxin = nxin + 1;
	      		jv = 0;
	      		icdpnow = icdp;
		}

		for(i=0;i<4;i++) {
			time4[i] = 0.;
			velo4[i] = 0.;
		}
		sscanf(cbuf,"%f %f %f %f %f %f %f %f",
			&time4[0],&velo4[0],&time4[1],&velo4[1],
			&time4[2],&velo4[2],&time4[3],&velo4[3]);
	  	for(i=0;i<4;i++) {
	     		ivelo = velo4[i];
	     		if (ivelo == 0) break; 
	     		times[jv] = time4[i];
	     		velos[jv] = velo4[i];
	     		jv = jv + 1;
	     	}
	}
     }

     if (jv>0) {
	if(nxin>=maxncdp) 
	err(" maximum number of functions exceeded ");
	if(jv>maxnp) 
	err(" maximum number of t-v pairs exceeded ");
	nps[nxin] = jv;
	cdp[nxin] = icdp;
   	nvt = jv;
	for(it=0;it<nvt;it++) {
		ts[it+nxin*maxnp] = times[it];
		vs[it+nxin*maxnp] = velos[it];
	}	
	nxin = nxin + 1;
	jv = 0;
	icdpnow = icdp;
     }
     *ncdp = nxin;

	
     free(times);
     free(velos);
     free(cbuf);
     free(time4);
     free(velo4);
}
