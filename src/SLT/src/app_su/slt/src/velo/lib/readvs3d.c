#include "velo.h"
#include "par.h"
/* read in velocity picks from input file pointer infp (VS3D card dataset) */
/* input:								*/
/*	infp			file pointer for VELF card dataset 	*/
/*	maxp			maximum number of t-v pairs per (x,y) 	*/
/*	maxnxy			maximum number of (x,y)'s of VS3D cards	*/
/* ouput:								*/
/*	xs[maxnxy]		x location of VS3D card 		*/	
/*	ys[maxnxy]		y location of VS3D card 		*/	
/*	ts[maxnxy][maxp]	time of picks (ms)			*/
/*	vs[maxnxy][maxp]	velocity of picks			*/
/*	nxy			total number of (x,y)'s found in VS3D cards*/
/*	nps[maxnxy]		number t-v pairs per (x,y)		*/
/*									*/	
/* author:	Zhiming Li		6/92	      			*/

void vs3dread(FILE *infp, float *xs, float *ys, float *ts, float *vs, 
	int *nxy, int *nps, int maxp, int maxnxy) {

     	int icmax, ic, cdpnow=0, cdppre=-1, cdpchange=0, jc=0;
	float i2, i3;
	int i;
	char *cbuf, x[9], y[9], velo[9], time[9];
	int cardfound=0;
	float xnow, ynow, xpre, ypre;

	icmax = maxnxy * maxp;

	cbuf = (char *) malloc(81*sizeof(char));

	/* rewind infp */
	efseek(infp,0,0);

	for (ic=0;ic<icmax;ic++) {
		if(feof(infp) !=0) break;
		bzero(cbuf,81);
                fgets(cbuf,81,infp);

                if(strncmp(cbuf, "VS3D",4)==0) {
			cardfound = 1;

			x[8] = '\0';
			strncpy(x,&cbuf[8],8);
                	if(strncmp(x, "        ",8)!=0) {
                		xnow = atof(x);
			} else {
				xnow = xpre;
			}
			y[8] = '\0';
			strncpy(y,&cbuf[16],8);
                	if(strncmp(y, "        ",8)!=0) {
                		ynow = atof(y);
			} else {
				ynow = ypre;
			}

			if(cdppre == -1) {
				xpre = xnow;
				ypre = ynow;
				cdppre = 0;
			}

		 	if( (xpre==xnow && ypre==ynow) ) {
				cdpnow = cdppre;
			} else {
				cdpnow = cdppre + 1;
			}
		
			/* if cdp changes */
          		if (cdpnow != cdppre ) {
				if(cdpchange>=maxnxy) 
				err(" maximum number of functions exceeded ");
				if(jc>maxp) 
				err(" maximum number of t-v pairs exceeded ");
             			nps[cdpchange] = jc;
				xs[cdpchange] = xpre;
				ys[cdpchange] = ypre;
				cdpchange += 1;
				jc = 0;
             			cdppre = cdpnow;
				xpre = xnow;
				ypre = ynow;
          		}

			/* store read values in tpicks and vpicks arrays */
          		for(i=0;i<3;i++) {
				time[8] = '\0';
				strncpy(time,&cbuf[24+i*16],8);
				sscanf(time,"%f",&i2);
				/*
                        	i2 = atof(time);
				*/
				velo[8] = '\0';
				strncpy(velo,&cbuf[32+i*16],8);
				i3 = -999999.;
				sscanf(velo,"%f",&i3);
				/*
                        	i3 = atof(velo);
				*/

				if(i3==-999999.) break;

             			ts[jc+cdpchange*maxp] = i2;
             			vs[jc+cdpchange*maxp] = i3;
             			jc = jc + 1;
          		}
		}
	}
/* last input cdp location */
	if(cardfound==1) {
    		xs[cdpchange] = xpre;
    		ys[cdpchange] = ypre;
    		nps[cdpchange] = jc;
		*nxy = cdpchange + 1;
	} else {
		*nxy = 0;
	}
	if(*nxy>maxnxy) 
		err("number of (x,y) of VS3D cards exceeds %d \n",maxnxy);
	free(cbuf);
}
