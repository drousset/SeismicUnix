h52735
s 00016/00003/00453
d D 1.2 88/05/25 06:52:59 shuki 2 1
c umainseq
e
s 00456/00000/00000
d D 1.1 88/04/14 13:52:33 shuki 1 0
c date and time created 88/04/14 13:52:33 by shuki
e
u
U
f e 0
t
T
I 1
/*
 * nmo - normal moveout
 *
 * Technical Reference:
 *
 * Credits:
 *
*/

#include <stdio.h>
#include <math.h>
#include "../include/su.h"

#ifdef HP
#define _SETGAIN_	setgain
#define _TRPENDS_	trpends
#define _CONNMO_	connmo
#define _LINTRP_	lintrp
#define _STRTCH_	strtch
#define _SNCTRP_	snctrp
#define _VARNMO_	varnmo
#else
#define _SETGAIN_	setgain_
#define _TRPENDS_	trpends_
#define _CONNMO_	connmo_
#define _LINTRP_	lintrp_
#define _STRTCH_	strtch_
#define _SNCTRP_	snctrp_
#define _VARNMO_	varnmo_
#endif

extern char *sdoc;
extern int xargc;
extern char **xargv;
extern bool verbose;

/*********************** self documentation **********************/
static char *lsdoc = 
"SUNMO - NMO and Inverse NMO with constant or variable velocity		\n\
									\n\
sunmo <stdin >stdout v= [stretch=1.5 verbose=0 tv= vfile= nw=] 		\n\
									\n\
							        	\n\
	Constant velocity nmo is with v=v0.				\n\
							        	\n\
	To specify a v(t) velocity give the optional			\n\
	v=v1,...vn and tv=t1,...,tn pairs of velocity-time picks.	\n\
									\n\
	To specify a v(t,x) velocity give a velocity file,		\n\
	v=filename or vfile=filename. Velocity file format:		\n\
		m midpoint1						\n\
			tv1.1    v1.1					\n\
			tv1.2    v1.2					\n\
	  	m midpoint2						\n\
			tv2.1    v2.1					\n\
			tv2.2    v2.2					\n\
							        	\n\
	The midpoints in the vfile should be in increasing order	\n\
							        	\n\
	stretch is the maximum nmo stretch for hard mute.		\n\
									\n\
	The default interpolation is linear.  To specify the		\n\
	alternative sinc interpolation, give the optional		\n\
	parameter, nw, that specifies the number of points		\n\
	to use.								\n\
									\n\
	verbose (1=YES, 0=NO) for echoing of info to stderr.		\n\
									\n\
";
/*****************************************************************/


static float stretch;	/* hard mute limit 			*/

D 2
trseq(itr,atr,abh)
int itr;
Sutrace *atr;
E 2
I 2
 
/* PRE PROCESSING */
prep(abh,atrin,aatrout,infd,outfd)
E 2
Subhed *abh;
I 2
Sutrace *atrin,**aatrout;
int infd,outfd;
E 2
{
I 2
    *aatrout = atrin;
}
 
/* TRACE SEQUENTIAL TPOW GAIN PROCESSING */
trseq(itr,atr,atrout,abh)
int itr;    
Sutrace *atr,*atrout;
Subhed *abh;
{
E 2
	static float os;	/* ... and reciprocal			*/
	static int it, nt;	/* number of samples 			*/
	static float dt;	/* sampling interval 			*/
	static float tmax;	/* nt*dt				*/
	static float *v;	/* velocity picks for vel model		*/
	static float *tv;	/* time picks for vel model		*/
	static int *itv;
	static unsigned nv;	/* number of v's read by getpar		*/
	static int nvmax,iv;
	int ntv;	/* number of tv's read by getpar	*/
	int j;		/* counter over v's and tv's		*/
	static int nw;		/* number of interpolation weights	*/
	static int t0, te;	/* smallest, largest non-zero weight row*/
	float x;	/* offset from trace header		*/
	static float *tnmo;	/* NMO times */
	static float *gain;	/* soft and hard mute */
	static int *itnmo;	/* base point for the interpolation, i.e.,
			   we use the indices itnmo+1, ...	*/
	static float *w;	/* interpolation weights		*/
	static float *nmoed;	/* NMO stretched data			*/
	static float *vvec;	/* velocity model v[nt]			*/
	static float *ovv;	/* sloth model -- sloth=sgn(v)/(v*v) */
	static int (*trpfun)();	/* interpolation function*/
	int _SNCTRP_(),_LINTRP_();
	static int (*nmofun)();	/* NMO function	 */
	int _CONNMO_(),_VARNMO_();
	char vfname[50];			/* name of vfile */
	char s1[50],s2[50];			/* string buffer */
	static enum {const,voft,vofxt} velvar;	/* velocity variations flag */
	static struct vxtstrct { int cmp; float *ovv;} *vxt;
	FILE *pvelf;	/* for the velocity file */
	int ncmp;	/* no of cmps for which velocity is specified */
	static int oldoffset,oldcmp;
	float a,b,c;
	bool newvelfun,opened,hit;

	/* First trace */
	if(itr==0) {
		dt = atr->dt/1000000.0; /* tr.dt in microsec */
		nt = atr->ns;
		tmax = nt*dt;
		if (verbose) fprintf(stderr, "\tdt=%g nt=%d tmax=%g\n",dt,nt,tmax);

		/* Stretch reciprocal for muting */
		os = 1.0/stretch;

		/* Select type of interpolation */
		if (lgetpar("nw", &nw)) { /* sinc interpolation */
			trpfun = _SNCTRP_;
			if (nw < 2) {
		   	err(__FILE__,__LINE__,"nw = %d--must be >= 2 for sinc interpolation", nw);
			}
			if (nw % 2) {
				nw++;
				warn(__FILE__,__LINE__,"changed nw (must be even)");
			}
			if (verbose) {
				fprintf(stderr,
			    	"\tinterpolation = sinc with %d points\n", nw);
			}
		} else { /* default linear interpolation */
			trpfun = _LINTRP_;
			nw = 2;
			if (verbose) {
				fprintf(stderr, "\tinterpolation = linear\n");
			}
		}

		/* Allocate space for nmo operations */
		tnmo  = (float *) malloc((unsigned) (nt * sizeof (float)));
		gain  = (float *) malloc((unsigned) (nt * sizeof (float)));
		itnmo = (int *)   malloc((unsigned) (nt * sizeof (int)));
		w     = (float *) malloc((unsigned) (nt * nw * sizeof (float)));
		nmoed = (float *) malloc((unsigned) (nt * sizeof (float)));
		vvec  = (float*)  malloc((unsigned) (nt * sizeof(float)));
		ovv   = (float*)  malloc((unsigned) (nt * sizeof(float)));

		/* Allocate space for velocity model using maxgetpar() */
		nv = maxgetpar();
		v  = (float *) malloc((unsigned) (nv*sizeof (float)));

		/* Velocity model */
		if(sgetpar("v",vfname)&&!sgetpar("tv",s1)) {
			/* That was illegal unless v is a file or v is constatnt */
			pvelf = fopen(vfname,"r");		/* Try to open */
			if(pvelf != NULL) {			/* A file ? */
				opened = true;
			} else if(fgetpar("v",v)==1) {	/* Constant? */
				opened = false;
			} else {				/* Illegal */
				warn(__FILE__,__LINE__,"must specify velocity model");
				selfdoc();
			}
		}
		if(opened||sgetpar("vfile",vfname)) {	/* IF VFILE */
	
			/* Set lateral velocity variations flag */
			velvar = vofxt;
	
			/* Set vertical vleocity variations nmo function */
			nmofun = _VARNMO_;
	
			/* Open vfname */
			if(!opened) {
				pvelf = fopen(vfname,"r");
				if(pvelf==NULL) err(__FILE__,__LINE__,"can't fopen(%s,%s)",vfname,"r");
			}
	
			if (verbose) {
				fprintf(stderr,
				"\tlateraly varying velocity mofel from vfile=%s\n",
						vfname);
			}
	
			/* Scan pass 1 (for ncmp and nvmax) */
			for(ncmp=0,nvmax=0,nv=0;fscanf(pvelf,"%s %s",s1,s2)!=EOF;) {
				if(*s1=='m') { /* New cmp */
					ncmp++;
					nv = 0;
				} else nv++;
				nvmax = MAX(nvmax,nv);
			}
			if(verbose)
				fprintf(stderr,"ncmp=%d	nvmax=%d\n",ncmp,nvmax);
	
			/* Allocate space for vxt */
			vxt = (struct vxtstrct*)
				malloc((unsigned)(ncmp*sizeof(struct vxtstrct)));
			for(j=0;j<ncmp;j++)
				vxt[j].ovv = (float*)
					malloc((unsigned)(nt*sizeof(float)));
	
			/* Allocate space for v and tv */
			v  = (float *) malloc((unsigned) (nvmax*sizeof (float)));
			tv = (float *) malloc((unsigned) (nvmax*sizeof (float)));
			itv = (int *)  malloc((unsigned) (nvmax*sizeof (int)));
	
			/* scan pass 2 to read the vfile and convert it to vxt */
			rewind(pvelf);
			fscanf(pvelf,"%s %s",s1,s2);
			/* fprintf(stderr,"Read %s %s\n",s1,s2); */
			for(j=0;j<ncmp;j++) {		/* Loop over midpoint */
				if(*s1!='m') err(__FILE__,__LINE__,"Bad velfile (%s)",vfname);
				vxt[j].cmp = atoi(s2);
				/* fprintf(stderr,"\tvxt[%d].cmp=%d\n",j,vxt[j].cmp); */
				for(nv=0;nv<=nvmax;nv++) {/* Loop over velocities */
					if(fscanf(pvelf,"%s %s",s1,s2)==EOF) break;
					/* fprintf(stderr,"Read %s %s\n",s1,s2); */
					if(*s1=='m') break;
					tv[nv] = atof(s1);
					v[nv] = atof(s2);
					/* fprintf(stderr,"\ttv[%d]=%f\tv[%d]=%f\n",nv,tv[nv],nv,v[nv]); */
				}
	
				/* Normalize v and tv so that dt = 1 */
					for (iv=0;iv<nv;iv++) {
						v[iv] *= dt;
						itv[iv] = tv[iv]/dt;
				}

				/* Interpolate velocity model */
				vintrp(vxt[j].ovv,nt,v,itv,iv);

				/* Convert to sloth */
				for(it=0;it<nt;it++) {
					/* fprintf(stderr,"vxt[%d].ovv[%d]=%f\n",j,it,vxt[j].ovv[it]); */
					vxt[j].ovv[it] = 1.0/
						(fabs(vxt[j].ovv[it])*vxt[j].ovv[it]);
				}
			}
			fclose(pvelf);
		} else {			/* NOT VFILE */
			nv = fgetpar("v",v);
			if(!nv) {
				warn(__FILE__,__LINE__,"velocity model unspecified");
				selfdoc();
			}
			/* Check that user-supplied v's are nonzero. */
			for (j = 0; j < nv; j++) if ( v[j] == 0.0) {
				err(__FILE__,__LINE__,"v's must be non zero");
			}
			tv  = (float *) malloc((unsigned)(nv*sizeof(float)));
			itv = (int *)   malloc((unsigned)(nv * sizeof (int)));

			ntv = fgetpar("tv", tv);

			/* Test for monotonicity */
			for (j = 0; j < ntv-1; j++) {
				if (tv[j+1] <= tv[j]) {
			   	err(__FILE__,__LINE__,"tv's must increase: tv[%d]=%f tv[%d]=%f",
						j,tv[j],j+1,tv[j+1]);
				}
			}
			if(nv==1) {		/* CONSTANT VELOCITY */
				/* Convert to sloth */
				*ovv = 1.0/(fabs(*v) * *v * dt * dt);
				velvar = const;
				nmofun = _CONNMO_;
				if (ntv) warn(__FILE__,__LINE__,"tv ignored (constant velocity)");
				if (verbose) {
					fprintf(stderr,"\tconstant velocity nmo v=%g\n",v[0]);
				}
			} else {	/* NOT CONSTANT, MUST BE V(T) */
				velvar = voft;
				nmofun = _VARNMO_;

				if (!ntv) {
					warn(__FILE__,__LINE__,"for variable velocity, need tv=");
					selfdoc();
				} else if (ntv != nv) {
					err(__FILE__,__LINE__,"number of tv's (%d) and v's (%d) unequal",ntv, nv);
				}
				if (verbose) {
					fprintf(stderr,"\tvertically varying velocity nmo\n");
					for (j = 0; j < nv; j++) {
						fprintf(stderr,"\ttv=%.2f\tv=%g\n",tv[j],v[j]);
					}
				}
				/* Normalize v and tv so that dt = 1 */
				for (j=0;j<nv;j++) {
					v[j] *= dt;
					itv[j] = tv[j]/dt;
				}

				/* Interpolate velocity model */
				vintrp(vvec,nt,v,itv,nv);

				/* Convert to sloth */
				for(j=0;j<nt;j++) {
					ovv[j] = 1.0/(fabs(vvec[j])*vvec[j]);
				}

			} /* END ELSE (NOT CONSTANT, MUST BE V(T)) */

		} /* ENDELSE (NOT VFILE) */

		oldcmp = atr->cdp - 1;
		oldoffset = atr->offset - 1;

	}	/* ENDIF FIRST TRACE */


	if(verbose)
	fprintf(stderr,"\nsx=%d gx=%d cdp=%d offset=%d\n",atr->sx,atr->gx,atr->cdp,atr->offset);

	x = atr->offset;

	/* New velocity function ? */
 	if(velvar==vofxt&&atr->cdp!=oldcmp) {

		newvelfun = true;

		/* Find j = midpoint index in the velocity model */
			for(j=0;j<ncmp;j++) {

/* 			fprintf(stderr,"j=%d vxt[%d].cmp=%d\n",j,j,vxt[j].cmp); */

			if(vxt[j].cmp==atr->cdp) {	/* Hit */
				hit = true;
				break;
			} else if(vxt[j].cmp>atr->cdp) {	/* Passed */
				hit = false;
				break;
			}
		}

		if(j<0||j>ncmp) err(__FILE__,__LINE__,"Illigal j=%d",j);

/* 		fprintf(stderr,"j=%d hit=%d\n",j,hit); */
/* 		fprintf(stderr,"vxt[%d].cmp=%d atr->cdp=%d vxt[%d].cmp=%d\n",j-1,vxt[j-1].cmp,atr->cdp,j,vxt[j].cmp); */

		/* Now that j is known, find the sloth for the cdp */
		if(hit) {		/* exact */
			bcopy(vxt[j].ovv,ovv,nt*4);
		} else if(j==0) {	/* end */
			bcopy(vxt[0].ovv,ovv,nt*4);
		} else if (j==ncmp) {	/* end */
			bcopy(vxt[ncmp-1].ovv,ovv,nt*4);
		} else {		/* linearly interpolate */
			a = vxt[j].cmp - atr->cdp;
			b = atr->cdp - vxt[j-1].cmp;
			c = a + b;
			a /= c;
			b /= c;
/* 			fprintf(stderr,"a=%f b=%f\n",a,b); */
			for(it=0;it<nt;it++) {
		   	ovv[it]=a*vxt[j-1].ovv[it]+b*vxt[j].ovv[it];
			}
/* 			fprintf(stderr," ovv[0]=%g = a * vxt[%d].ovv[0]=%g + b * vxt[%d].ovv[0]=%g\n",ovv[0], j-1, vxt[j-1].ovv[0], j, vxt[j].ovv[0]); */
		}
	} else newvelfun = false;

	/* New nmo function ? */
	if(newvelfun||atr->offset!=oldoffset) {

		/* Calculate nmo time tnmo */
		(*nmofun)(tnmo, &nt, ovv, &x);
		_SETGAIN_(tnmo,gain,&nt);
		hardmute(gain,nt,os);

		for(j=0;j<nt;j+=nt/20)
/* 			fprintf(stderr,"tnmo[%d]=%g\tgain=%g\n",j,tnmo[j],gain[j]); */

		/* Calculate the interpolation coefficients */
		(*trpfun)(itnmo, w, tnmo, &nt, &nw,gain);
		_TRPENDS_(itnmo,w,&t0,&te,&nt,&nw);
	}

	/* Perform the NMO; put new data into nmoed */
	_STRTCH_(nmoed, atr->data, &nt, &t0, &te, itnmo, w, &nw);

	/* Overwrite the segy data */
	bcopy(nmoed, atr->data, nt * sizeof (float));

	oldoffset = atr->offset;
	oldcmp = atr->cdp;

	return(1);
}

/****************************************************************/

hardmute(g,n,os)
int n;
float *g,os;
{
	int i;
	for(i=0;i<n;i++) {
		if(g[i]<os) g[i] = 0.0;
	}
}

/****************************************************************/

vintrp(vvec,nt,v,itv,nv)
int nt,nv,*itv;
float *vvec,*v;
{
	int i,j,minnttv;
	for(i=0;i<itv[0];i++) {
		vvec[i] = v[0];
	}
	minnttv = MIN(nt,itv[nv-1]);
	for(;i<minnttv;i++) {
		for(j=1;i>=itv[j];j++);
		vvec[i] = v[j-1]+(i-itv[j-1])*(v[j]-v[j-1])/(itv[j]-itv[j-1]);
	}
	for(;i<nt;i++) {
		vvec[i] = v[nv-1];
	}
}

/* INITIALIZE SELF DOCUMENTATION */
inits()
{
	int c;

	sdoc = lsdoc;

	/* GET OPTIONS */
	while( (c=getopt(xargc,xargv,"v"))!=EOF) {
		switch(c) {
		case 'v':
			verbose = true;
			break;
		case '?':
			warn(__FILE__,__LINE__,"getopt returned '?'");
		}
	}

	/* GET PARAMETERS */
	stretch = 1.5; fgetpar("stretch", &stretch);
}

/* ADD HISTORY TO ASCII HEADER */
/* 	hispr(outfd, "\tInterpolation=\n"); */
/* 	hispr(outfd, "\tMute on stretch=%f\n",stretch); */
I 2

postp(){}
E 2
E 1
