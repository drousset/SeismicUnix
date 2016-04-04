#include <math.h>
#include <stdio.h>
#define MIN(a,b) a<b?a:b

gain(data,nt,nx)
float *data;
{
	char *agc="no             ",*cliping="yes         ";
	float tpow,epow,gpow,clip,getclip(),wagc;

	/* TPOW */
	if(fgetpar("tpow",&tpow)) {
		fprintf(stderr,"tpow=%f\n",tpow);
		dotpow(data,nt,nx,tpow);
	}

	/* EPOW */
	if(fgetpar("epow",&epow)) {
		fprintf(stderr,"epow=%f\n",epow);
		doepow(data,nt,nx,epow);
	}

	/* GPOW */
	if(fgetpar("gpow",&gpow)) {
		fprintf(stderr,"gpow=%f\n",gpow);
		dogpow(data,nt*nx,gpow);
	}

	/* AGC */
	wagc = 40.0;
	sgetpar("agc",agc);
	if(fgetpar("wagc",&wagc) || *agc == 'y' || *agc == 'Y') {
		fprintf(stderr,"wagc=%f\n",wagc);
		doagc(data,nt,nx,wagc);
	}

	/* NORMALIZE AND CLIP */
	sgetpar("cliping",cliping);
	if(*cliping=='y' || *cliping=='Y') {
		clip = getclip(data,nt*nx);
		balclip(data,nt*nx,clip);
	}
}

doagc(data,nt,nx,wagc)
int nt,nx;
float *data,wagc;
{
	int i,ix,np,ip;
	float *s1,*s2,*pdata,ro,eps;

	if (wagc == 0.0) return;
	eps = 1.0/wagc;
	ro  = 1.0 - eps;
	s1  = (float*) malloc(nt*sizeof(float));
	s2  = (float*) malloc(nt*sizeof(float));

	np = 2;		lgetpar("npasses",&np);

	for (ix=0,pdata=data;ix<nx;pdata+=nt,ix++) {

		for (i=0;i<nt;i++) {
			s1[i] = fabs(pdata[i]);
		}
		for (ip=0;ip<np;ip++) {
			s2[0] = s1[0];
			for (i=1;i<nt;i++) {
				s2[i] = ro*s2[i-1] + eps*s1[i];
			}
			for (i=nt-2;i>=0;i--) {
				s1[i] = ro*s1[i+1] + eps*s2[i];
			}
		}
		for (i=0;i<nt;i++) {
			pdata[i] /= s1[i];
		}
	}
}

float getclip(data,n)
int n;
float *data;
{
	int k;
	float clip,qclip,*a,dclip=1.0;

	if(!fgetpar("clip",&clip)) {
		if(!fgetpar("qclip",&qclip))	qclip = 100.0;
		k = qclip/100.0*n - 0.5;
		if( (k<=0) || (k>n-1) ) err("bad qclip=%d\n",qclip);
		a = (float*) malloc(n*sizeof(float));
		copyabs(data,a,n);
		quant(k,a,n);
		clip = a[k];
		free( (char*)a );
		if(fgetpar("dclip",&dclip)) clip *= dclip;
/* 		fprintf(stderr,"clip=%f from qclip=%f and dclip=%f\n", */
/* 				clip,qclip,dclip); */
	} else {
		fprintf(stderr,"given clip=%f\n",clip);
	}
/* 	if( clip == 0.) err("zero clip=%f\n",clip); */
	return(clip);
}

dogpow(p,n,gpow)
float gpow;
register float *p;
int n;
{
	register float *pp,*pe;

	if(gpow==1.0) {
		return;
	} else if (gpow==0.5) {
		while(n--) {
			if (*p > 0.0)
				*p = sqrt(*p);
			else if (*p<0.0)
				*p = -sqrt(-*p);
			p++;
		}
	} else {
		while(n--) {
			if (*p > 0.0)
				*p = exp(gpow*log(*p));
			else if (*p<0.0)
				*p = -exp(gpow*log(-*p));
			p++;
		}
	}
}

doepow(data,nt,nx,epow)
float epow;
float *data;
int nx,nt;
{
	int i,t;
	float mepow;
	/* register */ float *pdata,egain;

	mepow = 88.;	/* 127*log(2) */
	if (epow>=mepow) {
		fprintf(stderr,
			"I'm going to crash because epow=%f>%f is too big\n",
			epow,mepow);
		fprintf(stderr,"I'll try\n");
	}
	epow /= (float)nt;
	for(t=0;t<nt;t++) {
		egain = exp( epow*t );
		pdata = data + t;
		for(i=0;i<nx;pdata+=nt,i++) {
			*pdata *= egain;
		}
	}
}

dotpow(data,nt,nx,tpow)
float tpow;
float *data;
int nx,nt;
{
	int i,t;
	float ont,mtpow;
	/* register */ float *pdata,tgain;

	mtpow = 127.0*log(2.0)/log((float)nt);
	if (tpow>=mtpow) {
		fprintf(stderr,
			"I'm going to crash because tpow=%f>%f is too big\n",
			tpow,mtpow);
		fprintf(stderr,"I'll try\n");
	}
	ont = tpow>0.0?1.0/nt:1.0;
	pdata = data;
	for(t=0;t<nt;t++) {
		tgain = exp( tpow*log( ont*(t+1) ) );
/* fprintf(stderr,"t=%d tgain=%f\n",t,tgain); */
		pdata = data + t;
		for(i=0;i<nx;pdata+=nt,i++) {
			*pdata *= tgain;
		}
	}
}

balclip(data,n,c)
float c;
register float *data;
int n;
{
	register float oc;
	oc = 1.0/c;
	while(n--) {
		*data *= oc;
		if( *data > 1.0 ) *data = 1.0;
		else if( *data < -1.0 ) *data = -1.0;
		data++;
	}
} 

copyabs(p,q,n)
register n;
register float *p,*q;
{
	while(n--)
		*q++ = fabs(*p++);
}

/*	this is a translation of program from SEP 10 p100. */
/*	quant reorders a[] that 
 *		a[j]<a[k] if j<k
 *	i.e. a[k-1] is the k/n quntile.
 */
quant(k,a,n)
int k,n; float *a;
{
	int low,hi,i,j;
	double ak,aa;
	low = 0;
	hi = n-1;
	while(low < hi)
	{
		ak = a[k];
		i = low;
		j = hi;
		do
		{
			while(a[i] < ak)
			{
				i++;
			}
			while(a[j] > ak)
			{
				j--;
			}
			if(i <= j)
			{
				aa = a[i];
				a[i] = a[j];
				a[j] = aa;
				i++;
				j--;
			}
		}
		while(i <= j);
		if(j < k)
		{
			low = i;
		}
		if(k < i)
		{
			hi = j;
		}
	}
}
