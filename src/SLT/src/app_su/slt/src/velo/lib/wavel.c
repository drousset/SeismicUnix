#include <par.h>
 
void wavel(int n1, int n2, int n3, float d1, float d2, float d3,
	int time, float *wl, float ***v)
/*************************************************************************
Compute minimum wavelengths for individual dimension and total
*************************************************************************/
{
	int  i1,i2, i3;
	float  k=0, k1=0, k2=0, k3=0, dv1, dv2, dv3, dv, vel, ovel;
	float ddv1, ddv2, ddv3, ddv, scal;
	float od1=1.0/d1, od2=1.0/d2, od3=1.0/d3;
	float o2d1=od1*od1, o2d2=od2*od2, o2d3=od3*od3;
	float d2v1, d2v2, d2v3;
 
 
   	for(i3=0; i3<n3; ++i3) 
 	    for(i2=0; i2<n2; ++i2)
		for(i1=0; i1<n1; ++i1){
			vel = v[i3][i2][i1];
			ovel = 1.0/vel;
			if(i1==0||i1==n1-1) d2v1 = ddv1 = 0.;
			else {
				dv1 = (vel-v[i3][i2][i1-1])*od1*ovel;
				d2v1 = dv1*dv1;
				ddv1 = (v[i3][i2][i1-1]-2.0*vel+
					  v[i3][i2][i1+1])*o2d1*ovel;
				if(ddv1<0) ddv1 = -ddv1;
			}
			if(i2==0||i2==n2-1)  d2v2 = ddv2 = 0.;
			else {
				dv2 = (vel-v[i3][i2-1][i1])*od2*ovel;
				d2v2 = dv2*dv2;
				ddv2 = (v[i3][i2-1][i1]-2.0*vel+
					  v[i3][i2+1][i1])*o2d2*ovel;
				if(ddv2<0) ddv2 = -ddv2;
			}
			if(i3==0||i3==n3-1) d2v3 = ddv3 = 0.;
			else {
				dv3 = (vel-v[i3-1][i2][i1])*od3*ovel;
				d2v3 = dv3*dv3;
				ddv3 = (v[i3-1][i2][i1]-2.0*vel+
					  v[i3+1][i2][i1])*o2d3*ovel;
				if(ddv3<0) ddv3 = -ddv3;
			}
 			if(time) {
				scal = 2000.*ovel;
				scal = scal*scal;
			}
			if(time==1) {
				d2v1 *= scal;
				ddv1 *= scal;
			}
			else if(time==2){
				d2v2 *= scal;
				ddv2 *= scal;
			}
 			else if(time==3){
				d2v3 *= scal;
				ddv3 *= scal;
			}



  			if(k1<d2v1) k1 = d2v1;
			if(k1<ddv1) k1 = ddv1;

  			if(k2<d2v2) k2 = d2v2;
			if(k2<ddv2) k2 = ddv2;

  			if(k3<d2v3) k3 = d2v3;
			if(k3<ddv3) k3 = ddv3;

  	 		dv = d2v1+d2v2+d2v3;
			ddv = ddv1+ddv2+ddv3;
 			if(k<dv)  k = dv; 
			if(k<ddv) k = ddv; 
 		}
	 
	wl[0] = (k)? 2.0*PI/sqrt(k): -1;
	wl[1] = (k1)? 2.0*PI/sqrt(k1): -1;
	wl[2] = (k2)? 2.0*PI/sqrt(k2): -1;
	wl[3] = (k3)? 2.0*PI/sqrt(k3): -1;
}

