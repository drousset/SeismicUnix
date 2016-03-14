/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION:  return an estimate of a specified quantile 

PARAMETERS:
p		i quantile to be estimated (0.0<=p<=1.0 is required)
n		i number of samples in array x (n>=5 is required)
x		i array of floats

NOTES:
The estimate should approach the sample quantile in the limit of large n.

The estimate is most accurate for cumulative distribution functions
that are smooth in the neighborhood of the quantile specified by p.

This function is an implementation of the algorithm published by
Jain, R. and Chlamtac, I., 1985, The PP algorithm for dynamic
calculation of quantiles and histograms without storing observations:
Comm. ACM, v. 28, n. 10.

AUTHOR:  I. D. Hale, 05/07/89
*/
float quest(float p, int n, float x[])
{
	int q[5],m0,m1,m2,m3,m4,mp,mm,i,j;
	float f0,f1,f2,f3,f4,d0,d1,d2,d3,d4,xtemp,fm0,fm1,fm2,fm3,fm4,fmp,fmm;
	register float q0,q1,q2,q3,q4,qtemp;
		
	/* initialize marker heights to first 5 x values sorted */
	q[0] = x[0];  q[1] = x[1];  q[2] = x[2];  q[3] = x[3];  q[4] = x[4];
	for (i=1; i<5; i++) {
		for (j=i; j>0 && q[j-1]>q[j]; j--) {
			qtemp = q[j-1];
			q[j-1] = q[j];
			q[j] = qtemp;
		}
	}
	q0 = q[0];  q1 = q[1];  q2 = q[2];  q3 = q[3];  q4 = q[4];
		
	/* initialize marker positions */
	m0 = 0;  m1 = 1;  m2 = 2;  m3 = 3;  m4 = 4;
	
	/* initialize desired marker positions */
	f0 = 0;  f1 = 2*p;  f2 = 4*p;  f3 = 2+2*p;  f4 = 4;
	
	/* compute increments in desired marker positions */
	d0 = 0;  d1 = p/2;  d2 = p;  d3 = (1+p)/2;  d4 = 1;
	
	/* loop over elements in array */
	for (i=5; i<n; i++) {
		xtemp = x[i];
		
		/* increment marker locations and update min and max */
		if (xtemp<q0) {
			m1++;  m2++;  m3++;  m4++;  q0 = xtemp;
		} else if (xtemp<q1) {
			m1++;  m2++;  m3++;  m4++;
		} else if (xtemp<q2) {
			m2++;  m3++;  m4++;
		} else if (xtemp<q3) {
			m3++;  m4++;
		} else if (xtemp<q4) {
			m4++;
		} else {
			m4++;  q4 = xtemp;
		}
		
		/* increment desired marker positions */
		f0+=d0;  f1+=d1;  f2+=d2;  f3+=d3;  f4+=d4;
		
		/* adjust height and location of marker 1, if necessary */
		mp = m1+1;  mm = m1-1;
		if (f1>=mp && m2>mp) {
			fmp = mp;  fm2 = m2;  fm1 = m1;  fm0 = m0;
			qtemp = q1+((fmp-fm0)*(q2-q1)/(fm2-fm1)+
				(fm2-fmp)*(q1-q0)/(fm1-fm0))/(fm2-fm0);
			if (q0<qtemp && qtemp<q2)
				q1 = qtemp;
			else
				q1 = q1+(q2-q1)/(fm2-fm1);
			m1 = mp;
		} else if (f1<=mm && m0<mm) {
			fmm = mm;  fm2 = m2;  fm1 = m1;  fm0 = m0;
			qtemp = q1-((fmm-fm0)*(q2-q1)/(fm2-fm1)+
				(fm2-fmm)*(q1-q0)/(fm1-fm0))/(fm2-fm0);
			if (q0<qtemp && qtemp<q2)
				q1 = qtemp;
			else
				q1 = q1-(q0-q1)/(fm0-fm1);
			m1 = mm;
		}
		
		/* adjust height and location of marker 2, if necessary */
		mp = m2+1;  mm = m2-1;
		if (f2>=mp && m3>mp) {
			fmp = mp;  fm3 = m3;  fm2 = m2;  fm1 = m1;
			qtemp = q2+((fmp-fm1)*(q3-q2)/(fm3-fm2)+
				(fm3-fmp)*(q2-q1)/(fm2-fm1))/(fm3-fm1);
			if (q1<qtemp && qtemp<q3)
				q2 = qtemp;
			else
				q2 = q2+(q3-q2)/(m3-m2);
			m2 = mp;
		} else if (f2<=mm && m1<mm) {
			fmm = mm;  fm3 = m3;  fm2 = m2;  fm1 = m1;
			qtemp = q2-((fmm-fm1)*(q3-q2)/(fm3-fm2)+
				(fm3-fmm)*(q2-q1)/(fm2-fm1))/(fm3-fm1);
			if (q1<qtemp && qtemp<q3)
				q2 = qtemp;
			else
				q2 = q2-(q1-q2)/(fm1-fm2);
			m2 = mm;
		}
		
		/* adjust height and location of marker 3, if necessary */
		mp = m3+1;  mm = m3-1;
		if (f3>=mp && m4>mp) {
			fmp = mp;  fm4 = m4;  fm3 = m3;  fm2 = m2;
			qtemp = q3+((fmp-fm2)*(q4-q3)/(fm4-fm3)+
				(fm4-fmp)*(q3-q2)/(fm3-fm2))/(fm4-fm2);
			if (q2<qtemp && qtemp<q4)
				q3 = qtemp;
			else
				q3 = q3+(q4-q1)/(fm4-fm3);
			m3 = mp;
		} else if (f3<=mm && m2<mm) {
			fmm = mm;  fm4 = m4;  fm3 = m3;  fm2 = m2;
			qtemp = q3-((fmm-fm2)*(q4-q3)/(fm4-fm3)+
				(fm4-fmm)*(q3-q2)/(fm3-fm2))/(fm4-fm2);
			if (q2<qtemp && qtemp<q4)
				q3 = qtemp;
			else
				q3 = q3-(q2-q3)/(fm2-fm3);
			m3 = mm;
		}
	}
	return q2;
}
