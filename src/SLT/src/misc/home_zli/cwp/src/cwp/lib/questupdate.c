/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION:  update and return a quantile estimate 

PARAMETERS:
s		i pointer to state of quantile estimator
n		i number of samples in array x
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

#include "cwp.h"

float questUpdate(QuestState *s, int n, float x[])
{
	int m0,m1,m2,m3,m4,mp,mm,i,j;
	float q0,q1,q2,q3,q4,qtemp,f0,f1,f2,f3,f4,d0,d1,d2,d3,d4,xtemp;
	
	/* get state variables */
	m0 = s->m0;  m1 = s->m1;  m2 = s->m2;  m3 = s->m3;  m4 = s->m4;
	q0 = s->q0;  q1 = s->q1;  q2 = s->q2;  q3 = s->q3;  q4 = s->q4;
	f0 = s->f0;  f1 = s->f1;  f2 = s->f2;  f3 = s->f3;  f4 = s->f4;
	d0 = s->d0;  d1 = s->d1;  d2 = s->d2;  d3 = s->d3;  d4 = s->d4;
	
	/* loop over elements in array */
	for (i=0; i<n; i++) {
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
			qtemp = q1+((mp-m0)*(q2-q1)/(m2-m1)+
				(m2-mp)*(q1-q0)/(m1-m0))/(m2-m0);
			if (q0<qtemp && qtemp<q2)
				q1 = qtemp;
			else
				q1 = q1+(q2-q1)/(m2-m1);
			m1 = mp;
		} else if (f1<=mm && m0<mm) {
			qtemp = q1-((mm-m0)*(q2-q1)/(m2-m1)+
				(m2-mm)*(q1-q0)/(m1-m0))/(m2-m0);
			if (q0<qtemp && qtemp<q2)
				q1 = qtemp;
			else
				q1 = q1-(q0-q1)/(m0-m1);
			m1 = mm;
		}
		
		/* adjust height and location of marker 2, if necessary */
		mp = m2+1;  mm = m2-1;
		if (f2>=mp && m3>mp) {
			qtemp = q2+((mp-m1)*(q3-q2)/(m3-m2)+
				(m3-mp)*(q2-q1)/(m2-m1))/(m3-m1);
			if (q1<qtemp && qtemp<q3)
				q2 = qtemp;
			else
				q2 = q2+(q3-q2)/(m3-m2);
			m2 = mp;
		} else if (f2<=mm && m1<mm) {
			qtemp = q2-((mm-m1)*(q3-q2)/(m3-m2)+
				(m3-mm)*(q2-q1)/(m2-m1))/(m3-m1);
			if (q1<qtemp && qtemp<q3)
				q2 = qtemp;
			else
				q2 = q2-(q1-q2)/(m1-m2);
			m2 = mm;
		}
		
		/* adjust height and location of marker 3, if necessary */
		mp = m3+1;  mm = m3-1;
		if (f3>=mp && m4>mp) {
			qtemp = q3+((mp-m2)*(q4-q3)/(m4-m3)+
				(m4-mp)*(q3-q2)/(m3-m2))/(m4-m2);
			if (q2<qtemp && qtemp<q4)
				q3 = qtemp;
			else
				q3 = q3+(q4-q1)/(m4-m3);
			m3 = mp;
		} else if (f3<=mm && m2<mm) {
			qtemp = q3-((mm-m2)*(q4-q3)/(m4-m3)+
				(m4-mm)*(q3-q2)/(m3-m2))/(m4-m2);
			if (q2<qtemp && qtemp<q4)
				q3 = qtemp;
			else
				q3 = q3-(q2-q3)/(m2-m3);
			m3 = mm;
		}
	}
	
	/* set state variables */
	s->m0 = m0;  s->m1 = m1;  s->m2 = m2;  s->m3 = m3;  s->m4 = m4;
	s->q0 = q0;  s->q1 = q1;  s->q2 = q2;  s->q3 = q3;  s->q4 = q4;
	s->f0 = f0;  s->f1 = f1;  s->f2 = f2;  s->f3 = f3;  s->f4 = f4;
	s->d0 = d0;  s->d1 = d1;  s->d2 = d2;  s->d3 = d3;  s->d4 = d4;
	
	/* return current quantile estimate */
	return q2;
}
