/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION:  alloc, init, & return pointer to state of quantile estimator 

PARAMETERS:
p		i quantile to be estimated (0.0<=p<=1.0 is required)
n		i number of samples in array x (n>=5 is required)
x		i array of floats

NOTES:
This function must be called before calling function questUpdate.

See also notes in questUpdate.

AUTHOR:  I. D. Hale, 05/07/89
*/

#include "cwp.h"

QuestState *questInit (float p, int n, float x[])
{
	int i,j;
	float q[5],qtemp;
	QuestState *s;
	
	/* allocate space for state */
	s = (QuestState *)malloc(sizeof *s);
	
	/* initialize marker heights to first 5 x values sorted */
	q[0] = x[0];  q[1] = x[1];  q[2] = x[2];  q[3] = x[3];  q[4] = x[4];
	for (i=1; i<5; i++) {
		for (j=i; j>0 && q[j-1]>q[j]; j--) {
			qtemp = q[j-1];
			q[j-1] = q[j];
			q[j] = qtemp;
		}
	}
	s->q0 = q[0];
	s->q1 = q[1];
	s->q2 = q[2];
	s->q3 = q[3];
	s->q4 = q[4];
		
	/* initialize marker positions */
	s->m0 = 0;  s->m1 = 1;  s->m2 = 2;  s->m3 = 3;  s->m4 = 4;
	
	/* initialize desired marker positions */
	s->f0 = 0;  s->f1 = 2*p;  s->f2 = 4*p;  s->f3 = 2+2*p;  s->f4 = 4;
	
	/* compute increments in desired marker positions */
	s->d0 = 0;  s->d1 = p/2;  s->d2 = p;  s->d3 = (1+p)/2;  s->d4 = 1;
	
	/* if more than 5 x values input, then update state */
	if (n>5) qtemp = questUpdate(s,n-5,&x[5]);
	
	/* return pointer to state */
	return s;
}
