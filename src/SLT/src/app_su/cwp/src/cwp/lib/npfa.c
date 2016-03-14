/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION:  return smallest valid n not less than nmin for prime factor fft

PARAMETERS:
nmin		i lower bound on returned value (see notes below)

NOTES:
npfa will be composed of mutually prime factors from the
set {2,3,4,5,7,8,9,11,13,16}.  because npfa cannot exceed
720720 = 5*7*9*11*13*16, npfa = 720720 is returned if nmin 
exceeds 720720.

AUTHOR:  I. D. Hale, Colorado School of Mines, 04/28/89
*/

#define N2 4
#define N3 2
#define N5 1
#define N7 1
#define N11 1
#define N13 1

static int ipow();

int npfa (int nmin)
{
	int i2,i3,i5,i7,i11,i13,n,ntemp;

	ntemp = ipow(2,N2)*
		ipow(3,N3)*
		ipow(5,N5)*
		ipow(7,N7)*
		ipow(11,N11)*
		ipow(13,N13);
	for (i2=0; i2<=N2; i2++) {
		for (i3=0; i3<=N3; i3++) {
			for (i5=0; i5<=N5; i5++) {
				for (i7=0; i7<=N7; i7++) {
					for (i11=0; i11<=N11; i11++) {
						for (i13=0; i13<=N13; i13++) {
							n = ipow(2,i2)*
								ipow(3,i3)*
								ipow(5,i5)*
								ipow(7,i7)*
								ipow(11,i11)*
								ipow(13,i13);
							if (n>=nmin && ntemp>n) ntemp = n;
						}
					}
				}
			}
		}
	}
	return ntemp;
}

static int ipow(i,j)
int i,j;
{
	int k,m;
	for (k=0,m=1; k<j; k++)
		m = m*i;
	return m;
}
