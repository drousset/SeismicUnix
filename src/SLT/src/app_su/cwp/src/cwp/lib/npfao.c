/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*
FUNCTION:  return optimal n between nmin and nmax for prime factor fft

PARAMETERS:
nmin        i lower bound on returned value (see notes below)
nmax        i desired (but not guaranteed) upper bound on returned value

NOTES:
npfao will be composed of mutually prime factors from the
set {2,3,4,5,7,8,9,11,13,16}.  Because npfao cannot exceed
720720 = 5*7*9*11*13*16, npfao = 720720 is returned if nmin 
exceeds 720720.  If nmin does not exceed 720720, then the
returned npfao will not be less than nmin.  The optimal
npfao is chosen to minimize the number of multiplications 
and additions, while satisfying the constraint, if possible, 
that npfao not exceed nmax.

AUTHOR:  I. D. Hale, Colorado School of Mines, 06/13/89
*/

#include "cwp.h"
static int napfa (int n);
static int nmpfa (int n);

int npfao (int nmin, int nmax)
{
    int n,nopt,nc,ncmin;

    n = npfa(nmin);
    ncmin = napfa(n)+nmpfa(n);
    nopt = n;
    while (n<nmax) {
        n = npfa(n+1);
        nc = napfa(n)+nmpfa(n);
        if (nc<ncmin) {
            ncmin = nc;
            nopt = n;
        }
    }
    return nopt;
}

/* function to determine number of real additions for pfa */
#define NA2 4
#define NA3 12
#define NA4 16
#define NA5 32
#define NA7 60
#define NA8 52
#define NA9 80
#define NA11 140
#define NA13 192
#define NA16 144

static int napfa (int n)
{
    int nadd=0,nleft=n;

    if (nleft%16==0) {
        nleft = nleft/16;
        nadd = nadd+n/16*NA16;
    }
    if (nleft%13==0) {
        nleft = nleft/13;
        nadd = nadd+n/13*NA13;
    }
    if (nleft%11==0) {
        nleft = nleft/11;
        nadd = nadd+n/11*NA11;
    }
    if (nleft%9==0) {
        nleft = nleft/9;
        nadd = nadd+n/9*NA9;
    }
    if (nleft%8==0) {
        nleft = nleft/8;
        nadd = nadd+n/8*NA8;
    }
    if (nleft%7==0) {
        nleft = nleft/7;
        nadd = nadd+n/7*NA7;
    }
    if (nleft%5==0) {
        nleft = nleft/5;
        nadd = nadd+n/5*NA5;
    }
    if (nleft%4==0) {
        nleft = nleft/4;
        nadd = nadd+n/4*NA4;
    }
    if (nleft%3==0) {
        nleft = nleft/3;
        nadd = nadd+n/3*NA3;
    }
    if (nleft%2==0) {
        nleft = nleft/2;
        nadd = nadd+n/2*NA2;
    }
    return nadd;
}

/* function to determine number of real multiplications for pfa */
#define NM2 0
#define NM3 4
#define NM4 2
#define NM5 12
#define NM7 32
#define NM8 8
#define NM9 28
#define NM11 92
#define NM13 134
#define NM16 30

static int nmpfa (int n)
{
    int nmul=0,nleft=n;

    if (nleft%16==0) {
        nleft = nleft/16;
        nmul = nmul+n/16*NM16;
    }
    if (nleft%13==0) {
        nleft = nleft/13;
        nmul = nmul+n/13*NM13;
    }
    if (nleft%11==0) {
        nleft = nleft/11;
        nmul = nmul+n/11*NM11;
    }
    if (nleft%9==0) {
        nleft = nleft/9;
        nmul = nmul+n/9*NM9;
    }
    if (nleft%8==0) {
        nleft = nleft/8;
        nmul = nmul+n/8*NM8;
    }
    if (nleft%7==0) {
        nleft = nleft/7;
        nmul = nmul+n/7*NM7;
    }
    if (nleft%5==0) {
        nleft = nleft/5;
        nmul = nmul+n/5*NM5;
    }
    if (nleft%4==0) {
        nleft = nleft/4;
        nmul = nmul+n/4*NM4;
    }
    if (nleft%3==0) {
        nleft = nleft/3;
        nmul = nmul+n/3*NM3;
    }
    if (nleft%2==0) {
        nleft = nleft/2;
        nmul = nmul+n/2*NM2;
    }
    return nmul;
}
