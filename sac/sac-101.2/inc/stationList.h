#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#ifndef TRUE
#define TRUE   1
#endif
#ifndef FALSE
#define FALSE   0
#endif

struct ChannelData{
   long int traceNum;
   char KCMPNM[16];
   long int NPTS;
   float B;
   float *data;
   float A;
   float USER0;
   float USER1;
   float T[10];
   long int year;
   long int jday;
   long int hour;
   long int min;
   long int sec;
   long int msec;
   char KA[8];
   char KUSER0[8];
   char KUSER1[8];
   char KT09[80];

};


void AddToStationList(long int, long int, float *);
void UpdateFromStationList(long int );
