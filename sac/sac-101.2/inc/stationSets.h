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
   float azimuth;
   char KCMPNM[16];
   long int NPTS;
   float B;
   float *data;
   float A;
   float USER0;
   float USER1;
   float USER2;
   float USER3;
   float USER4;
   float USER5;
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
   char KUSER2[8];
   char KT09[80];
   
};

struct StationComp{
   char *name;
   float delta;
   struct StationComp *next;
   int Vfound;
   struct ChannelData V;
   int H1found;   
   struct ChannelData H1;
   int H2found;   
   struct ChannelData H2;
   int flipHoriz;		/* true if axis1-axis2 angle is negative */
   float Axis1Angle;		/* angle of axis 1 from East */
};

#define NULLSET (struct StationComp *) 0

/*  Component identifiers  */
enum Component { VERTICAL, HORIZ1, HORIZ2 };


void matAddToChanSet(long int, long int, float *);
void matTrimSets(void); /* Delete incomplete or incorrect station sets*/
long int matGetCompIndex(struct StationComp *, enum Component);
long int matGetNumStationSets(void);
void matFreeChanSetList(void);
void matListStationSets(void);  /* print list of station sets */
long int matGetMaxDataLen(void);
float *matGetSeisPntr(enum Component , long int , long int *);
void matCopyToHeader(enum Component , long int , double *);
void matCopyStrings(enum Component , long int , char *);
void matCopyHeaderValues(enum Component , long int , double *);
void matCopyStringsToHeader(enum Component , long int , char *);
void matUpdateFromChanSet(long int );
