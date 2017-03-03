
#include <config.h>

#ifdef HAVE_MATLAB

#include <string.h>
#include <stdio.h>
#include <math.h>

#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
#include "matStructDefs.h"


struct SACdata{
   struct hdrTimes times;
   struct hdrStation station;
   struct hdrEvent event;
   struct hdrUser user[10];
   struct hdrDataDescrip descrip;
   struct hdrEventSta evsta;
   struct hdrLLNLextensions llnl;
   struct hdrDepMec depmec; 
   double response[10];
   double *trcReal;
   double *trcImag;
   double traceLength;
};

struct SACdata *SACdataArray;
long NumberofTraces;
   
#define ARRAY_BLK_SIZE 10   /* allocate SACdataArray in units of this size */        
#include "matHeaderDataManage.h"


/* Functions that define SACMAT SACdata structures */

void matSetHeaderTimes(struct hdrTimes *times)
{
   int j;

   times->delta = *delta;
   times->b = *b;
   times->e = *e;
   times->o = *o;
   times->a = *a;
   times->f = *f;
   strncpy(times->ko,ko, CHAR_FIELD_LEN );
   strncpy(times->ka,ka, CHAR_FIELD_LEN );
   strncpy(times->kf,kf, CHAR_FIELD_LEN );

   for(j=0; j < 10; j++){
      times->t[j] = *(t0 + j);
      strncpy(times->kt[j], kt0 + CHAR_FIELD_LEN *j, CHAR_FIELD_LEN );
   }
}


void matUpdateTimes(struct hdrTimes times)
{
   int j;

   *delta = times.delta;
   *b = times.b;
   *e = times.e;
   *o = times.o;
   *a = times.a;
   *f = times.f;
   strncpy(ko,times.ko, CHAR_FIELD_LEN );
   strncpy(ka,times.ka, CHAR_FIELD_LEN );
   strncpy(kf,times.kf, CHAR_FIELD_LEN );

   for(j=0; j < 10; j++){
      *(t0 + j) = times.t[j];
      strncpy( kt0 + CHAR_FIELD_LEN *j, times.kt[j], CHAR_FIELD_LEN );
   }
}




void matSetHeaderStation(struct hdrStation *station)
{
   station->stla = *stla;
   station->stlo = *stlo;
   station->stel = *stel;
   station->stdp = *stdp;
   station->cmpaz = *cmpaz;
   station->cmpinc = *cmpinc;
   strncpy(station->kstnm, kstnm,CHAR_FIELD_LEN);
   strncpy(station->kcmpnm,kcmpnm,CHAR_FIELD_LEN);
   strncpy(station->knetwk,knetwk,CHAR_FIELD_LEN);
}


void matUpdateStation(struct hdrStation station)
{
   *stla = station.stla;
   *stlo = station.stlo;
   *stel = station.stel;
   *stdp = station.stdp;
   *cmpaz = station.cmpaz;
   *cmpinc =  station.cmpinc;
   strncpy(kstnm, station.kstnm,CHAR_FIELD_LEN);
   strncpy(kcmpnm, station.kcmpnm,CHAR_FIELD_LEN);
   strncpy(knetwk, station.knetwk,CHAR_FIELD_LEN);

   /* Disallow undefined kstnm and kcmpnm */
   uniqueStaAndChan () ;
}

         

void matSetHeaderEvent(struct hdrEvent *event)
{
   event->evla = *evla;
   event->evlo = *evlo;
   event->evel = *evel;
   event->evdp = *evdp;
   event->nzyear = *nzyear;
   event->nzjday = *nzjday;
   event->nzhour = *nzhour;
   event->nzmin = *nzmin;
   event->nzsec = *nzsec;
   event->nzmsec = *nzmsec;
   event->mag = *mag;
   event->imagtyp = *imagtyp;
   event->imagsrc = *imagsrc;
   strncpy(event->kevnm,kevnm,2*CHAR_FIELD_LEN-1);
}



void matUpdateEvent(struct hdrEvent event)
{
   *evla = event.evla;
   *evlo = event.evlo;
   *evel = event.evel;
   *evdp = event.evdp;
   *nzyear = event.nzyear;
   *nzjday = event.nzjday;
   *nzhour = event.nzhour;
   *nzmin = event.nzmin;
   *nzsec = event.nzsec;
   *nzmsec = event.nzmsec;
   *mag = event.mag;
   *imagtyp = event.imagtyp;
   *imagsrc = event.imagsrc;
   strncpy(kevnm, event.kevnm,2*CHAR_FIELD_LEN-1);
}




 void matSetHeaderUser(struct hdrUser *user)
{
   int j;
   for(j=0;j<10;j++){
      user[j].data = *(user0 +j);
      if(j < 3)
         strncpy(user[j].label, kuser0 +j*CHAR_FIELD_LEN,CHAR_FIELD_LEN);
      else
	 strncpy(user[j].label, SAC_CHAR_UNDEFINED,6);
   }
} 


 void matUpdateUser(struct hdrUser *user)
{
   int j;
   for(j=0;j<10;j++){
      *(user0 +j) = user[j].data;
      if(j < 3)
         strncpy(kuser0 +j*CHAR_FIELD_LEN, user[j].label, CHAR_FIELD_LEN);	
   }
} 






 void matSetHeaderDataDescrip(struct hdrDataDescrip *descrip, int index)
{
   long int ic1, ic2;
   descrip->iftype = *iftype;
   descrip->idep = *idep;
   descrip->iztype = *iztype;
   descrip->iinst = *iinst;
   descrip->istreg = *istreg;
   descrip->ievreg = *ievreg;
   descrip->ievtyp = *ievtyp;
   descrip->iqual = *iqual;
   descrip->isynth = *isynth;
   descrip->filename = ""; 
   lnumcl(kmdfm.kdfl, MAXCHARS, index+1, &ic1, &ic2);
   /*   printf("fileindex, ic1, ic2: %d, %d, %d \n", index+1, ic1, ic2);*/
   descrip->filename = (char *) malloc(ic2-ic1+2);
   strncpy(descrip->filename, kmdfm.kdfl+ic1-1, ic2-ic1+1 );
   descrip->filename[ic2-ic1+1]='\0';
   /* printf("the filename is: %s", descrip->filename ); */
}



 void matUpdateDataDescrip(struct hdrDataDescrip descrip)
{
   *iftype = descrip.iftype;
   *idep = descrip.idep;
   *iztype = descrip.iztype;
   *iinst = descrip.iinst;
   *istreg = descrip.istreg;
   *ievreg = descrip.ievreg;
   *ievtyp = descrip.ievtyp;
   *iqual = descrip.iqual;
   *isynth = descrip.isynth;
}



void matSetHeaderEvsta(struct hdrEventSta *evsta)
{
   evsta->dist = *dist;
   evsta->az = *az;
   evsta->baz = *baz;
   evsta->gcarc = *gcarc;
}




void matUpdateHeaderEvsta(struct hdrEventSta evsta)
{
   *dist = evsta.dist;
   *az = evsta.az;
   *baz = evsta.baz;
   *gcarc = evsta.gcarc;
}




void matSetHeaderLLNL(struct hdrLLNLextensions *llnl)
{
   llnl->xminimum = *xminimum;
   llnl->xmaximum = *xmaximum;
   llnl->yminimum = *yminimum;
   llnl->ymaximum = *ymaximum;
   llnl->norid = *norid;
   llnl->nevid = *nevid;
   llnl->nwfid = *nwfid;
   llnl->nxsize = *nxsize;
   llnl->nysize = *nysize;
} 





void matUpdateHeaderLLNL(struct hdrLLNLextensions llnl)
{
   *xminimum = llnl.xminimum;
   *xmaximum = llnl.xmaximum;
   *yminimum = llnl.yminimum;
   *ymaximum = llnl.ymaximum;
   *norid = llnl.norid;
   *nevid = llnl.nevid;
   *nwfid = llnl.nwfid;
   *nxsize = llnl.nxsize;
   *nysize = llnl.nysize;
} 


void matSetHeaderDepMec(struct hdrDepMec *depmec)
{
   depmec->checked = *fhdr65;
   depmec->flipped = *fhdr64;
   depmec->signoise = *fhdr69;
   depmec->snrfixed = *fhdr70;
   depmec->filtertype = *ihdr4;
   depmec->filterorder = *nhdr15;
   depmec->lowerfiltercorner = *fhdr66;
   depmec->upperfiltercorner = *fhdr67;
   depmec->mtisotropicfraction = *fhdr68;
} 





void matUpdateHeaderDepMec(struct hdrDepMec depmec)
{
   *fhdr65 = depmec.checked;
   *fhdr64 = depmec.flipped;
   *fhdr69 = depmec.signoise;
   *fhdr70 = depmec.snrfixed;
   *ihdr4 = depmec.filtertype; 
   *nhdr15 = depmec.filterorder; 
   *fhdr66 = depmec.lowerfiltercorner; 
   *fhdr67 = depmec.upperfiltercorner; 
   *fhdr68 = depmec.mtisotropicfraction; 
} 




 void matSetHeaderResponse(double *response)
{
   int j;
   for(j=0;j<10;j++){
      response[j] = *(resp0 +j);
   }
} 


 void matUpdateResponse(double *response)
{
   int j;
   for(j=0;j<10;j++){
       *(resp0 +j) = response[j];
   }
} 



/* function used to determine whether to realloc SACdataArray */
int matAddBlock( long index)
{
   if(index == 0)return TRUE;
   if( index % ARRAY_BLK_SIZE) return FALSE;
   return TRUE;
}




/* function to add an element to SACdataArray */
int matAddSACdataElement(long index, float *DatafloatPtrReal, float *DatafloatPtrImag, long nlen)
{
   long newSize, j;
   
   NumberofTraces = index+1;
   if( matAddBlock(index)){
      newSize = (index / ARRAY_BLK_SIZE + 1 ) * ARRAY_BLK_SIZE;
      SACdataArray = (struct SACdata *) realloc(SACdataArray, newSize * sizeof(struct SACdata));
      if( SACdataArray == (struct SACdata *) NULL){
         fprintf(stderr, "ERROR: Could not allocate SACdataArray. \n");
         exit(-1);
      }
      for(j=index;j<index + ARRAY_BLK_SIZE;j++){
         SACdataArray[j].trcReal = (double *) NULL;
         SACdataArray[j].trcImag = (double *) NULL;
      }
   }
   DataIsComplex = FALSE;
   SACdataArray[index].traceLength = nlen;
   matSetHeaderTimes(&(SACdataArray[index].times));
   matSetHeaderStation(&(SACdataArray[index].station));
   matSetHeaderEvent(&(SACdataArray[index].event));
   matSetHeaderUser(SACdataArray[index].user);
   matSetHeaderDataDescrip(&(SACdataArray[index].descrip), index);
   matSetHeaderEvsta(&(SACdataArray[index].evsta));
   matSetHeaderLLNL(&(SACdataArray[index].llnl));
   matSetHeaderDepMec(&SACdataArray[index].depmec);
   matSetHeaderResponse(SACdataArray[index].response);
   /* Copy real data to double array */
   SACdataArray[index].trcReal = (double *) calloc(nlen,sizeof(double));
   if(SACdataArray[index].trcReal == (double *) NULL){
      fprintf(stderr,"ERROR: Could not allocate real array in AddSACdataElement.");
      exit(-1);
   }
   for(j=0;j<nlen;j++)
      SACdataArray[index].trcReal[j] = DatafloatPtrReal[j];

   /* Copy imaginary data if present */
   if( *iftype == *irlim || *iftype == *iamph){
      DataIsComplex = TRUE;
      SACdataArray[index].trcImag = (double *) calloc(nlen,sizeof(double));
      if(SACdataArray[index].trcImag == (double *) NULL){
         fprintf(stderr,"ERROR: Could not allocate imaginary array in AddSACdataElement.");
         exit(-1);
      }
      for(j=0;j<nlen;j++)
         SACdataArray[index].trcImag[j] = DatafloatPtrImag[j];
   }

   return TRUE;
}







/* function to update current trace data from SACdataArray */
int matUpdateSACfromSACdataArray(long index, float *DatafloatPtrReal, float *DatafloatPtrImag, long nlen)
{
   long newSize, j;
   matUpdateTimes(SACdataArray[index].times);
   matUpdateStation(SACdataArray[index].station);
   matUpdateEvent(SACdataArray[index].event);
   matUpdateUser(SACdataArray[index].user);
   matUpdateDataDescrip(SACdataArray[index].descrip);
   matUpdateHeaderEvsta(SACdataArray[index].evsta);
   matUpdateHeaderLLNL(SACdataArray[index].llnl);
   matUpdateHeaderDepMec(SACdataArray[index].depmec);
   matUpdateResponse(SACdataArray[index].response);
 
   /* Copy double data to real array */
   for(j=0;j<nlen;j++) 
      DatafloatPtrReal[j] = SACdataArray[index].trcReal[j]; 


   /* Copy imaginary data if present */
   if( *iftype == *irlim || *iftype == *iamph){
      printf("updating imaginary \n");
      for(j=0;j<nlen;j++)
         DatafloatPtrImag[j] = SACdataArray[index].trcImag[j]; 
   }

   return TRUE;

}


/* function to destroy the SACdataArray */
void matDestroySACdataArray(void)
 {
    long j;
    
    if( SACdataArray == (struct SACdata *) NULL ) return;
    for(j=0;j<NumberofTraces;j++){

       if(SACdataArray[j].trcReal != (double *)NULL)
          free(SACdataArray[j].trcReal);
       if(SACdataArray[j].trcImag != (double *)NULL)
          free(SACdataArray[j].trcImag); 
    }
    free(SACdataArray);
    SACdataArray = (struct SACdata *) NULL;
 
 }



long matGetNumChans(void)
{
   return NumberofTraces;
}


long matGetMaxTraceLen(void)
{
   long j;
   long maxLen;
   maxLen=0;

   for(j=0;j<NumberofTraces;j++){
      if(SACdataArray[j].traceLength > maxLen)maxLen = SACdataArray[j].traceLength;
   }
   return maxLen;
}



double *matGetSeisRealPtr(long index)
{
   return SACdataArray[index].trcReal;
}

double *matGetSeisImagPtr(long index)
{
   return SACdataArray[index].trcImag;
}



long matGetSeisLength(long index)
{
   return SACdataArray[index].traceLength;
}   




 struct hdrTimes *matGetTimesPtr(long index)
{
   return &(SACdataArray[index].times);
}


struct hdrStation *matGetStationPtr(long index)
{
   return &(SACdataArray[index].station);
}


struct hdrEvent *matGetEventPtr(long index)
{
   return &(SACdataArray[index].event);
}

 struct hdrUser *matGetUserPtr(long index)
{
   return SACdataArray[index].user;
}

struct hdrDataDescrip *matGetDescripPtr(long index)
{
   return &(SACdataArray[index].descrip );
}

double  *matGetResponsePtr(long index)
{
   return SACdataArray[index].response;
}

double *matGetTraceLengthPtr(long index)
{
   return &(SACdataArray[index].traceLength);
}


struct hdrEventSta *matGetEvstaPtr(long index)
{
   return &(SACdataArray[index].evsta );
}

 struct hdrLLNLextensions *matGetLLNLPtr(long index)
{
   return &(SACdataArray[index].llnl );
}

 struct hdrDepMec *matGetDepMecPtr(long index)
{
   return &(SACdataArray[index].depmec );
}

#endif /* HAVE_MATLAB */

#ifndef HAVE_MATLAB

void __matHeaderDataManage_undef_symbol() { }

#endif 
