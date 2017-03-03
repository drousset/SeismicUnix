#ifndef MAT_HEADER_DATA_MANAGE_H
#   define MAT_HEADER_DATA_MANAGE_H

   extern long matGetNumChans(void);
   extern long matGetMaxTraceLen(void);
   extern double *matGetSeisRealPtr(long) ;
   extern double *matGetSeisImagPtr(long) ;
   extern long matGetSeisLength(long );
   extern struct hdrTimes *matGetTimesPtr(long );
   extern struct hdrStation *matGetStationPtr(long );
   extern struct hdrEvent *matGetEventPtr(long );
   extern struct hdrUser *matGetUserPtr(long );
   extern struct hdrDataDescrip *matGetDescripPtr(long );
   extern struct hdrEventSta *matGetEvstaPtr(long);
   extern struct hdrLLNLextensions *matGetLLNLPtr(long );
   extern struct hdrDepMec *matGetDepMecPtr(long );
   extern double  *matGetResponsePtr(long );
   extern double *matGetTraceLengthPtr(long );
#endif
