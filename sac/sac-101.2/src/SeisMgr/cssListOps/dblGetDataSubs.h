#ifndef DBL_GET_DATA_SUBS_H
#define DBL_GET_DATA_SUBS_H

long dblGetE1 ( long int NPTS , FILE *fptr , struct wfdiscList * wfStruc );
long dblGetS2I2G2 ( long int NPTS ,
                    FILE *fptr ,
                    struct wfdiscList * wfStruc ,
                    char * pType );
long dblGetS3 ( long int NPTS , FILE * fptr , struct wfdiscList *wfStruc );
long dblGetT4F4 ( long int NPTS , FILE * fptr , struct wfdiscList *wfStruc ,
                  char* pType );
long dblGetS4I4 ( long int NPTS , FILE *fptr , struct wfdiscList *wfStruc ,
                  char* pType );
long dblGetT8F8 ( long int NPTS , FILE * fptr , struct wfdiscList *wfStruc ,
                  char* pType );
void enlarge(FILE *fp, long outMax, long* data, /* long int * byteOff,*/ long* nerr);                    


#endif
