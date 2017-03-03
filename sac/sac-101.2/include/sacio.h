
#ifndef __SACIO_H__
#define __SACIO_H__

/** 
 * Define an automatic string length from C
 *
 */
#define SAC_STRING_LENGTH   -1

#ifndef TRUE
#define TRUE  1
#endif /* TRUE */

#ifndef FALSE
#define FALSE 0
#endif /* FALSE */

#define SAC_FLOAT_UNDEFINED      -12345.0
#define SAC_REAL_UNDEFINED       SAC_FLOAT_UNDEFINED
#define SAC_INT_UNDEFINED        -12345
#define SAC_INTEGER_UNDEFINED    SAC_INT_UNDEFINED
#define SAC_NUMBER_UNDEFINED     SAC_INT_UNDEFINED
#define SAC_CHAR_UNDEFINED       "-12345  "
#define SAC_CHARACTER_UNDEFINED  SAC_CHAR_UNDEFINED

void getfhv(char      *kname, 
            float     *fvalue, 
            long int  *nerr, 
            int        kname_s);

void getihv(char      *kname, 
            char      *kvalue, 
            long int  *nerr, 
            int        kname_s, 
            int        kvalue_s);

void getkhv(char      *kname, 
            char      *kvalue, 
            long int  *nerr, 
            int        kname_s, 
            int        kvalue_s);

void getlhv(char      *kname, 
            long int  *lvalue, 
            long int  *nerr, 
            int        kname_s);

void getnhv(char      *kname, 
            long int  *nvalue, 
            long int  *nerr, 
            int        kname_s);

void newhdr ();

void rsac1(char      *kname, 
           float      yarray[], 
           long int  *nlen, 
           float     *beg, 
           float     *del, 
           long int  *max_, 
           long int  *nerr, 
           long int   kname_s);

void rsac2(char      *kname, 
           float      yarray[], 
           long int  *nlen, 
           float      xarray[], 
           long int  *max_, 
           long int  *nerr, 
           long int   kname_s);

void rsach(char *kname,
           int  *nerr,
           int   kname_s);

void setfhv(char      *kname, 
            float     *fvalue, 
            long int  *nerr, 
            long int   kname_s);

void setihv(char      *kname, 
            char      *kvalue, 
            long int  *nerr, 
            long int   kname_s,
            long int   kvalue_s);

void setkhv(char      *kname, 
            char      *kvalue, 
            long int  *nerr, 
            long int   kname_s,
            long int   kvalue_s);

void setlhv(char      *kname, 
            long int  *lvalue, 
            long int  *nerr, 
            long int   kname_s);

void setnhv(char      *kname, 
            long int  *nvalue, 
            long int  *nerr, 
            long int   kname_s);

void wsac0(char      *kname, 
           float     *xarray, 
           float     *yarray,
           long int  *nerr,
           long int   kname_s);

void wsac1(char      *kname, 
           float     *yarray, 
           long int  *nlen, 
           float     *beg, 
           float     *del, 
           long int  *nerr, 
           long int   kname_s);

void wsac2(char      *kname, 
           float     *yarray, 
           long int  *nlen, 
           float     *xarray, 
           long int  *nerr, 
           long int   kname_s);

void wsac3(char      *kname, 
           float     *xarray, 
           float     *yarray,
           long int  *nerr,
           long int   kname_s);

void getbbv(char      *kname, 
            char      *kvalue, 
            long int  *nerr, 
            long int   kname_s, 
            long int   kvalue_s);

void writebbf(char      *kname, 
              long int  *nerr, 
              long int   kname_s);

void readbbf(char      *kname, 
             long int  *nerr, 
             long int   kname_s);


void setbbv(char      *kname, 
            char      *kvalue, 
            long int  *nerr, 
            long int   kname_s, 
            long int   kvalue_s);



#endif /* __SACIO_H__ */
