#define DELETEBIT       1UL<<31
#define READONLYBIT     1<<30
#define INDIRECTBIT     1<<29
#define SHAREDBIT       1<<28
#define RESERVEDBIT     1<<27
#define VALUETYPEBITS    037
#define VALUETYPESHIFT   22
#define APPLICATIONBIT1 1<<21
#define APPLICATIONBIT2 1<<20
#define NAMELENGTHBITS  07
#define NAMELENGTHSHIFT	17
#define LONGVALUEBIT    1<<16
#define VALUELENGTHBITS	0177777


void encodevdesc( descriptor, deleteflag, readonlyflag, indirectflag, 
		   sharedflag, reservedflag, applflag1, applflag2,
                   valuetype, desclength, namelength, valuelength )
long *descriptor, *deleteflag, *readonlyflag, *indirectflag, 
     *sharedflag, *reservedflag, *applflag1, *applflag2, 
     *valuetype, *namelength, *valuelength, *desclength;
/*
*=====================================================================
* PURPOSE: To encode the vars descriptor from indvidual fields
*=====================================================================
* INPUT ARGUMENTS:
*   *deleteflag:    logical (long) TRUE if delete bit to be set.
*   *readonlyflag:  logical (long) TRUE if readonly bit to be set.
*   *indirectflag:  logical (long) TRUE if indirect bit to be set. 
*   *sharedflag:    logical (long) TRUE if shared bit to be set. 
*   *reservedflag:  logical (long) TRUE if reserved bit to be set. 
*   *applflag1:     logical (long) TRUE if first application bit to be set. 
*   *applflag2:     logical (long) TRUE if second application bit to be set. 
*   *valuetype:     long integer contains datatype field.
*   *namelength:    long integer contains namelength field.
*   *valuelength:   long integer contains valuelength field.
*=====================================================================
* OUTPUT ARGUMENTS:
*   *descriptor:    pointer to 32 vars descriptor.
*   *desclength:    long integer contains length (number of words) of descriptor.
*=====================================================================
* MODULE/LEVEL: vars/5
*=====================================================================
* MODIFICATION HISTORY:
*    920320:  Portability to IBM RISC 6000 via precompiler flag.
*    881104:  Modifications due to further changes in descriptor format.
*             
*    870208:  Modifications due to change in vars descriptor format.
*    861218:  Original version.
*=====================================================================
* DOCUMENTED/REVIEWED:  881104
*=====================================================================
*/
{
long register descrip, val ;

   descrip = 0L ;
   if ( *deleteflag )  descrip |= DELETEBIT ;
   if ( *readonlyflag )  descrip |= READONLYBIT ;
   if ( *indirectflag )  descrip |= INDIRECTBIT ;
   if ( *sharedflag )  descrip |= SHAREDBIT ;
   if ( *reservedflag )  descrip |= RESERVEDBIT ;
   descrip |= ( *valuetype & VALUETYPEBITS ) << VALUETYPESHIFT ;
   if ( *applflag1 )  descrip |= APPLICATIONBIT1 ;
   if ( *applflag2 )  descrip |= APPLICATIONBIT2 ;
   descrip |= ( ( *namelength - 1L ) & NAMELENGTHBITS ) << NAMELENGTHSHIFT ; 
   if ( ( val = *valuelength - 1L ) > VALUELENGTHBITS ) {
      descrip |= LONGVALUEBIT ;
      *descriptor = descrip ;
      *++descriptor = val ;
      *desclength = 2L ;
      }
    else {
      descrip |= val & VALUELENGTHBITS ;
      *descriptor = descrip ;
      *desclength = 1L ;
      }
}
