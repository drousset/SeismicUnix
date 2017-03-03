/* ../../inc/mem.h */

/*  old initializations
#ifdef DOINITS
long int *const isacmem = (long*)cmmem.sacmem;

long *const Isacmem = (long*)&cmmem.sacmem[0] - 1;

float *const Sacmem = &cmmem.sacmem[0] - 1;
#else
extern long int *const isacmem;
extern long *const Isacmem;
extern float *const Sacmem;
#endif
   old initializations */

/* These dynamic pointers now must be initialized and
   managed in routines like iniam, allamb, reaamb.
   Isacmem and Sacmem will no longer be needed, I
   think.                                        */

#ifdef DOINITS
  struct t_cmmem cmmem = { 0, NULL };
#else
  extern struct t_cmmem cmmem;
#endif
