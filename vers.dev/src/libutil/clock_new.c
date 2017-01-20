#include <time.h>

#if addunderscore
#define ELAPSTIME_NEW elapstime_new_
#define CLOCK_NEW clock_new_
#endif

/*------------------------------------------------------------*/

#if defined WIN32
#define EXTERN_VOID extern void __stdcall 
#else
#define EXTERN_VOID void 
#endif

/*------------------------------------------------------------*/
 
EXTERN_VOID ELAPSTIME_NEW (tsec)
float *tsec;
{
struct timeval { 
        time_t          tv_sec;    /* seconds */ 
/*        suseconds_t     tv_usec;  */      /* microseconds */ 
}; 
struct timeval tim;
gettimeofday(&tim, NULL);
*tsec=tim.tv_sec  ;
}

/*------------------------------------------------------------*/

EXTERN_VOID CLOCK_NEW (itime,istep)
unsigned int *istep, *itime;
{
   *itime = clock();
#if sun
  *istep = 1000000;
#else
  *istep = CLOCKS_PER_SEC;
#endif
}

