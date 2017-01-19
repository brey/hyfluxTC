 
#include <stdlib.h>
#include <stdio.h>
 
 
 
#if defined addunderscore
#define PUTENV90 putenv90_
#endif
 
#if defined WIN32
extern void __stdcall PUTENV90 (name, ldum)
#else
void PUTENV90 (name, ldum)
#endif
 
char *name;
unsigned int *ldum;
{
int iret;
char *name1;
 
  iret=_putenv(name);
      printf( "iret=: %d\n", iret );
name1 = getenv("day");
 
      printf( "name1=: %s\n", name1 );
 
}
 
