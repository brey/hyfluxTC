 
#include <stdio.h>
 
 
#if defined addunderscore
#define GETBIN getbin_
#endif
 
#if defined WIN32
extern void __stdcall GETBIN (name, ldum,sizerec,shift,buf)
#else
void GETBIN (name,sizerec,shift,buf, ldum)
#endif
 
char *name;
int *ldum,*shift;
size_t *sizerec;
void *buf;
{
FILE *f;
size_t size=1;

f=fopen(name,"r"); 
fseek(f,*shift,SEEK_SET);
fread (buf,*sizerec,size,f);
fclose(f);
 
}
 
