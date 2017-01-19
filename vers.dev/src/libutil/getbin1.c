 
#include <stdio.h>
 
 
#if defined addunderscore
#define GETBIN1 getbin1_
#define PUTBIN1 putbin1_
#endif
 
#if defined WIN32
extern void __stdcall GETBIN1 (name, ldum,nval,sizeval,buf)
#else
void GETBIN1 (name,nval,sizeval,buf, ldum)
#endif
 
char *name;
unsigned int *ldum;
unsigned int *sizeval,*nval ;
void *buf[];
{
FILE *f;
size_t size=1, sizerec=(*sizeval) * (*nval);
/* printf ("sizerec= %d \n",sizerec); */
 
f=fopen(name,"r");
fseek(f,0,SEEK_SET);
fread (buf,sizerec,size,f);
 
fclose(f);
 
}
 
#if defined WIN32
extern void __stdcall PUTBIN1 (name, ldum,nval,sizeval,buf)
#else
void PUTBIN1 (name,nval,sizeval,buf, ldum)
#endif
 
char *name;
unsigned int *ldum;
unsigned int *sizeval,*nval ;
void *buf[];
{
FILE *f;
size_t size=1, sizerec=(*sizeval) * (*nval);
/* printf ("sizerec= %d \n",sizerec); */
 
f=fopen(name,"w+");
fwrite (buf,sizerec,size,f);
 
fclose(f);
 
}
