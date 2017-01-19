
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
// #include <direct.h>

/*------------------------------------------------------------*/

#if defined WIN32
#define EXTERN_VOID extern void __stdcall 
#else
#define EXTERN_VOID void 
#endif


typedef enum {
   FileType_NoFile = 0,
   FileType_Directory,
   FileType_RegularFile,
   FileType_SpecialFile   /* pipe, FIFO, device, etc. */
} ;

/* --------------------- Utilities.Internal --------------------------------- */
#if addunderscore
#define STATDIR statdir_
#endif
/* ----------------------------------------------------------------------- */ 


#if defined WIN32
EXTERN_VOID STATDIR (name,ldum,type)
#else
EXTERN_VOID STATDIR (name,type,ldum)
#endif
char *name;
unsigned int *ldum;
int *type;
{
int result;
     struct stat statinfo;

     result = stat(name, &statinfo);
   if (result < 0 ) {
	*type = FileType_NoFile;
    return;
   } else {
     switch (statinfo.st_mode & S_IFMT) {
     case S_IFREG:  *type = FileType_RegularFile ; break;
     case S_IFDIR:  *type = FileType_Directory; break;
     default:  *type = FileType_SpecialFile; break;}
}
}

