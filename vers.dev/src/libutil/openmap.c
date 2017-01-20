#include <stdlib.h>
#include "csf.h"
 
#if defined addunderscore
#define GETMAPATTR getmapattr_
#define GETMAP getmap_
#define OPENMAP openmap_
#define CLOSEMAP closemap_
#define OPENMAPTYPE openmaptype_
#define OPENMAPTYPE1 openmaptype1_
#define OPENMAPTYPE2 openmaptype2_
#define GETVALMAP getvalmap_
#define GETVALMAP1 getvalmap1_
#define PUTVALMAP1 putvalmap1_
#define NEWMAP newmap_
#define NEWMAPTYPE1 newmaptype1_
#define NEWMAPTYPE2 newmaptype2_
#define NEWMAPTYPE3 newmaptype3_
#endif
 
static CSF_VS VS;
static CSF_PT Proj;
static REAL8 Ang, Xul,Yul, Size;
static CSF_CR CR;      /* cell representation of new map  */
static size_t Nc, Nr;
static MAP *map;
/* static UINT4 VS; */
/* ------------------------------------------------------------ */
/* open a map, store attributes, return nval
   caution ! the map is kept open */
/* ------------------------------------------------------------ */
 
#if defined WIN32
extern void __stdcall GETMAPATTR (name, ldum)
#else
void GETMAPATTR (name, ldum)
#endif
 
char *name;
unsigned int *ldum;
{
 
 /* fprintf(stderr,"%s: map= \n",name); */
 
  map = Mopen(name, M_READ);
  if (map == NULL)
  {
     fprintf(stderr,"%s: Error from Mopen \n ",name);
  	  Mperror(name);
      exit(1);
  };
 
 Nr = map->raster.nrRows;
 Nc = map->raster.nrCols;
 Xul = map->raster.xUL;
 Yul = map->raster.yUL;
 Size = map->raster.cellSizeX;
 VS = map->raster.valueScale;
 Proj = map->main.projection,
 Ang = map->raster.angle;
 CR = map->raster.cellRepr;
 
 
 Mclose(map);
 
}
 
/* ------------------------------------------------------------ */
 
#if defined WIN32
extern void __stdcall OPENMAP (name, ldum, ncol,nrow)
#else
void OPENMAP (name, ncol,nrow, ldum)
#endif
 
char *name;
unsigned int *ldum;
unsigned int *ncol, *nrow;
{
 
 /* fprintf(stderr,"%s: map= \n",name); */
 
  map = Mopen(name, M_READ);
  if (map == NULL)
  {
     fprintf(stderr,"%s: Error from Mopen \n ",name);
  	  Mperror(name);
      exit(1);
  };
 
 Nr = map->raster.nrRows;
 Nc = map->raster.nrCols;
 Xul = map->raster.xUL;
 Yul = map->raster.yUL;
 Size = map->raster.cellSizeX;
 VS = map->raster.valueScale;
 Proj = map->main.projection,
 Ang = map->raster.angle;
 CR = map->raster.cellRepr;
 
 
 /* Mclose(map); */
 
 *ncol = Nc;
 *nrow = Nr;
 
 
}
 
/* ------------------------------------------------------------ */
 
#if defined WIN32
extern void __stdcall GETMAP (name, ldum, ncol,nrow,buf,retcode)
#else
void GETMAP (name, ncol,nrow,buf,retcode, ldum)
#endif
 
void *buf;
char *name;
unsigned int *ldum;
unsigned int *ncol, *nrow, *retcode;
{
 
 /* fprintf(stderr,"%s: map= \n",name); */
 
  *retcode = 0;
  map = Mopen(name, M_READ);
  if (map == NULL)
  {
     fprintf(stderr,"%s: Error from Mopen \n ",name);
  	  Mperror(name);
       *retcode = 1;
      exit(0);
  };
 
 Nr = map->raster.nrRows;
 Nc = map->raster.nrCols;

  if (Nr != *nrow ) { *retcode = 2;};
  if (Nc != *ncol ) { *retcode = 2;};
 
RgetSomeCells (map, 0, Nr*Nc, buf) ;
 
 Mclose(map); 
 
}
 
/* ------------------------------------------------------------ */
 
 
#if defined WIN32
extern void __stdcall OPENMAPTYPE1 (name, ldum, ncol,nrow,typ
						   , xstart, ystart, cellsize)
#else
void OPENMAPTYPE1 (name, ncol,nrow,typ
						   , xstart, ystart, cellsize, ldum)
#endif
 
char *name;
unsigned int *ldum;
unsigned int *ncol, *nrow;
unsigned *typ;
REAL8 *xstart, *ystart, *cellsize;
{
 
 /* fprintf(stderr,"%s: map= \n",name); */
 
  map = Mopen(name, M_READ);
  if (map == NULL)
  {
     fprintf(stderr,"%s: Error from Mopen \n ",name);
  	  Mperror(name);
      exit(1);
  };
 
 Nr = map->raster.nrRows;
 Nc = map->raster.nrCols;
 Xul = map->raster.xUL;
 Yul = map->raster.yUL;
 Size = map->raster.cellSizeX;
 VS = map->raster.valueScale;
 Proj = map->main.projection,
 Ang = map->raster.angle;
 CR = map->raster.cellRepr;
 
 
 /* Mclose(map); */
 
 *ncol = Nc;
 *nrow = Nr;
 *xstart = Xul;
 *ystart = Yul;
 *cellsize = Size;

/*
 printf("map=%s \n",name); 
printf ("ncol=%d nrow=%d xstart=%g ystart=%g cellsize=%g \n",Nc,Nr,Xul,Yul,Size);
*/
 
	switch (VS)
	{
	case VS_BOOLEAN		: *typ = 1; break;
	case VS_NOMINAL		: *typ = 2; break;
	case VS_ORDINAL		: *typ = 3; break;
	case VS_SCALAR		: *typ = 4; break;
	case VS_DIRECTION	: *typ = 5; break;
	case VS_LDD			: *typ = 6; break;
	default				: *typ=0;
	}
 
}
/* ------------------------------------------------------------ */
 
 
#if defined WIN32
extern void __stdcall OPENMAPTYPE2 (name, ldum, ncol,nrow,typ,nbyte
      , xstart, ystart, cellsize, error)
#else
void OPENMAPTYPE2 (name, ncol,nrow,typ,nbyte
      , xstart, ystart, cellsize, error,ldum)
#endif
 
char *name;
unsigned int *ldum;
unsigned int *ncol, *nrow;
unsigned int *typ,*nbyte,*error;
REAL8 *xstart, *ystart, *cellsize;
{
 
 /* fprintf(stderr,"%s: map= \n",name); */
 
  map = Mopen(name, M_READ);

  if (map == NULL) {
  *error = 1;
   return;
    } else {
   *error = 0;
  };
 
 Nr = map->raster.nrRows;
 Nc = map->raster.nrCols;
 Xul = map->raster.xUL;
 Yul = map->raster.yUL;
 Size = map->raster.cellSizeX;
 VS = map->raster.valueScale;
 Proj = map->main.projection,
 Ang = map->raster.angle;
 CR = map->raster.cellRepr;
 
 
 /* Mclose(map); */
 
 *ncol = Nc;
 *nrow = Nr;
 *xstart = Xul;
 *ystart = Yul;
 *cellsize = Size;
 
	switch (VS)
	{
	case VS_BOOLEAN		: *typ = 1; break;
	case VS_NOMINAL		: *typ = 2; break;
	case VS_ORDINAL		: *typ = 3; break;
	case VS_SCALAR		: *typ = 4; break;
	case VS_DIRECTION	: *typ = 5; break;
	case VS_LDD			: *typ = 6; break;
	default				: *typ=0;
	}

       switch (CR)
	{
        case CR_REAL8	: *nbyte = 8; break;
        case CR_REAL4	: *nbyte = 4; break;
        case CR_UINT4	: *nbyte = 4; break;
        case CR_INT4	: *nbyte = 4; break;
        case CR_UINT2	: *nbyte = 2; break;
        case CR_INT2	: *nbyte = 2; break;
        case CR_UINT1	: *nbyte = 1; break;
        case CR_INT1	: *nbyte = 1; break;
        default		: *nbyte = 0;
        }
 
}
/* ------------------------------------------------------------ */
 
/* close the map */
 
#if defined WIN32
extern void __stdcall CLOSEMAP ()
#else
void CLOSEMAP ()
#endif
 
 
{
 
  Mclose(map);
}
/* ------------------------------------------------------------ */
 
/* get values & close the map */
 
#if defined WIN32
extern void __stdcall GETVALMAP (buf)
#else
void GETVALMAP (buf)
#endif
 
void *buf;
{
 
RgetSomeCells (map, 0, Nr*Nc, buf) ;
 
  Mclose(map);
}
/* ------------------------------------------------------------ */
 
/* get values from a starting point */
 
#if defined WIN32
extern void __stdcall GETVALMAP1 (shift,nval,buf)
#else
void GETVALMAP1 (shift,nval,buf)
#endif


unsigned int *shift,*nval;
 
void *buf;
{
RgetSomeCells (map, *shift, *nval, buf) ;
}
/* ------------------------------------------------------------ */
 
/* put values from a starting point */
 
#if defined WIN32
extern void __stdcall PUTVALMAP1 (shift,nval,buf)
#else
void PUTVALMAP1 (shift,nval,buf)
#endif
 
unsigned int *shift,*nval;
 
void *buf;
{

RputSomeCells (map, *shift, *nval, buf) ;
 
}

/* ------------------------------------------------------------ */
/*  create a new map using the attributes stored previously */
 
#if defined WIN32
extern void __stdcall NEWMAP (name, ldum, buf, nval)
#else
void NEWMAP (name, buf, nval, ldum)
#endif
 
char *name;
unsigned int *ldum, *nval;
void *buf;
{
MAP *newMap;
 
if ( *nval != Nr*Nc)
 {
  fprintf(stderr,"%u: wrong no of values nval = \n",*nval
	  ,"%u Nr =\n",Nr
	  ,"%u Nc =\n",Nc );
  exit(1);
 }
 
newMap = Rcreate(name, Nr, Nc, CR, VS_SCALAR, Proj, Xul, Yul, Ang, Size);
 
if (newMap==NULL)
	{Mperror(name);
	exit (1);
	};
 
RputSomeCells (newMap, 0, *nval, buf) ;
 
  Mclose(newMap);
 
}
/* ------------------------------------------------------------ */
/*  create a new map using the attributes stored previously, but with
a defined type 1-6 */
 
#if defined WIN32
extern void __stdcall NEWMAPTYPE1 (name, ldum, buf, nval,typ)
#else
void NEWMAPTYPE1 (name, buf, nval,typ, ldum)
#endif
 
char *name;
unsigned int *ldum, *nval,*typ;
void *buf;
{
MAP *newMap;
CSF_VS VSnew;      /* cell representation of new map  */
CSF_CR CRnew;      /* cell representation of new map  */
 
if ( *nval != Nr*Nc)
 {
  fprintf(stderr,"%u: wrong no of values nval = \n",*nval
	  ,"%u Nr =\n",Nr
	  ,"%u Nc =\n",Nc );
  exit(1);
 }
 
	switch (*typ)
	{
	case 1 : VSnew = VS_BOOLEAN; CRnew = CR_UINT1;
				break;
	case 2 : VSnew = VS_NOMINAL ; CRnew=CR_INT4;
				break;
	case 3 : VSnew = VS_ORDINAL; CRnew = CR_INT4 ;
			    break;
	case 4 : VSnew = VS_SCALAR; CRnew = CR_REAL4 ;
			    break;
	case 5 : VSnew = VS_DIRECTION; CRnew = CR_REAL4 ;
			    break;
	case 6 : VSnew = VS_LDD; CRnew = CR_UINT1 ;
			    break;
	}
 
newMap = Rcreate(name, Nr, Nc, CRnew, VSnew, Proj, Xul, Yul, Ang, Size);
 
if (newMap==NULL)
	{Mperror(name);
	exit (1);
	};
 
RputSomeCells (newMap, 0, *nval, buf) ;
 
  Mclose(newMap);
 
}
/* ------------------------------------------------------------ */
/*  create a new map  with assigned attribute and
a defined type 1-6 */
#if defined WIN32
extern void __stdcall NEWMAPTYPE2 (name, ldum, ncol,nrow,typ
		, xstart, ystart, cellsize,buf,nval)
#else
void  NEWMAPTYPE2 (name, ncol,nrow,typ
		, xstart, ystart, cellsize,buf,nval,ldum)
#endif
 
char *name;
unsigned int *ncol, *nrow;
REAL8 *xstart, *ystart, *cellsize;
 
unsigned int *ldum, *nval,*typ;
void *buf;
{
MAP *newMap;
CSF_VS VSnew;      /* cell representation of new map  */
CSF_CR CRnew;      /* cell representation of new map  */
 
Nc= *ncol ;
Nr= *nrow ;
Xul= *xstart;
Yul= *ystart;
Size= *cellsize;
Proj = PT_YDECT2B;
Ang = 0;
 
if ( *nval != Nr*Nc)
 {
  fprintf(stderr,"%u: wrong no of values nval = \n",*nval
	  ,"%u Nr =\n",Nr
	  ,"%u Nc =\n",Nc );
  exit(1);
 }
 
	switch (*typ)
	{
	case 1 : VSnew = VS_BOOLEAN; CRnew = CR_UINT1; 				break;
	case 2 : VSnew = VS_NOMINAL ; CRnew=CR_INT4; 				break;
	case 3 : VSnew = VS_ORDINAL; CRnew = CR_INT4 ; 			    break;
	case 4 : VSnew = VS_SCALAR; CRnew = CR_REAL4 ; 			    break;
	case 5 : VSnew = VS_DIRECTION; CRnew = CR_REAL4 ; 		    break;
	case 6 : VSnew = VS_LDD; CRnew = CR_UINT1 ; 			    break;
	case 7 : VSnew = VS_SCALAR; CRnew = CR_REAL8 ; 			    break;
	}
 
newMap = Rcreate(name, Nr, Nc, CRnew, VSnew, Proj, Xul, Yul, Ang, Size);
 
if (newMap==NULL)
	{Mperror(name);
	exit (1);
	};
 
RputSomeCells (newMap, 0, *nval, buf) ;
 
  Mclose(newMap);
 
}
 
/* ------------------------------------------------------------ */
/*  create a new map  with assigned attribute and
a defined type 1-6 */
#if defined WIN32
extern void __stdcall NEWMAPTYPE3 (name, ldum, ncol,nrow,typ
		, xstart, ystart, cellsize)
#else
void  NEWMAPTYPE3 (name, ncol,nrow,typ
		, xstart, ystart, cellsize,ldum)
#endif
 
char *name;
unsigned int *ncol, *nrow;
REAL8 *xstart, *ystart, *cellsize;
 
unsigned int *ldum, *typ;
{
CSF_VS VSnew;      /* cell representation of new map  */
CSF_CR CRnew;      /* cell representation of new map  */
 
Nc= *ncol ;
Nr= *nrow ;
Xul= *xstart;
Yul= *ystart;
Size= *cellsize;
Proj = PT_YDECT2B;
Ang = 0;
 
	switch (*typ)
	{
	case 1 : VSnew = VS_BOOLEAN; CRnew = CR_UINT1;
				break;
	case 2 : VSnew = VS_NOMINAL ; CRnew=CR_INT4;
				break;
	case 3 : VSnew = VS_ORDINAL; CRnew = CR_INT4 ;
			    break;
	case 4 : VSnew = VS_SCALAR; CRnew = CR_REAL4 ;
			    break;
	case 5 : VSnew = VS_DIRECTION; CRnew = CR_REAL4 ;
			    break;
	case 6 : VSnew = VS_LDD; CRnew = CR_UINT1 ;
			    break;
	}

 
/*  fprintf(stderr,"shift =  %u \n",shift1) ; */
/*  fprintf(stderr,"nval =  %u \n",*nval) ; */

 	map = Rcreate(name, Nr, Nc, CRnew, VSnew, Proj, Xul, Yul, Ang, Size); 
}
 
