#include <stdlib.h>
#include "csf.h"
 
#if defined addunderscore
#define OPENPCRASTER openPCRaster_
#define GETPCRASTERVAL getPCRasterVal_
#define CLOSEPCRASTER closePCRaster_


#define OUTPCRASTER outPCRaster_

#define GETMV1 getmv1_
#define GETMV2 getmv2_
#define GETMV4 getmv4_
#endif
 
static CSF_VS VS;
static CSF_PT Proj;
static REAL8 Ang, Xul,Yul, Size;
static CSF_CR CR;      /* cell representation of new map  */
static size_t Nc, Nr;
static MAP *map;
/* static UINT4 VS; */

/* ------------------------------------------------------------ */
 
#if defined WIN32
extern void __stdcall OPENPCRASTER (name, ldum, ncol,nrow,typ,nbyte
      , xstart, ystart, cellsize, error)
#else
void OPENPCRASTER (name, ncol,nrow,typ,nbyte
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
 
/* get values & close the map */
 
#if defined WIN32
extern void __stdcall GETPCRASTERVAL (buf)
#else
void GETPCRASTERVAL (buf)
#endif
 
void *buf;
{
 
RgetSomeCells (map, 0, Nr*Nc, buf) ;
 
  Mclose(map);
}


/* ------------------------------------------------------------ */
 
/* close the map */
 
#if defined WIN32
extern void __stdcall CLOSEPCRASTER ()
#else
void CLOSEPCRASTER ()
#endif
 
 
{
 
  Mclose(map);
}


/* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
/*  create a new map  with assigned attribute and a defined type 1-6 */
// put cell values
/* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
#if defined WIN32
extern void __stdcall OUTPCRASTER (name, ldum, ncol,nrow,typ
		, xstart, ystart, cellsize,buf,nval)
#else
void  OUTPCRASTER (name, ncol,nrow,typ
		, xstart, ystart, cellsize,buf,nval,ldum)
#endif
 
char *name;
unsigned int *ncol, *nrow;
double *xstart, *ystart, *cellsize;
 
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
 
/* get MV for
CR_UINT1
*/
 
#if defined WIN32
extern void __stdcall GETMV1 (mv)
#else
void GETMV1 (mv)
#endif
 
UINT1 *mv;
{
*mv = MV_UINT1;
}
/* ------------------------------------------------------------ */
 
/* get MV for
CR_INT2
*/
 
#if defined WIN32
extern void __stdcall GETMV2 (mv)
#else
void GETMV2 (mv)
#endif
 
UINT2 *mv;
{
*mv = MV_INT2;
}
/* ------------------------------------------------------------ */
 
/* get MV for
CR_INT4, typ=2
*/
 
#if defined WIN32
extern void __stdcall GETMV4 (mv)
#else
void GETMV4 (mv)
#endif
 
UINT4 *mv;
{
*mv = MV_INT4;
}
