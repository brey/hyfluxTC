#include <stdlib.h>
 
#if defined addunderscore
#define OPENGDAL opengdal_
#define GETGDALATTR getgdalattr_
#define GETGDALVAL0 getgdalval0_
#define GETNODATA getnodata_
#define CLOSEGDAL closegdal_
#define OUTPCRASTER outPCRaster_

#define OUTGDAL0 outgdal0_
#define PUTGDALATTR putgdalattr_
#endif

#define REAL8 double

/* ------------------------------------------------------------ */


#include "gdal.h"
#include "cpl_vsi.h"
#include "cpl_conv.h"
#include "cpl_string.h"

/* ------------------------------------------------------------ */
    GDALDatasetH	hDataset, hOutDS;
     GDALRasterBandH	hBand;
    char		**papszMetadata;
    unsigned int			i, nSrcBand = -1;
    unsigned int			nRasterXSize, nRasterYSize;
    const char		*pszSource=NULL, *pszDest=NULL, *pszFormat = "GTiff";
    GDALDriverH		hDriver;
    unsigned int			*panBandList, nBandCount;
    double		adfGeoTransform[6];
    GDALDataType	eOutputType = GDT_Unknown;
    unsigned int			nOXSize = 0, nOYSize = 0;
    char                **papszCreateOptions = NULL;
    unsigned int                 anSrcWin[4], bStrict = TRUE;
    const char          *pszProjection=NULL;
    char                *DataType=NULL, *Metadata=NULL ;
    unsigned int			nPixelSize=-1;
//    double 		noDataValue='0xFFFFFFFFFFFFFFFF';
    double 		noDataValue=99999.;


/* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
/*         OPENGDAL:  open map and get parameters                                    */
/* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
#if defined WIN32
extern void __stdcall OPENGDAL (name, ldum1, ncol,nrow, PCRtyp, xstart, ystart, cellsizex, cellsizey, inpfmt, ldum2, nband, error)
#else
void OPENGDAL (name, ncol,nrow,PCRtyp, xstart, ystart, cellsizex, cellsizey,inpfmt,  nband, error, ldum1,ldum2)
#endif
 
char *name , *inpfmt;
unsigned int *ldum1, *ldum2 ;
unsigned int *ncol, *nrow, *nband, *PCRtyp;
unsigned *error;
double *xstart, *ystart, *cellsizex,  *cellsizey ;
{
/* ------------------------------------------------------------ */

    anSrcWin[0] = 0;
    anSrcWin[1] = 0;
    anSrcWin[2] = 0;
    anSrcWin[3] = 0;


/* -------------------------------------------------------------------- */
/*      Register standard GDAL drivers, and identify output driver.     */
/* -------------------------------------------------------------------- */

    GDALAllRegister();


pszSource = name ;

if ( *error < 0 ) printf("name= %s ldum1= %d ldum2= %d \n",name,ldum1,ldum2);

*error = 0;

/* -------------------------------------------------------------------- */
/*      Attempt to open source file.                                    */
/* -------------------------------------------------------------------- */

    hDataset = GDALOpen( pszSource, GA_ReadOnly );
    
    if( hDataset == NULL )
    {
        fprintf( stderr,
                 "GDALOpen failed - %d\n%s\n",
                 CPLGetLastErrorNo(), CPLGetLastErrorMsg() );
*error = 1;
return;
    }

	nBandCount = GDALGetRasterCount( hDataset );
*nband=nBandCount;

    nRasterXSize = GDALGetRasterXSize( hDataset );
    nRasterYSize = GDALGetRasterYSize( hDataset );
*ncol = nRasterXSize ;
*nrow = nRasterYSize ;
    
    if( anSrcWin[2] == 0 && anSrcWin[3] == 0 )
    {
        anSrcWin[2] = nRasterXSize;
        anSrcWin[3] = nRasterYSize;
    }

    hDriver = GDALGetDatasetDriver( hDataset );
    // printf( "Driver: %s\n", GDALGetDriverShortName( hDriver ) );


strExport ( GDALGetDriverShortName( hDriver ) , inpfmt, ldum2 ) ;

/*----------------------------------------------------------------------------*/
// get Metadata and set PCRtyp

    papszMetadata = GDALGetMetadata( hDataset, NULL );

*PCRtyp = 0;
   if( CSLCount(papszMetadata) > 0 ) 
{
       if ( EQUAL(papszMetadata[0],"PCRASTER_VALUESCALE=VS_BOOLEAN"  )) { *PCRtyp = 1; }
  else if ( EQUAL(papszMetadata[0],"PCRASTER_VALUESCALE=VS_NOMINAL"  )) { *PCRtyp = 2; }
  else if ( EQUAL(papszMetadata[0],"PCRASTER_VALUESCALE=VS_ORDINAL"  )) { *PCRtyp = 3;  }
  else if ( EQUAL(papszMetadata[0],"PCRASTER_VALUESCALE=VS_SCALAR"   )) { *PCRtyp = 4;  }
  else if ( EQUAL(papszMetadata[0],"PCRASTER_VALUESCALE=VS_DIRECTION" )) { *PCRtyp = 5;  }
  else if ( EQUAL(papszMetadata[0],"PCRASTER_VALUESCALE=VS_LDD"      )) { *PCRtyp = 6; }
}

// printf ("PCRTyp=%d \n",PCRtyp);

/* -------------------------------------------------------------------- */
/*      Get Projection                                            */
/* -------------------------------------------------------------------- */

    // printf ("get projection=%s \n",GDALGetProjectionRef(hDataset ));


/* -------------------------------------------------------------------- */
/*      Report Geotransform.                                            */
/* -------------------------------------------------------------------- */
    if( GDALGetGeoTransform( hDataset, adfGeoTransform ) == CE_None )
    {
        if( adfGeoTransform[2] == 0.0 && adfGeoTransform[4] == 0.0 )
        {

*xstart = adfGeoTransform[0];
*ystart = adfGeoTransform[3];
*cellsizex = adfGeoTransform[1];
*cellsizey = adfGeoTransform[5];
        }
        else
        {
            printf( "Not able to get origin and pixel size \n" );
*error = 2;
return;
        }
    }

}

/* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */ 
/*   		strExport:	export c string to fortran string	   */
/* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */ 

void strExport ( const char *c_str, char *f_str, int lstr )
{  

unsigned int lbuff=strlen(c_str) ;
if (lbuff>lstr) lbuff=lstr;
// printf ("lbuff=%d  lstr=%d \n",lbuff,lstr);

  for ( i = 0;  i < lbuff ; i++ )   f_str[ i ] = c_str[ i ];
  for ( ; i < lstr; i++ ) f_str[ i ] = ' ';
//  for ( ; i < lstr; i++ ) f_str[ i ] = '';
// printf ("done strExport \n");

}

/* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */ 
/* 		CLOSEGDAL	close map                                  */
/* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
#if defined WIN32
extern void __stdcall CLOSEGDAL ()
#else
void CLOSEGDAL ()
#endif
 
{
 
    GDALClose( hDataset );
}

/* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
/* 		GETGDALATTR	get raster attribute & size (in byte)	   */
/* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
 
#if defined WIN32
extern void __stdcall GETGDALATTR (iband,size,ctype,ldum)
#else
void GETGDALATTR (iband,size,ctype,ldum)
#endif
 
unsigned int *iband, *size, *ldum ;
char *ctype ;
{ 
int type;
        nSrcBand = *iband;
        hBand = GDALGetRasterBand( hDataset, nSrcBand );
        type = GDALGetRasterDataType( hBand ) ;               
        DataType = GDALGetDataTypeName(type);
        strExport(DataType , ctype, ldum) ;
        *size = GDALGetDataTypeSize( type ) / 8;
}

/* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
/* 		GETNODATA	get nodata value			   */
/* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
 
#if defined WIN32
extern void __stdcall GETNODATA (nodata)
#else
void  GETNODATA (nodata)
#endif

 double *nodata ;
{ 
    int             bHasNoData=FALSE ;
        *nodata = GDALGetRasterNoDataValue( hBand, &bHasNoData ) ;               
}

/* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
/* 		PUTGDALATTR	put raster attribute 			   */
/* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
 
#if defined WIN32
extern void __stdcall PUTGDALATTR (iband,ctype,ldum)
#else
void PUTGDALATTR (iband,ctype,ldum)
#endif
 
unsigned int *iband, *ldum ;
char *ctype ;
{ 
        DataType = ctype;
        nSrcBand = *iband;
}

/* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
/* 		GETGDALVAL0 	get band values 		`	   */
/* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
 
#if defined WIN32
extern void __stdcall GETGDALVAL0 (iband,buf,nbuf,nodata)
#else
void GETGDALVAL0 (iband,buf,nbuf,nodata)
#endif
 
void *buf;
unsigned int *iband, *nbuf ;
double *nodata;
{ 

/* -------------------------------------------------------------------- */
/*      Establish some parameters.                                      */
/* -------------------------------------------------------------------- */

        nOXSize = anSrcWin[2];
        nOYSize = anSrcWin[3];
    
int jcount=0 ;
/* -------------------------------------------------------------------- */
/*       copying bands.                                                 */
/* -------------------------------------------------------------------- */

        int     iBlockY;
        nSrcBand = *iband;
        hBand = GDALGetRasterBand( hDataset, nSrcBand );


/* -------------------------------------------------------------------- */
int             bHasNoData=FALSE ;
*nodata = GDALGetRasterNoDataValue( hBand, &bHasNoData ) ;        
*nbuf = nRasterXSize *  nRasterYSize ;

       
int type = GDALGetRasterDataType( hBand ) ;               
        
//        printf( "Band %d RasterDataType = %d\n",
//                nSrcBand , GDALGetRasterDataType( hBand ) );

/* -------------------------------------------------------------------- */
/*      Write out the raw raster data.                                  */
/* -------------------------------------------------------------------- */
        nPixelSize = GDALGetDataTypeSize( GDALGetRasterDataType(hBand) ) / 8;


//        printf( "Block size %d nPixelSize %d nOXSize %d \n",nPixelSize*nOXSize,nPixelSize,nOXSize);

        for( iBlockY = 0; iBlockY < nOYSize; iBlockY++ )
        {

            int		iSrcYOff;

            if( nOYSize == anSrcWin[3] )
                iSrcYOff = iBlockY + anSrcWin[1];
            else
            {
                iSrcYOff = (iBlockY / (double) nOYSize) * anSrcWin[3]
                    + anSrcWin[1];
                iSrcYOff = MAX(0,MIN(anSrcWin[3]-1,iSrcYOff));
            }

            GDALRasterIO( hBand, GF_Read,
                          anSrcWin[0], iSrcYOff, anSrcWin[2], 1,
                          buf+jcount, nOXSize, 1,
                          type,
                          0, 0 );

jcount+=nPixelSize*nOXSize;
        }


  GDALClose( hDataset );

}  


/* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */
/*  	OUTGDAL: 	create a new map  with assigned attribute  	   */
/* ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */

#if defined WIN32
extern void __stdcall OUTGDAL0 (name, ldum1, ncol,nrow, PCRtyp, xstart, ystart, cellsizex, buf, nval, cellsizey, outfmt, ldum2, nband, error)
#else
void OUTGDAL0 (name, ncol,nrow,PCRtyp, xstart, ystart, cellsizex, buf, nval, cellsizey, outfmt,  nband, error, ldum1,ldum2)
#endif
 
 
char *name , *outfmt ;
unsigned int *ldum1, *ldum2;
unsigned int *ncol, *nrow, *nband, *nval, *PCRtyp;
unsigned *error;
double *xstart, *ystart, *cellsizex,  *cellsizey ;

void *buf;

{
*error = 0;

if ( EQUAL(outfmt,"PCRaster" ) ) 
{
// printf("\n OUTGDAL: call OUTPCRASTER map=%s \n ",name);
  OUTPCRASTER (name, ncol,nrow,PCRtyp
		, xstart, ystart, cellsizex,buf,nval,ldum1);
return ;
}


nOXSize = *ncol;
nOYSize = *nrow;
pszDest = name;

adfGeoTransform[0] = *xstart ;
adfGeoTransform[1] = *cellsizex ;
adfGeoTransform[2] = 0.0 ; 
adfGeoTransform[3] = *ystart ;
adfGeoTransform[4] = 0.0 ;

if ( *PCRtyp == 0 ) {
adfGeoTransform[5] = *cellsizey ;
nBandCount = *nband;
} else {
adfGeoTransform[5] = - *cellsizex ;
nBandCount = 1 ;
nSrcBand = 1 ;
}

//if ( EQUAL(outfmt,"GTiff" ) )  pszFormat = "GTiff" ;
// else 
pszFormat = outfmt ;



/* -------------------------------------------------------------------- */
/*      Find the output driver.                                         */
/* -------------------------------------------------------------------- */

    hDriver = GDALGetDriverByName( pszFormat );
    if( hDriver == NULL )
    {
        int	iDr;
        
        printf("OUTGDAL: map %s \n", name );
        printf("Output driver `%s' not recognised.\n", outfmt );
*error = 1;
return ;
    }


/* -------------------------------------------------------------------- */
switch (*PCRtyp)
{
case 0 :  Metadata=NULL ; break ;
case 1 :  Metadata="PCRASTER_VALUESCALE=VS_BOOLEAN" ; DataType="Byte" ; break ;
case 2 :  Metadata="PCRASTER_VALUESCALE=VS_NOMINAL" ; DataType="Int32"; break ;
case 3 :  Metadata="PCRASTER_VALUESCALE=VS_ORDINAL" ; DataType="Int32" ; break ;
case 4 :  Metadata="PCRASTER_VALUESCALE=VS_SCALAR" ; DataType="Float32" ; break ;
case 5 :  Metadata="PCRASTER_VALUESCALE=VS_DIRECTION" ; DataType="Float32"; break ;
case 6 :  Metadata="PCRASTER_VALUESCALE=VS_LDD" ; DataType="Byte" ; break ;
default :  Metadata=NULL ; DataType="Float32" ;  
} 

// printf("\n OUTGDAL \n name= %s outfmt=%s PCRtyp=%d DataType=%s ncol=%d nrow=%d nband=%d iband=%d \n",name,outfmt,*PCRtyp,DataType,*ncol,*nrow,*nband, nSrcBand);

/* -------------------------------------------------------------------- */
            int	iType;
            
            for( iType = 1; iType < GDT_TypeCount; iType++ )
            {
                if ( EQUAL(GDALGetDataTypeName((GDALDataType)iType), DataType ) )
                {
                    eOutputType = (GDALDataType) iType;
                }
            }

        nPixelSize = GDALGetDataTypeSize( eOutputType ) / 8;

// printf ("DataType=%s Metadata=%s NPixelSize=%d \n ",DataType,Metadata,nPixelSize) ;


 // printf ("pszDest=%s nOXSize=%d nOYSize=%d nBandCount=%d eOutputType=%d \n ",
// pszDest, nOXSize, nOYSize, nBandCount, eOutputType);



/* -------------------------------------------------------------------- */
/*      Create the output database.                                     */
/* -------------------------------------------------------------------- */
//    GDALTermProgress( 0.0, NULL, NULL );

		if ( EQUAL(pszFormat,"GTiff") ) {
papszCreateOptions = CSLAddString( papszCreateOptions, "COMPRESS=LZW" );
	GDALValidateCreationOptions(hDriver,papszCreateOptions); 
 } 		else
			papszCreateOptions = CSLAddString( papszCreateOptions, "" );

    hOutDS = GDALCreate( hDriver, pszDest, nOXSize, nOYSize, 
                         nBandCount, eOutputType, papszCreateOptions );
    if( hOutDS == NULL )
    {
        printf( "GDALCreate() failed.\n" );
*error = 2 ;
return;
}
// else  printf ("Created output database %s \n",pszDest);

    
/* -------------------------------------------------------------------- */
/*	Set projection							*/
/* -------------------------------------------------------------------- */

 // printf ("Set projection %s \n",pszProjection);

    if( pszProjection != NULL && strlen(pszProjection) > 0 )
{
        GDALSetProjection( hOutDS, pszProjection );
}

 // printf ("get projection=%s \n",GDALGetProjectionRef(hOutDS ));

/* -------------------------------------------------------------------- */
/*	Set Metadata							*/
/* -------------------------------------------------------------------- */

if ( pszFormat == "PCRaster" )
{
  GDALSetMetadata( hDataset, Metadata, NULL );
 // printf ("Set Metadata %s \n",Metadata);
}
/* -------------------------------------------------------------------- */
/*      Set Geotransform.                                            */
/* -------------------------------------------------------------------- */

    GDALSetGeoTransform( hOutDS, adfGeoTransform ) ;


/* -------------------------------------------------------------------- */
//		write one band
/* -------------------------------------------------------------------- */ 

        int     iBlockY;
        GDALRasterBandH hDstBand;

        hDstBand = GDALGetRasterBand( hOutDS, nSrcBand );
        GDALSetRasterNoDataValue(hDstBand, noDataValue);

/* -------------------------------------------------------------------- */
/*      Write out the raw raster data.                                  */
/* -------------------------------------------------------------------- */
int jcount=0;
        for( iBlockY = 0; iBlockY < nOYSize; iBlockY++ )
        {
            int		iSrcYOff;

            if( nOYSize == anSrcWin[3] )
                iSrcYOff = iBlockY + anSrcWin[1];
            else
            {
                iSrcYOff = (iBlockY / (double) nOYSize) * anSrcWin[3]
                    + anSrcWin[1];
                iSrcYOff = MAX(0,MIN(anSrcWin[3]-1,iSrcYOff));
            }


            GDALRasterIO( hDstBand, GF_Write,
                          0, iBlockY, nOXSize, 1,
                          buf+jcount, nOXSize, 1,
                          eOutputType,
                          0, 0 );
jcount+=nPixelSize*nOXSize;

        }
    GDALClose( hOutDS );     
}
 
