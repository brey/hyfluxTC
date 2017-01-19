#include "definitions.h"
#include "gdal_int.h"
#include "stdlib.h"
#include <stdio.h>
#include "gdal.h"

#include "stringlib.h"
#include "gdal_alg.h"
#include "ogr_srs_api.h"
#ifdef LINUX
	#include "cpl_string.h"
#endif
#include "cpl_conv.h"
#include "cpl_multiproc.h"

GDALDriverH drv;
int gdalInit(char * fname);
GDALDriverH CPL_DLL CPL_STDCALL GDALGetDriverByName( const char * );
static int 
GDALInfoReportCorner( GDALDatasetH hDataset, 
                      OGRCoordinateTransformationH hTransform,
                      const char * corner_name,
                      double x, double y );
void getColHeight(float hei, int * colred, int * colgreen, int * colblue, float hgrey, float HCutOff, float hmax);
void getColDepth(float dept, int * colred, int * colgreen, int * colblue);
void writeASCII(char * fname, float xmin, float xmax,float ymin,float ymax, int nXSize, int nYSize, int offsetIX, int offsetIY, double factor, float  h[(NROWS+1)][(NCOLS+1)]);
//boolean compString1(char *s1, char *s2, int size);

void gdalGetLine(int ix0, int dx, int iy0, int dy, char * pszSrc, int mode, GDALRasterBandH  &dstband, int Iband, GDALDatasetH  &hDstDS,  int &nXSize, int &nYSize, float &xmin, float &dxCell, float &ymax, float &dyCell, int &ierr, double * scanline)
{   
	if (mode==0 || mode==3)
	{
		GDALDriverH     hDriver;
		int nBands;
	
		GDALAllRegister();
		//printf("Opening file for reading: %s\n",pszSrc);
		hDstDS=GDALOpen(pszSrc,GA_ReadOnly);
		hDriver = GDALGetDatasetDriver( hDstDS );
	
		nXSize=GDALGetRasterXSize( hDstDS ); 
		nYSize=GDALGetRasterYSize( hDstDS );
		nBands=GDALGetRasterCount( hDstDS );

	
		double Geotransform[7];

		GDALGetGeoTransform( hDstDS, Geotransform ) ;

		xmin=(float) Geotransform[0];
		dxCell=(float) Geotransform[1];
		ymax=(float) Geotransform[3];
		dyCell=(float) Geotransform[5];
		float ymin=ymax+dyCell*nYSize;

		//GDALRasterBandH dstband;
		dstband=GDALGetRasterBand(hDstDS,Iband);

		int bGotNodata;
		double dfNoData = GDALGetRasterNoDataValue( dstband, &bGotNodata );
   //     if( bGotNodata )
   //     {
			//scanline[0]=dfNoData;
   //         printf( "  NoData Value=%.18g\n", dfNoData );
   //     }

	}
	if (mode==1 || mode==3)
	{  
		CPLErr ret;

		//nXSize=GDALGetRasterXSize( hDstDS ); 
		//nYSize=GDALGetRasterYSize( hDstDS );

		if (ix0<0 || ix0>=nXSize || iy0<0 || iy0>=nYSize)
		{
			ierr=-1;
		}
		ierr=0;
		ret=GDALRasterIO( dstband, GF_Read, ix0, iy0, dx,dy, 
						  scanline, dx, dy, GDT_Float64, 0, 0 );
	}
	if (mode==2 || mode==3)
	{
		GDALClose( hDstDS );
		//printf("Closed file: %s\n",pszSrc);
		
		
	}
}

void gdalCreateFile(char * pszDest, char * pszFormat, float xmin, float xmax,float ymin,float ymax, int nXSize, int nYSize, int offsetIX, int offsetIY, double factor, float  h[(NROWS+1)][(NCOLS+1)], float d[(NROWS+1)][(NCOLS+1)], bool flipVertical, float hgrey, float HCutOff, float hmax, int colorTransp )
{
	GDALDriverH     hDriver;
   
	GDALDataType    eOutputType = GDT_Float32;
    char            **papszCreateOptions = NULL;
    float hei;
    bool onlyInundation=false;
	int nBands;
	if (colorTransp<0 && colorTransp !=-1)
	{ colorTransp=-colorTransp;onlyInundation=true;}
	
	//printf("Creating file: %s\n",pszDest);
	if (compString(pszFormat,"AAIGrid"))
	{
		writeASCII(pszDest,xmin,xmax,ymin,ymax,nXSize,nYSize,offsetIX,offsetIY,factor,h);
	}
	if (compString(pszFormat,"GTiff") || compString(pszFormat,"AAIGrid"))
	{
		eOutputType = GDT_Float32;
		nBands =1;
		if (compString(pszFormat,"GTiff"))
			papszCreateOptions = CSLAddString( papszCreateOptions, "COMPRESS=LZW" );
		else
			papszCreateOptions = CSLAddString( papszCreateOptions, "" );
	}
	if (compString(pszFormat,"JPEG") || compString(pszFormat,"BMP") || compString(pszFormat,"PNG"))
	{
		eOutputType = GDT_Byte;
		nBands =3;
	}


	GDALAllRegister();

/* -------------------------------------------------------------------- */
/*      Create target raster file.                                      */
/* -------------------------------------------------------------------- */
	
	hDriver = GDALGetDriverByName( pszFormat );
    if( hDriver == NULL )
    {
        //int	iDr;
        
        printf( "Output driver `%s' not recognised.\n", pszFormat );
	}
    // FIXME
    if ( nXSize == 0 )
        nXSize = 256;
    if ( nYSize == 0 )
        nYSize = 256;
	
	
	GDALDatasetH hDstDS;

	int ret=GDALValidateCreationOptions(hDriver,papszCreateOptions);
    hDstDS = GDALCreate( hDriver, pszDest, nXSize, nYSize, nBands,
                         eOutputType, papszCreateOptions );
    if ( hDstDS == NULL )
    {
        fprintf( stderr, "Unable to create target dataset \"%s\".\n",
                 pszDest );
        fprintf( stderr, "%s\n", CPLGetLastErrorMsg() );
        exit( 3 );
    }

	double Geotransform[7];

	Geotransform[0] = xmin;
	Geotransform[1] = (xmax - xmin) / nXSize;
	Geotransform[2] = 0;
	Geotransform[3] = ymax;
	Geotransform[4] = 0;
	Geotransform[5] = (ymin - ymax) / nYSize;
	Geotransform[6] = 0;
	if (compString(pszFormat,"GTiff"))
	{
		char WKT[]="GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,298.2572235629972,AUTHORITY[\"EPSG\",\"7030\"]],AUTHORITY[\"EPSG\",\"6326\"]],PRIMEM[\"Greenwich\",0],UNIT[\"degree\",0.0174532925199433],AUTHORITY[\"EPSG\",\"4326\"]]";
		GDALSetProjection(hDstDS,WKT);
	}
	GDALSetGeoTransform(hDstDS,Geotransform);


	if (compString(pszFormat,"GTiff") || compString(pszFormat,"AAIGrid"))
	{
		GDALRasterBandH dstband;
		dstband=GDALGetRasterBand(hDstDS,1);

		GDALSetRasterOffset(dstband,0.0);
		GDALSetRasterScale(dstband, 1.0);

		GDALSetRasterNoDataValue(dstband,99999.0);
		double * scanline;
		scanline = (double *) VSIMalloc(sizeof(double) * nXSize);

		CPLErr ret;
		int kx;
		//FILE * outfile;
		//outfile = fopen("d:\\output_W1.txt", "w");
		int iLine,iy,ix;
		for( iLine = 0; iLine < nYSize ; iLine++ )
		{
			for (kx=0;kx<nXSize;kx++)
			{
				iy=iLine+offsetIY+1;
				ix=kx+offsetIX+1;
				if (flipVertical) iy=nYSize-iy+offsetIY+2;
				scanline[kx] =h[iy][ix] * factor;
				//fprintf(outfile,"%i %i %f   iline=%i kx=%i  iline*10000+kx=%i  \n",ix,iy,h[iy][ix],iLine,kx, iLine*10000+kx);
			}
			ret=GDALRasterIO( dstband, GF_Write, 0, iLine, nXSize, 1, 
						  scanline, nXSize, 1, GDT_Float64, 0, 0 );
		}
		//fclose(outfile);
	}
	
	else
	{
		GDALRasterBandH dstband_red,dstband_green,dstband_blue;

		dstband_red=GDALGetRasterBand(hDstDS,1);
		dstband_green=GDALGetRasterBand(hDstDS,2);
		dstband_blue=GDALGetRasterBand(hDstDS,3);

	
		GDALSetRasterOffset(dstband_red,0.0);
		GDALSetRasterScale(dstband_red, 1.0);
		GDALSetRasterNoDataValue(dstband_red,-1.0);
		
		GDALSetRasterOffset(dstband_green,0.0);
		GDALSetRasterScale(dstband_green, 1.0);
		GDALSetRasterNoDataValue(dstband_green,-1.0);
		
		GDALSetRasterOffset(dstband_blue,0.0);
		GDALSetRasterScale(dstband_blue, 1.0);
		GDALSetRasterNoDataValue(dstband_blue,-1.0);
		
		unsigned char  * scanline_red, * scanline_green,  * scanline_blue;
		
		scanline_red   = (unsigned char *) VSIMalloc(sizeof(unsigned char) * nXSize);
		scanline_green = (unsigned char *) VSIMalloc(sizeof(unsigned char) * nXSize);
		scanline_blue  = (unsigned char *) VSIMalloc(sizeof(unsigned char) * nXSize);

		//CPLErr ret;
		int kx,ix,iy;
		int red=0,green=0,blue=0;
		int iLine;
		for( iLine = 0; iLine < nYSize ; iLine++ )
		{
			for (kx=0;kx<nXSize;kx++)
			{
				//iy=iLine+offsetIY+1;
				//if (flipVertical) iy=nYSize-iy;
				iy=iLine+offsetIY+1;
				ix=kx+offsetIX+1;
				if (flipVertical) iy=nYSize-iy+offsetIY+2;
				hei=(float) (h[iy][ix] * factor);
				getColHeight(hei, &red, &green, &blue, hgrey, HCutOff, hmax);
				if (colorTransp==-1 )
				{
					if ((red==-1 && green==-1 && blue==-1) || d[iy][ix]<0)
					{
						hei=(float) (d[iy][ix] * factor);
						getColDepth(hei, &red, &green, &blue);
					}
				}
				else
				{
					if ( colorTransp>1000) 
					// hyflux
					{  if (red==-1 && green==-1 && blue==-1)	
							{red=colorTransp-1000;green=colorTransp-1000;blue=colorTransp-1000;}
						else
						if (onlyInundation)
						{
							if(d[iy][ix]<0)
								{red=255;green=0;blue=128;}
							else
								{red=colorTransp-1000;green=colorTransp-1000;blue=colorTransp-1000;}
						}
						else
						{
							if(d[iy][ix]<0)
								{red=255;green=0;blue=128;}
						}
					}
					else
					// swan
					{					
					if (red==-1 && green==-1 && blue==-1 || d[iy][ix]<0)	
					{red=colorTransp;green=colorTransp;blue=colorTransp;}
					}
				}
				scanline_red[kx] = red;
				scanline_green[kx] = green;
				scanline_blue[kx] = blue;
			}
			GDALRasterIO( dstband_red, GF_Write, 0, iLine, nXSize, 1, 
						  scanline_red, nXSize, 1, GDT_Byte, 0, 0 );
			GDALRasterIO( dstband_green, GF_Write, 0, iLine, nXSize, 1, 
						  scanline_green, nXSize, 1, GDT_Byte, 0, 0 );
			GDALRasterIO( dstband_blue, GF_Write, 0, iLine, nXSize, 1, 
						  scanline_blue, nXSize, 1, GDT_Byte, 0, 0 );
		}

	}
	
	GDALClose( hDstDS );

}

float gdalGetValue(float lon, float lat, char * pszSrc, int mode, GDALRasterBandH  &dstband, GDALDatasetH  &hDstDS,  int &nXSize, int &nYSize, float &xmin, float &dxCell, float &ymax, float &dyCell, int &ierr)
{   
	if (mode==0)
	{
		GDALDriverH     hDriver;
		int nBands;
	
		GDALAllRegister();
		//printf("Opening file for reading: %s\n",pszSrc);
		hDstDS=GDALOpen(pszSrc,GA_ReadOnly);
		hDriver = GDALGetDatasetDriver( hDstDS );
	
		nXSize=GDALGetRasterXSize( hDstDS ); 
		nYSize=GDALGetRasterYSize( hDstDS );
		nBands=GDALGetRasterCount( hDstDS );

	
		double Geotransform[7];

		GDALGetGeoTransform( hDstDS, Geotransform ) ;

		xmin=(float) Geotransform[0];
		dxCell=(float) Geotransform[1];
		ymax=(float) Geotransform[3];
		dyCell=(float) Geotransform[5];
		float ymin=ymax+dyCell*nYSize;

		//GDALRasterBandH dstband;
		dstband=GDALGetRasterBand(hDstDS,1);
		return 0;
	}
	if (mode==0 || mode==1 || mode==3)
	{  
		double *scanline1,*scanline2,*scanline3,*scanline4;
		scanline1= (double*) calloc(2,sizeof(double));
		scanline2= (double*) calloc(1,sizeof(double));
		scanline3= (double*) calloc(2,sizeof(double));
		scanline4= (double*) calloc(1,sizeof(double));
		CPLErr ret;

		//nXSize=GDALGetRasterXSize( hDstDS ); 
		//nYSize=GDALGetRasterYSize( hDstDS );
		float xf,yf;
		xf	=((lon - xmin) / dxCell)-0.5;
		int ix,iy;
		ix	= (int) xf;
		yf=((-ymax + lat) / dyCell)-0.5;
		iy = (int) yf;
		//printf("%i    %i \n",ix,iy);
		if (ix==nXSize-1 || ix==nXSize || ix==nXSize-2)
			ix=nXSize-3;
		if (iy==nYSize-1 || iy==nYSize)
		{
			iy=nYSize-2;
		}
		if (ix<0 || ix>=nXSize || iy<0 || iy>=nYSize)
		{
			ierr=-1;
			return 0;
		}
		ierr=0;
		//printf("%i  %i  nXSize=%i  nYSize=%i \n",ix,iy,nXSize,nYSize);
		float value;
		if (dxCell<3 && mode!=3 )
		{
			ret=GDALRasterIO( dstband, GF_Read, ix, iy, 1, 1, 
							  scanline1, 2, 1, GDT_Float64, 0, 0 );
			ret=GDALRasterIO( dstband, GF_Read, ix+1, iy, 1, 1, 
							  scanline2, 1, 1, GDT_Float64, 0, 0 );
			ret=GDALRasterIO( dstband, GF_Read, ix, iy+1, 1, 1, 
							  scanline3, 2, 1, GDT_Float64, 0, 0 );
			ret=GDALRasterIO( dstband, GF_Read, ix+1, iy+1, 1, 1, 
							  scanline4, 1, 1, GDT_Float64, 0, 0 );
			//printf("%f  %f  %f  %f\n",scanline1[0],scanline2[0],scanline3[0],scanline4[0]);
			float value1=(xf-ix)*scanline2[0]+(1-xf+ix)*scanline1[0];
			float value2=(xf-ix)*scanline4[0]+(1-xf+ix)*scanline3[0];
			
			//float value1=(xf-ix)*scanline1[1]+(1-xf+ix)*scanline1[0];
			//float value2=(xf-ix)*scanline3[1]+(1-xf+ix)*scanline3[0];
		
			value= (yf-iy)*value2+(1-yf+iy)*value1;
			//printf("%f  %f   %f\n",value1,value2,value);
			
		}
		else
		{
			ret=GDALRasterIO( dstband, GF_Read, ix, iy, 1, 1, 
			scanline1, 1, 1, GDT_Float64, 0, 0 );
			value= scanline1[0];
		}
		return (float) value;
	}
	if (mode==2)
	{
		GDALClose( hDstDS );
		//printf("Closed file: %s\n",pszSrc);
		return 0;
		
	}
	return -1;
}

void gdalGetLine(char * pszSrc, int mode, GDALRasterBandH  &dstband, GDALDatasetH  &hDstDS,  int &nXSize, int &nYSize, float &xmin, float &dxCell, float &ymax, float &dyCell, int &ierr, double * scanline)
{   
	if (mode==0)
	{
		GDALDriverH     hDriver;
		int nBands;
	
		GDALAllRegister();
		//printf("Opening file for reading: %s\n",pszSrc);
		hDstDS=GDALOpen(pszSrc,GA_ReadOnly);
		if(hDstDS==0)
		{
			printf("\n****  this file does not exists or cannot be read by GDAL: %s\n\n",pszSrc);
			return;
		}
		hDriver = GDALGetDatasetDriver( hDstDS );
	
		nXSize=GDALGetRasterXSize( hDstDS ); 
		nYSize=GDALGetRasterYSize( hDstDS );
		nBands=GDALGetRasterCount( hDstDS );

	
		double Geotransform[7];

		GDALGetGeoTransform( hDstDS, Geotransform ) ;

		xmin=(float) Geotransform[0];
		dxCell=(float) Geotransform[1];
		ymax=(float) Geotransform[3];
		dyCell=(float) Geotransform[5];
		float ymin=ymax+dyCell*nYSize;

		//GDALRasterBandH dstband;
		dstband=GDALGetRasterBand(hDstDS,1);
		int bGotNodata;
		double dfNoData = GDALGetRasterNoDataValue( dstband, &bGotNodata );
        if( bGotNodata )
        {
            printf( "  NoData Value=%.18g\n", dfNoData );
        }

	}
	if (mode==0 || mode==1)
	{  
		CPLErr ret;

		//nXSize=GDALGetRasterXSize( hDstDS ); 
		//nYSize=GDALGetRasterYSize( hDstDS );

		int ix = 0; //(int) ((lon - xmin) / dxCell);
		int iy = 0; //(int) ((-ymax + lat) / dyCell);	
		if (ix<0 || ix>=nXSize || iy<0 || iy>=nYSize)
		{
			ierr=-1;
		}
		ierr=0;
		ret=GDALRasterIO( dstband, GF_Read, ix, iy, 1, 1, 
						  scanline, 1, 1, GDT_Float64, 0, 0 );
	}
	if (mode==2)
	{
		GDALClose( hDstDS );
		//printf("Closed file: %s\n",pszSrc);
		
		
	}
}

void gdalGetParam(char * pszSrc, float *xmin, float *xmax, float *ymin, float *ymax, float *dx, float *dy, int * nXSize, int *  nYSize)
{
	gdalInfo(pszSrc);

	GDALDriverH     hDriver;
    int nBands;
	
	GDALAllRegister();

	GDALDatasetH hDstDS;
	hDstDS=GDALOpen(pszSrc,GA_ReadOnly);
	if(hDstDS==0)
	{
		printf("\n****  this file does not exists or cannot be read by GDAL: %s\n\n",pszSrc);
		return ;
	}

	hDriver = GDALGetDatasetDriver( hDstDS );
	
	*nXSize=GDALGetRasterXSize( hDstDS ); 
    *nYSize=GDALGetRasterYSize( hDstDS );
	nBands=GDALGetRasterCount( hDstDS );


	/* -------------------------------------------------------------------- */
/*      Create target raster file.                                      */
/* -------------------------------------------------------------------- */
	
	
	double Geotransform[7];

	GDALGetGeoTransform( hDstDS, Geotransform ) ;

	*xmin=(float) Geotransform[0];
	*xmax=(float) Geotransform[1]*(*nXSize-1) +*xmin;
	*ymax=(float) Geotransform[3];
	*ymin=(float) (*ymax+Geotransform[5] *(*nYSize-1));
	float dummy;
	if(*ymin>*ymax) {dummy=*ymin;*ymin=*ymax;*ymax=dummy;}
	*dx=(*xmax-*xmin)/(*nXSize);
    *dy=(*ymax-*ymin)/(*nYSize);
	GDALClose( hDstDS );
}
int gdalReadFile(char * pszSrc,  int offsetIX, int offsetIY, double factor, float  h[(NROWS+1)][(NCOLS+1)], bool flipvertical)
{
	GDALDriverH     hDriver;
    int nBands;
	
	GDALAllRegister();

	GDALDatasetH hDstDS;
	hDstDS=GDALOpen(pszSrc,GA_ReadOnly);
	if(hDstDS==0)
	{
		printf("\n****  this file does not exists or cannot be read by GDAL: %s\n\n",pszSrc);
		gdalInfo(pszSrc);
		return -1;
	}

	hDriver = GDALGetDatasetDriver( hDstDS );
	
	int nXSize=GDALGetRasterXSize( hDstDS ); 
    int nYSize=GDALGetRasterYSize( hDstDS );
	nBands=GDALGetRasterCount( hDstDS );

/* -------------------------------------------------------------------- */
/*      Create target raster file.                                      */
/* -------------------------------------------------------------------- */
	
	
	double Geotransform[7];

	GDALGetGeoTransform( hDstDS, Geotransform ) ;

	float xmin=(float) Geotransform[0];
	float xmax=(float) Geotransform[1]*(nXSize-1) +xmin;
	float ymax=(float) Geotransform[3];
	float ymin=(float) (ymax+Geotransform[5] *(nYSize-1));
	float dummy;
	if(ymin>ymax) {dummy=ymin;ymin=ymax;ymax=dummy;}
	GDALRasterBandH dstband;
	dstband=GDALGetRasterBand(hDstDS,1);
	int bGotNodata;
	double dfNoData = GDALGetRasterNoDataValue( dstband, &bGotNodata );
   //     if( bGotNodata )
   //     {
			//scanline[0]=dfNoData;
   //         printf( "  NoData Value=%.18g\n", dfNoData );
   //     }


	double * scanline;
	scanline = (double *) VSIMalloc(sizeof(double) * nXSize);

	//CPLErr ret;
	int kx;
	//FILE *outfile;
	
	/* Open the file.  If NULL is returned there was an error */
	//outfile = fopen("output_R.txt", "w");
	int iLine,iy,ix;
	for( iLine = 0; iLine < nYSize ; iLine++ )
	{
		GDALRasterIO( dstband, GF_Read, 0, iLine, nXSize, 1, 
						scanline, nXSize, 1, GDT_Float64, 0, 0 );
		//printf("%i",iLine);
		for (kx=0;kx<nXSize;kx++)
		{
			iy=iLine+offsetIY+1;
			if (flipvertical) iy=nYSize-iy+offsetIY+2;
			ix=kx+offsetIX+1 ;
			//fprintf(outfile,"ix=%i  iy=%i  val=%f\n",ix,iy,scanline[kx]);

			h[iy][ix]=(float) (scanline[kx] *factor);
		}
		
		
	}
	
	
	//fclose(outfile);
	GDALClose( hDstDS );

	return 0;
}

int gdalInfo(char * pszFilename)
{
    GDALDatasetH	hDataset;
    GDALRasterBandH	hBand;
    int			i, iBand;
    double		adfGeoTransform[6];
    GDALDriverH		hDriver;
    char		**papszMetadata;
    int                 bComputeMinMax = FALSE, bSample = FALSE;
    int                 bShowGCPs = TRUE, bShowMetadata = TRUE, bShowRAT=TRUE;
    int                 bStats = FALSE, bApproxStats = TRUE, iMDD;
    int                 bShowColorTable = TRUE, bComputeChecksum = FALSE;
    int                 bReportHistograms = FALSE;
    char              **papszExtraMDDomains = NULL, **papszFileList;
    const char  *pszProjection = NULL;
    OGRCoordinateTransformationH hTransform = NULL;
	
	GDALAllRegister();
//	GDALAllRegister;
//	drv = GDALGetDriverByName("GTiff") ;
/* -------------------------------------------------------------------- */
/*      Open dataset.                                                   */
/* -------------------------------------------------------------------- */
    hDataset = GDALOpen( pszFilename, GA_ReadOnly );
    
    if( hDataset == NULL )
    {
        fprintf( stderr,
                 "gdalinfo failed - unable to open '%s'.\n",
                 pszFilename );

        //CSLDestroy( argv );
    
        GDALDumpOpenDatasets( stderr );

        GDALDestroyDriverManager();

        CPLDumpSharedList( NULL );

        return 0; //exit( 1 );
    }
    
/* -------------------------------------------------------------------- */
/*      Report general info.                                            */
/* -------------------------------------------------------------------- */
    hDriver = GDALGetDatasetDriver( hDataset );
    printf( "Driver: %s/%s\n",
            GDALGetDriverShortName( hDriver ),
            GDALGetDriverLongName( hDriver ) );

    papszFileList = GDALGetFileList( hDataset );
    if( CSLCount(papszFileList) == 0 )
    {
        printf( "Files: none associated\n" );
    }
    else
    {
        printf( "Files: %s\n", papszFileList[0] );
        for( i = 1; papszFileList[i] != NULL; i++ )
            printf( "       %s\n", papszFileList[i] );
    }
    CSLDestroy( papszFileList );

    printf( "Size is %d, %d\n",
            GDALGetRasterXSize( hDataset ), 
            GDALGetRasterYSize( hDataset ) );

/* -------------------------------------------------------------------- */
/*      Report projection.                                              */
/* -------------------------------------------------------------------- */
    if( GDALGetProjectionRef( hDataset ) != NULL )
    {
        OGRSpatialReferenceH  hSRS;
        char		      *pszProjection;

        pszProjection = (char *) GDALGetProjectionRef( hDataset );

        hSRS = OSRNewSpatialReference(NULL);
        if( OSRImportFromWkt( hSRS, &pszProjection ) == CE_None )
        {
            char	*pszPrettyWkt = NULL;

            OSRExportToPrettyWkt( hSRS, &pszPrettyWkt, FALSE );
            printf( "Coordinate System is:\n%s\n", pszPrettyWkt );
            CPLFree( pszPrettyWkt );
        }
        else
            printf( "Coordinate System is `%s'\n",
                    GDALGetProjectionRef( hDataset ) );

        OSRDestroySpatialReference( hSRS );
    }

/* -------------------------------------------------------------------- */
/*      Report Geotransform.                                            */
/* -------------------------------------------------------------------- */
    if( GDALGetGeoTransform( hDataset, adfGeoTransform ) == CE_None )
    {
        if( adfGeoTransform[2] == 0.0 && adfGeoTransform[4] == 0.0 )
        {
            printf( "Origin = (%.15f,%.15f)\n",
                    adfGeoTransform[0], adfGeoTransform[3] );

            printf( "Pixel Size = (%.15f,%.15f)\n",
                    adfGeoTransform[1], adfGeoTransform[5] );
        }
        else
            printf( "GeoTransform =\n"
                    "  %.16g, %.16g, %.16g\n"
                    "  %.16g, %.16g, %.16g\n", 
                    adfGeoTransform[0],
                    adfGeoTransform[1],
                    adfGeoTransform[2],
                    adfGeoTransform[3],
                    adfGeoTransform[4],
                    adfGeoTransform[5] );
    }

/* -------------------------------------------------------------------- */
/*      Report GCPs.                                                    */
/* -------------------------------------------------------------------- */
    if( bShowGCPs && GDALGetGCPCount( hDataset ) > 0 )
    {
        if (GDALGetGCPProjection(hDataset) != NULL)
        {
            OGRSpatialReferenceH  hSRS;
            char		      *pszProjection;

            pszProjection = (char *) GDALGetGCPProjection( hDataset );

            hSRS = OSRNewSpatialReference(NULL);
            if( OSRImportFromWkt( hSRS, &pszProjection ) == CE_None )
            {
                char	*pszPrettyWkt = NULL;

                OSRExportToPrettyWkt( hSRS, &pszPrettyWkt, FALSE );
                printf( "GCP Projection = \n%s\n", pszPrettyWkt );
                CPLFree( pszPrettyWkt );
            }
            else
                printf( "GCP Projection = %s\n",
                        GDALGetGCPProjection( hDataset ) );

            OSRDestroySpatialReference( hSRS );
        }

        for( i = 0; i < GDALGetGCPCount(hDataset); i++ )
        {
            const GDAL_GCP	*psGCP;
            
            psGCP = GDALGetGCPs( hDataset ) + i;

            printf( "GCP[%3d]: Id=%s, Info=%s\n"
                    "          (%.15g,%.15g) -> (%.15g,%.15g,%.15g)\n", 
                    i, psGCP->pszId, psGCP->pszInfo, 
                    psGCP->dfGCPPixel, psGCP->dfGCPLine, 
                    psGCP->dfGCPX, psGCP->dfGCPY, psGCP->dfGCPZ );
        }
    }

/* -------------------------------------------------------------------- */
/*      Report metadata.                                                */
/* -------------------------------------------------------------------- */
    papszMetadata = (bShowMetadata) ? GDALGetMetadata( hDataset, NULL ) : NULL;
    if( bShowMetadata && CSLCount(papszMetadata) > 0 )
    {
        printf( "Metadata:\n" );
        for( i = 0; papszMetadata[i] != NULL; i++ )
        {
            printf( "  %s\n", papszMetadata[i] );
        }
    }

    for( iMDD = 0; bShowMetadata && iMDD < CSLCount(papszExtraMDDomains); iMDD++ )
    {
        papszMetadata = GDALGetMetadata( hDataset, papszExtraMDDomains[iMDD] );
        if( CSLCount(papszMetadata) > 0 )
        {
            printf( "Metadata (%s):\n", papszExtraMDDomains[iMDD]);
            for( i = 0; papszMetadata[i] != NULL; i++ )
            {
                printf( "  %s\n", papszMetadata[i] );
            }
        }
    }

/* -------------------------------------------------------------------- */
/*      Report "IMAGE_STRUCTURE" metadata.                              */
/* -------------------------------------------------------------------- */
    papszMetadata = (bShowMetadata) ? GDALGetMetadata( hDataset, "IMAGE_STRUCTURE" ) : NULL;
    if( bShowMetadata && CSLCount(papszMetadata) > 0 )
    {
        printf( "Image Structure Metadata:\n" );
        for( i = 0; papszMetadata[i] != NULL; i++ )
        {
            printf( "  %s\n", papszMetadata[i] );
        }
    }

/* -------------------------------------------------------------------- */
/*      Report subdatasets.                                             */
/* -------------------------------------------------------------------- */
    papszMetadata = GDALGetMetadata( hDataset, "SUBDATASETS" );
    if( CSLCount(papszMetadata) > 0 )
    {
        printf( "Subdatasets:\n" );
        for( i = 0; papszMetadata[i] != NULL; i++ )
        {
            printf( "  %s\n", papszMetadata[i] );
        }
    }

/* -------------------------------------------------------------------- */
/*      Report geolocation.                                             */
/* -------------------------------------------------------------------- */
    papszMetadata = (bShowMetadata) ? GDALGetMetadata( hDataset, "GEOLOCATION" ) : NULL;
    if( bShowMetadata && CSLCount(papszMetadata) > 0 )
    {
        printf( "Geolocation:\n" );
        for( i = 0; papszMetadata[i] != NULL; i++ )
        {
            printf( "  %s\n", papszMetadata[i] );
        }
    }

/* -------------------------------------------------------------------- */
/*      Report RPCs                                                     */
/* -------------------------------------------------------------------- */
    papszMetadata = (bShowMetadata) ? GDALGetMetadata( hDataset, "RPC" ) : NULL;
    if( bShowMetadata && CSLCount(papszMetadata) > 0 )
    {
        printf( "RPC Metadata:\n" );
        for( i = 0; papszMetadata[i] != NULL; i++ )
        {
            printf( "  %s\n", papszMetadata[i] );
        }
    }

/* -------------------------------------------------------------------- */
/*      Setup projected to lat/long transform if appropriate.           */
/* -------------------------------------------------------------------- */
    if( GDALGetGeoTransform( hDataset, adfGeoTransform ) == CE_None )
        pszProjection = GDALGetProjectionRef(hDataset);

    if( pszProjection != NULL && strlen(pszProjection) > 0 )
    {
        OGRSpatialReferenceH hProj, hLatLong = NULL;

        hProj = OSRNewSpatialReference( pszProjection );
        if( hProj != NULL )
            hLatLong = OSRCloneGeogCS( hProj );

        if( hLatLong != NULL )
        {
            CPLPushErrorHandler( CPLQuietErrorHandler );
            hTransform = OCTNewCoordinateTransformation( hProj, hLatLong );
            CPLPopErrorHandler();
            
            OSRDestroySpatialReference( hLatLong );
        }

        if( hProj != NULL )
            OSRDestroySpatialReference( hProj );
    }

/* -------------------------------------------------------------------- */
/*      Report corners.                                                 */
/* -------------------------------------------------------------------- */
    printf( "Corner Coordinates:\n" );
    GDALInfoReportCorner( hDataset, hTransform, "Upper Left", 
                          0.0, 0.0 );
    GDALInfoReportCorner( hDataset, hTransform, "Lower Left", 
                          0.0, GDALGetRasterYSize(hDataset));
    GDALInfoReportCorner( hDataset, hTransform, "Upper Right", 
                          GDALGetRasterXSize(hDataset), 0.0 );
    GDALInfoReportCorner( hDataset, hTransform, "Lower Right", 
                          GDALGetRasterXSize(hDataset), 
                          GDALGetRasterYSize(hDataset) );
    GDALInfoReportCorner( hDataset, hTransform, "Center", 
                          GDALGetRasterXSize(hDataset)/2.0, 
                          GDALGetRasterYSize(hDataset)/2.0 );

    if( hTransform != NULL )
    {
        OCTDestroyCoordinateTransformation( hTransform );
        hTransform = NULL;
    }
    
/* ==================================================================== */
/*      Loop over bands.                                                */
/* ==================================================================== */
    for( iBand = 0; iBand < GDALGetRasterCount( hDataset ); iBand++ )
    {
        double      dfMin, dfMax, adfCMinMax[2], dfNoData;
        int         bGotMin, bGotMax, bGotNodata, bSuccess;
        int         nBlockXSize, nBlockYSize, nMaskFlags;
        double      dfMean, dfStdDev;
        GDALColorTableH	hTable;
        CPLErr      eErr;

        hBand = GDALGetRasterBand( hDataset, iBand+1 );

        if( bSample )
        {
            float afSample[10000];
            int   nCount;

            nCount = GDALGetRandomRasterSample( hBand, 10000, afSample );
            printf( "Got %d samples.\n", nCount );
        }
        
        GDALGetBlockSize( hBand, &nBlockXSize, &nBlockYSize );
        printf( "Band %d Block=%dx%d Type=%s, ColorInterp=%s\n", iBand+1,
                nBlockXSize, nBlockYSize,
                GDALGetDataTypeName(
                    GDALGetRasterDataType(hBand)),
                GDALGetColorInterpretationName(
                    GDALGetRasterColorInterpretation(hBand)) );

        if( GDALGetDescription( hBand ) != NULL 
            && strlen(GDALGetDescription( hBand )) > 0 )
            printf( "  Description = %s\n", GDALGetDescription(hBand) );

        dfMin = GDALGetRasterMinimum( hBand, &bGotMin );
        dfMax = GDALGetRasterMaximum( hBand, &bGotMax );
        if( bGotMin || bGotMax || bComputeMinMax )
        {
            printf( "  " );
            if( bGotMin )
                printf( "Min=%.3f ", dfMin );
            if( bGotMax )
                printf( "Max=%.3f ", dfMax );
        
            if( bComputeMinMax )
            {
                CPLErrorReset();
                GDALComputeRasterMinMax( hBand, FALSE, adfCMinMax );
                if (CPLGetLastErrorType() == CE_None)
                {
                  printf( "  Computed Min/Max=%.3f,%.3f", 
                          adfCMinMax[0], adfCMinMax[1] );
                }
            }

            printf( "\n" );
        }

        eErr = GDALGetRasterStatistics( hBand, bApproxStats, bStats, 
                                        &dfMin, &dfMax, &dfMean, &dfStdDev );
        if( eErr == CE_None )
        {
            printf( "  Minimum=%.3f, Maximum=%.3f, Mean=%.3f, StdDev=%.3f\n",
                    dfMin, dfMax, dfMean, dfStdDev );
        }

        if( bReportHistograms )
        {
            int nBucketCount, *panHistogram = NULL;

            eErr = GDALGetDefaultHistogram( hBand, &dfMin, &dfMax, 
                                            &nBucketCount, &panHistogram, 
                                            TRUE, GDALTermProgress, NULL );
            if( eErr == CE_None )
            {
                int iBucket;

                printf( "  %d buckets from %g to %g:\n  ",
                        nBucketCount, dfMin, dfMax );
                for( iBucket = 0; iBucket < nBucketCount; iBucket++ )
                    printf( "%d ", panHistogram[iBucket] );
                printf( "\n" );
                CPLFree( panHistogram );
            }
        }

        if ( bComputeChecksum)
        {
            printf( "  Checksum=%d\n",
                    GDALChecksumImage(hBand, 0, 0,
                                      GDALGetRasterXSize(hDataset),
                                      GDALGetRasterYSize(hDataset)));
        }

        dfNoData = GDALGetRasterNoDataValue( hBand, &bGotNodata );
        if( bGotNodata )
        {
            printf( "  NoData Value=%.18g\n", dfNoData );
        }

        if( GDALGetOverviewCount(hBand) > 0 )
        {
            int		iOverview;

            printf( "  Overviews: " );
            for( iOverview = 0; 
                 iOverview < GDALGetOverviewCount(hBand);
                 iOverview++ )
            {
                GDALRasterBandH	hOverview;
                const char *pszResampling = NULL;

                if( iOverview != 0 )
                    printf( ", " );

                hOverview = GDALGetOverview( hBand, iOverview );
                printf( "%dx%d", 
                        GDALGetRasterBandXSize( hOverview ),
                        GDALGetRasterBandYSize( hOverview ) );

                pszResampling = 
                    GDALGetMetadataItem( hOverview, "RESAMPLING", "" );

                if( pszResampling != NULL 
                    && EQUALN(pszResampling,"AVERAGE_BIT2",12) )
                    printf( "*" );
            }
            printf( "\n" );

            if ( bComputeChecksum)
            {
                printf( "  Overviews checksum: " );
                for( iOverview = 0; 
                    iOverview < GDALGetOverviewCount(hBand);
                    iOverview++ )
                {
                    GDALRasterBandH	hOverview;

                    if( iOverview != 0 )
                        printf( ", " );

                    hOverview = GDALGetOverview( hBand, iOverview );
                    printf( "%d",
                            GDALChecksumImage(hOverview, 0, 0,
                                      GDALGetRasterBandXSize(hOverview),
                                      GDALGetRasterBandYSize(hOverview)));
                }
                printf( "\n" );
            }
        }

        if( GDALHasArbitraryOverviews( hBand ) )
        {
            printf( "  Overviews: arbitrary\n" );
        }
        
        nMaskFlags = GDALGetMaskFlags( hBand );
        if( (nMaskFlags & (GMF_NODATA|GMF_ALL_VALID)) == 0 )
        {
            GDALRasterBandH hMaskBand = GDALGetMaskBand(hBand) ;

            printf( "  Mask Flags: " );
            if( nMaskFlags & GMF_PER_DATASET )
                printf( "PER_DATASET " );
            if( nMaskFlags & GMF_ALPHA )
                printf( "ALPHA " );
            if( nMaskFlags & GMF_NODATA )
                printf( "NODATA " );
            if( nMaskFlags & GMF_ALL_VALID )
                printf( "ALL_VALID " );
            printf( "\n" );

            if( hMaskBand != NULL &&
                GDALGetOverviewCount(hMaskBand) > 0 )
            {
                int		iOverview;

                printf( "  Overviews of mask band: " );
                for( iOverview = 0; 
                     iOverview < GDALGetOverviewCount(hMaskBand);
                     iOverview++ )
                {
                    GDALRasterBandH	hOverview;

                    if( iOverview != 0 )
                        printf( ", " );

                    hOverview = GDALGetOverview( hMaskBand, iOverview );
                    printf( "%dx%d", 
                            GDALGetRasterBandXSize( hOverview ),
                            GDALGetRasterBandYSize( hOverview ) );
                }
                printf( "\n" );
            }
        }

        if( strlen(GDALGetRasterUnitType(hBand)) > 0 )
        {
            printf( "  Unit Type: %s\n", GDALGetRasterUnitType(hBand) );
        }

        if( GDALGetRasterCategoryNames(hBand) != NULL )
        {
            char **papszCategories = GDALGetRasterCategoryNames(hBand);
            int i;

            printf( "  Categories:\n" );
            for( i = 0; papszCategories[i] != NULL; i++ )
                printf( "    %3d: %s\n", i, papszCategories[i] );
        }

        if( GDALGetRasterScale( hBand, &bSuccess ) != 1.0 
            || GDALGetRasterOffset( hBand, &bSuccess ) != 0.0 )
            printf( "  Offset: %.15g,   Scale:%.15g\n",
                    GDALGetRasterOffset( hBand, &bSuccess ),
                    GDALGetRasterScale( hBand, &bSuccess ) );

        papszMetadata = (bShowMetadata) ? GDALGetMetadata( hBand, NULL ) : NULL;
        if( bShowMetadata && CSLCount(papszMetadata) > 0 )
        {
            printf( "  Metadata:\n" );
            for( i = 0; papszMetadata[i] != NULL; i++ )
            {
                printf( "    %s\n", papszMetadata[i] );
            }
        }

        papszMetadata = (bShowMetadata) ? GDALGetMetadata( hBand, "IMAGE_STRUCTURE" ) : NULL;
        if( bShowMetadata && CSLCount(papszMetadata) > 0 )
        {
            printf( "  Image Structure Metadata:\n" );
            for( i = 0; papszMetadata[i] != NULL; i++ )
            {
                printf( "    %s\n", papszMetadata[i] );
            }
        }

        if( GDALGetRasterColorInterpretation(hBand) == GCI_PaletteIndex 
            && (hTable = GDALGetRasterColorTable( hBand )) != NULL )
        {
            int			i;

            printf( "  Color Table (%s with %d entries)\n", 
                    GDALGetPaletteInterpretationName(
                        GDALGetPaletteInterpretation( hTable )), 
                    GDALGetColorEntryCount( hTable ) );

            if (bShowColorTable)
            {
                for( i = 0; i < GDALGetColorEntryCount( hTable ); i++ )
                {
                    GDALColorEntry	sEntry;
    
                    GDALGetColorEntryAsRGB( hTable, i, &sEntry );
                    printf( "  %3d: %d,%d,%d,%d\n", 
                            i, 
                            sEntry.c1,
                            sEntry.c2,
                            sEntry.c3,
                            sEntry.c4 );
                }
            }
        }

        if( bShowRAT && GDALGetDefaultRAT( hBand ) != NULL )
        {
            GDALRasterAttributeTableH hRAT = GDALGetDefaultRAT( hBand );
            
            GDALRATDumpReadable( hRAT, NULL );
        }
    }

    GDALClose( hDataset );
    
    CSLDestroy( papszExtraMDDomains );
    //CSLDestroy( argv );
    
    GDALDumpOpenDatasets( stderr );

    GDALDestroyDriverManager();

    CPLDumpSharedList( NULL );
    CPLCleanupTLS();

    return -1;
}

/************************************************************************/
/*                        GDALInfoReportCorner()                        */
/************************************************************************/

static int 
GDALInfoReportCorner( GDALDatasetH hDataset, 
                      OGRCoordinateTransformationH hTransform,
                      const char * corner_name,
                      double x, double y )

{
    double	dfGeoX, dfGeoY;
    double	adfGeoTransform[6];
        
    printf( "%-11s ", corner_name );
    
/* -------------------------------------------------------------------- */
/*      Transform the point into georeferenced coordinates.             */
/* -------------------------------------------------------------------- */
    if( GDALGetGeoTransform( hDataset, adfGeoTransform ) == CE_None )
    {
        dfGeoX = adfGeoTransform[0] + adfGeoTransform[1] * x
            + adfGeoTransform[2] * y;
        dfGeoY = adfGeoTransform[3] + adfGeoTransform[4] * x
            + adfGeoTransform[5] * y;
    }

    else
    {
        printf( "(%7.1f,%7.1f)\n", x, y );
        return FALSE;
    }

/* -------------------------------------------------------------------- */
/*      Report the georeferenced coordinates.                           */
/* -------------------------------------------------------------------- */
    if( ABS(dfGeoX) < 181 && ABS(dfGeoY) < 91 )
    {
        printf( "(%12.7f,%12.7f) ", dfGeoX, dfGeoY );

    }
    else
    {
        printf( "(%12.3f,%12.3f) ", dfGeoX, dfGeoY );
    }

/* -------------------------------------------------------------------- */
/*      Transform to latlong and report.                                */
/* -------------------------------------------------------------------- */
    if( hTransform != NULL 
        && OCTTransform(hTransform,1,&dfGeoX,&dfGeoY,NULL) )
    {
        
        printf( "(%s,", GDALDecToDMS( dfGeoX, "Long", 2 ) );
        printf( "%s)", GDALDecToDMS( dfGeoY, "Lat", 2 ) );
    }

    printf( "\n" );

    return TRUE;
}

void getColHeight(float hei, int * colred, int * colgreen, int * colblue, float hgrey, float HCutOff, float hmax)
{
	// california colors

    *colred=-1;*colgreen=-1;*colblue=-1;
	
    if (hei > 0)
	{
        if (hei > hgrey && hei < HCutOff)
		{
            *colred = 128;
            *colgreen = 128;
            *colblue = 128;
		}

        if ( hei >= HCutOff && hei < hmax )
		{
                *colred = 255;
                *colgreen = (int) ((hei - HCutOff) * 256);
                if (*colgreen < 0) colgreen = 0;
                *colblue = 0;
		}
        if (hei >= hmax)
		{
		        *colred = 248; *colgreen = 248; *colblue = 232;
                
		}
	}
	else
	{
        if (hei < -HCutOff)
		{
            *colgreen =(int) ( (-hei - HCutOff) / 1 * 256);
            *colblue = *colgreen;
            *colred = 0;
		}
        else
		
		if (hei < -hgrey)
		{
			*colred = 128;
			*colgreen = 128;
			*colblue = 0;
		}
		else
		{
			*colgreen = -1;
			*colblue = -1;
			*colred = -1;
		}	
		
	}
    
    if (*colblue  > 255)  *colblue = 255;
    if (*colgreen > 255)  *colgreen = 255;
    if (*colred   > 255)  *colred = 255;
//		if(*colred==-1)
//			*colred=*colred;

}

void getColDepth(float d, int * colred, int * colgreen, int * colblue)
{
	char FillColor1[]="255,255,255";
	*colred=-1;*colgreen=-1;*colblue=-1;
	float hei;
	if (d <= 0)
		{
			hei = -d;
			//' World Premium colors
			// montains
			if (hei > 8000 )	{*colred=130;*colgreen=77;*colblue=47;}
		if (hei >= 7000 && hei < 8000 )	{*colred=163;*colgreen=100;*colblue=60;}
		if (hei >= 6000 && hei < 7000 )	{*colred=176;*colgreen=117;*colblue=65;}
		if (hei >= 5000 && hei < 6000 )	{*colred=191;*colgreen=137;*colblue=86;}
		if (hei >= 4000 && hei < 5000 )	{*colred=199;*colgreen=150;*colblue=103;}
		if (hei >= 3000 && hei < 4000 )	{*colred=193;*colgreen=165;*colblue=126;}
		if (hei >= 2000 && hei < 3000 )	{*colred=206;*colgreen=185;*colblue=113;}
		if (hei >= 1000 && hei < 2000 )	{*colred=230;*colgreen=215;*colblue=142;}
		if (hei >= 500 && hei < 1000 )	{*colred=219;*colgreen=232;*colblue=155;}
		if (hei >= 200 && hei < 500 )	{*colred=199;*colgreen=227;*colblue=134;}
		if (hei >= 0 && hei < 200 )	{*colred=170;*colgreen=207;*colblue=108;}
		}		
	else			
		{		
			// oceans
		if (d > 0 && d <= 200 )	{*colred=222;*colgreen=251;*colblue=252;}
		if (d > 200 && d <= 500 )	{*colred=182;*colgreen=247;*colblue=250;}
		if (d > 500 && d <= 1000 )	{*colred=167;*colgreen=231;*colblue=250;}
		if (d > 1000 && d <= 2000 )	{*colred=154;*colgreen=213;*colblue=255;}
		if (d > 2000 && d <= 3000 )	{*colred=145;*colgreen=198;*colblue=255;}
		if (d > 3000 && d <= 4000 )	{*colred=128;*colgreen=183;*colblue=255;}
		if (d > 4000 && d <= 5000 )	{*colred=114;*colgreen=174;*colblue=255;}
		if (d > 5000 && d <= 6000 )	{*colred=100;*colgreen=160;*colblue=250;}
		if (d > 6000 && d <= 7000 )	{*colred=85;*colgreen=148;*colblue=242;}
		if (d > 7000 && d <= 8000 )	{*colred=79;*colgreen=130;*colblue=240;}
		if (d > 8000 && d <= 9000 )	{*colred=79;*colgreen=98;*colblue=240;}
		if (d > 9000 && d <= 10000 )	{*colred=50;*colgreen=81;*colblue=219;}
		if (d > 10000 )	{*colred=30;*colgreen=61;*colblue=199;}

		//if (false)
		//{
			float d1 ;
			float rr;
			d1= d;
			if (d1 > 8000 )  d1 = 8000;
			//bb= 23002 - d1 / 8000 * (23002 - 13002);
			//if (bb>255) bb=255;

			*colblue=255;
			rr= 216 - d1 / 8000 * (216 - 13);
			*colred =(int) rr;
			*colgreen = *colred;
	    //}
	   }
	//	if(*colred==-1)
		//	*colred=*colred;


}

void writeASCII(char * fname, float xmin, float xmax,float ymin,float ymax, int nXSize, int nYSize, int offsetIX, int offsetIY, double factor, float  h[(NROWS+1)][(NCOLS+1)])
{
	FILE * outfile;
	
	/* Open the file.  If NULL is returned there was an error */
	//printf("Printing locations in %s\n",fname);
	outfile = fopen(fname, "w");

	//
	fprintf(outfile,"ncols     %i\n" , nXSize);
	fprintf(outfile,"nrows     %i\n" , nYSize);
	fprintf(outfile,"xllcorner %f\n" , xmin);
	fprintf(outfile,"yllcorner %f\n" , ymin);
	fprintf(outfile,"cellsize  %f\n" , (xmax - xmin) / (nXSize - 1));
	fprintf(outfile, "nodata value -32768\n");
	//
	//
	int i,j;
	for(j = 1;j<=nYSize;j++)
	{
		for (i = 1+offsetIX;i<=nXSize+offsetIX;i++)
			fprintf(outfile,"%f ",h[j+offsetIY][i]*factor);
		fprintf(outfile,"\n");
	}
	fclose(outfile);
}

#ifdef WINDOWS


/**********************************************************************
 *                       CSLAddString()
 *
 * Append a string to a StringList and return a pointer to the modified
 * StringList.
 * If the input StringList is NULL, then a new StringList is created.
 * Note that CSLAddString performance when building a list is in O(n^2)
 * which can cause noticable slow down when n > 10000.
 **********************************************************************/

char **CSLAddString(char **papszStrList, const char *pszNewString)
{
    int nItems=0;

    if (pszNewString == NULL)
        return papszStrList;    /* Nothing to do!*/

    /* Allocate room for the new string */
    if (papszStrList == NULL)
        papszStrList = (char**) CPLCalloc(2,sizeof(char*));
    else
    {
        nItems = CSLCount(papszStrList);
        papszStrList = (char**)CPLRealloc(papszStrList, 
                                          (nItems+2)*sizeof(char*));
    }

    /* Copy the string in the list */
    papszStrList[nItems] = CPLStrdup(pszNewString);
    papszStrList[nItems+1] = NULL;

    return papszStrList;
}

/************************************************************************/
/*                              CSLCount()                              */
/************************************************************************/

/**
 * Return number of items in a string list.
 *
 * Returns the number of items in a string list, not counting the 
 * terminating NULL.  Passing in NULL is safe, and will result in a count
 * of zero.  
 *
 * Lists are counted by iterating through them so long lists will
 * take more time than short lists.  Care should be taken to avoid using
 * CSLCount() as an end condition for loops as it will result in O(n^2)
 * behavior. 
 *
 * @param papszStrList the string list to count.
 * 
 * @return the number of entries.
 */
int CSLCount(char **papszStrList)
{
    int nItems=0;

    if (papszStrList)
    {
        while(*papszStrList != NULL)
        {
            nItems++;
            papszStrList++;
        }
    }

    return nItems;
}
/************************************************************************/
/*                             CSLDestroy()                             */
/************************************************************************/

/**
 * Free string list.
 * 
 * Frees the passed string list (null terminated array of strings).
 * It is safe to pass NULL. 
 *
 * @param papszStrList the list to free.
 */
void CPL_STDCALL CSLDestroy(char **papszStrList)
{
    char **papszPtr;

    if (papszStrList)
    {
        papszPtr = papszStrList;
        while(*papszPtr != NULL)
        {
            CPLFree(*papszPtr);
            papszPtr++;
        }

        CPLFree(papszStrList);
    }
}


#endif
