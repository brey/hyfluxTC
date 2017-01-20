#include "gdal.h"
int   gdalInfo      (char * pszFilename);
void  gdalCreateFile(char * pszDest, char * pszFormat, float xmin, float xmax,float ymin,float ymax, int nXSize, int nYSize, int offsetIX, int offsetIY, double factor, float  h[(NROWS+1)][(NCOLS+1)], float d[(NROWS+1)][(NCOLS+1)], bool flipVertical, float hgray, float HCutOff, float hmax, int colorTransp);
int	  gdalReadFile  (char * pszSrc,int offsetIX, int offsetIY, double factor, float  h[(NROWS+1)][(NCOLS+1)], bool flipvertical);
float gdalGetValue  (float lon, float lat, char * pszSrc, int mode, GDALRasterBandH &dstband, GDALDatasetH  &hDstDS,  int &nXSize, int &nYSize, float &xmin, float &dxCell, float &ymax, float &dyCell, int &ierr);
void  gdalGetLine   (int ix0, int dx, int iy0, int dy, char * pszSrc, int mode, GDALRasterBandH  &dstband, int Iband, GDALDatasetH  &hDstDS,  int &nXSize, int &nYSize, float &xmin, float &dxCell, float &ymax, float &dyCell, int &ierr, double * scanline);
void  gdalGetParam  (char * pszSrc, float *xmin, float *xmax, float *ymin, float *ymax, float *dx, float *dy, int * nXSize, int *  nYSize);
#ifdef WINDOWS
	int CSLCount(char **papszStrList);
	void CPL_STDCALL CSLDestroy(char **papszStrList);
	char **CSLAddString(char **papszStrList, const char *pszNewString);
#endif