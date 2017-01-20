#include "netcdf.h"
#include <stdio.h>
#include "stringlib.h"
#include <malloc.h>


void NetCDF_Write(char * name, char * basefname, int openMode,double  TimeVal,int ncols, int nrows, double *lon_v, double *lat_v, float ha_v[(NROWS+1)][(NCOLS+1)], float factor );
void writeAttrib(int ncid, char * nameAttr, char * attrVal, int varID);
void CreateVar(int ncid,char * NameVar, int typeVar, double * quantity,int size, int * dimID, int * VarID, bool  unlimited, int * dimids, int ndims);
void NETCDF_Read(char * fname, char * basefname, int Mode, int IDTime, double * timeValues,int *ntimes, int * ncols, int * nrows, double *lon_v, double *lat_v, float ha_v[(NROWS+1)][(NCOLS+1)],float factor );
void NETCDF_ReadTime(char * fname,  char * basefname,int Mode, double  * TimeVals, int * nval_time );
void NETCDFgetLine(int ix0, int dx, int iy0, int dy, char * fname, char * nameV, int mode, int *ncid, int IDTime, int *VARID_TIME, int *VARID_LAT, int *VARID_LON, int *VARID_HA, int *nrows, int *ncols, float *xmin, float *dxCell, float *ymax, float *dyCell, int *ierr, float * scanline);
