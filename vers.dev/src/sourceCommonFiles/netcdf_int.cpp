#include "netcdf_int.h"
#include "stringlib.h"

void logErr(int res, char * testo)
{
	if (res !=0)
	{
		printf("** %s: %i",testo,res);
		return;
	}

}
void NetCDF_Write(char * name, char * basefname, int openMode,double  TimeVal,int ncols, int nrows, double *lon_v, double *lat_v, float ha_v[(NROWS+1)][(NCOLS+1)], float factor )
{
	int res, DIMID_LON[1], VARID_LON=-1;
	int DIMID_LAT[1], VARID_LAT=-1;
	int DIMID_TIME[1], VARID_TIME=-1, VARID_HA=-1;
	int dimlen;
	double *vardouble;
	char * namevar, *nameAttr, *attrVal;	
	char * lonName =(char*) calloc(256,sizeof(char));
	char * latName =(char*) calloc(256,sizeof(char));
	char * timeName =(char*) calloc(256,sizeof(char));

	int *dimids,*ndims,*natts;
	//int  dimids [3],ndims[3], natts[3];
	//int  dimids2 [2],ndims2[2], natts2[2];
	char * nameV=(char*) calloc(256,sizeof(char));
	char * worldFileName = (char*) calloc(256,sizeof(char));
	size_t * dimlen1=(size_t *) calloc(256, sizeof(size_t));
	nc_type * xtype=(nc_type *) calloc(256,sizeof(nc_type));
	namevar=(char*) calloc(256,sizeof(char));
	attrVal=(char*) calloc(256,sizeof(char));
	nameAttr=(char*) calloc(256,sizeof(char));
	
	if (compString(basefname,"TIF_H_"))
	{
		copystring("HA",namevar);
		copystring("units",nameAttr);
		copystring("m",attrVal);
		copystring("LON",lonName);
		copystring("LAT",latName);
		copystring("TIME",timeName);
	}
	if (compString(basefname,"NOAA_Bat"))
	{
		copystring("bathy",namevar);
		copystring("units",nameAttr);
		copystring("m",attrVal);
		copystring("lon",lonName);
		copystring("lat",latName);
//		copystring("time",timeName);

	}
	if (compString(basefname,"NOAA_HA"))
	{
		copystring("ha",namevar);
		copystring("units",nameAttr);
		copystring("centimeters",attrVal);
		copystring("lon",lonName);
		copystring("lat",latName);
		copystring("time",timeName);

	}

	if (compString(basefname,"TIF_V_"))
	{
		copystring("V",namevar);
		copystring("units",nameAttr);
		copystring("m/s",attrVal);
		copystring("LON",lonName);
		copystring("LAT",latName);
		copystring("TIME",timeName);

	}
	if (compString(basefname,"TIF_U_"))
	{
		copystring("U",namevar);
		copystring("units",nameAttr);
		copystring("m/s",attrVal);
		copystring("LON",lonName);
		copystring("LAT",latName);
		copystring("TIME",timeName);
	}
	if (compString(basefname,"bathymetry"))
	{
		copystring("DE",namevar);
		copystring("units",nameAttr);
		copystring("m",attrVal);
		copystring("LON",lonName);
		copystring("LAT",latName);
		copystring("TIME",timeName);
	}

	int ncid;
        if (openMode == NC_WRITE)
		{

            res = nc_create(name, NC_CLOBBER | NC_64BIT_OFFSET, &ncid);
			logErr(res,"error in netcdf creating file");

            //Write some attributes.
			
			writeAttrib(ncid, "hystory", "SWAN 1.0 - JRC Ispra European Commission", NC_GLOBAL);

           CreateVar(ncid,lonName, NC_DOUBLE, lon_v,ncols, DIMID_LON, &VARID_LON,false, NULL,NULL);
			writeAttrib(ncid, "units", "degrees_east", VARID_LON);
			writeAttrib(ncid, "point_spacing","even", VARID_LON);

            CreateVar(ncid, latName, NC_DOUBLE, lat_v, nrows, DIMID_LAT, &VARID_LAT,false,NULL,NULL);
			writeAttrib(ncid, "units", "degrees_north", VARID_LAT);
			writeAttrib(ncid, "point_spacing","even", VARID_LAT);

			
			if ( !compString(basefname,"NOAA_Bat"))
			{	dimids=(int*) calloc(3,sizeof(int));
				natts=(int*) calloc(3,sizeof(int));
				ndims=(int*) calloc(3,sizeof(int));
				dimids[0]=DIMID_TIME[0];
				dimids[1]=DIMID_LAT[0];
				dimids[2]=DIMID_LON[0];
				CreateVar(ncid, timeName, NC_DOUBLE, lat_v,-1, DIMID_TIME, &VARID_TIME,true,NULL,NULL);
				writeAttrib(ncid, "units", "seconds", VARID_TIME);
				int dimids[3]={DIMID_TIME[0], DIMID_LAT[0], DIMID_LON[0]};
				CreateVar(ncid, namevar, NC_FLOAT,  NULL,NULL,NULL, &VARID_HA, false , dimids,3);
				writeAttrib(ncid, nameAttr, attrVal, VARID_HA);
			}
			else
			{
				dimids=(int*) calloc(2,sizeof(int));
				natts=(int*) calloc(2,sizeof(int));
				ndims=(int*) calloc(2,sizeof(int));
				
				dimids[0]=DIMID_LAT[0];
				dimids[1]=DIMID_LON[0];
				CreateVar(ncid, namevar, NC_FLOAT,  NULL,NULL,NULL, &VARID_HA, false , dimids,2);
				writeAttrib(ncid, nameAttr, attrVal, VARID_HA);
			}
            res = nc_enddef(ncid) ;
			logErr(res,"error defining vars");
			
            res = nc_put_var_double(ncid, VARID_LON, lon_v) ; logErr(res,"error putting lon_v");
            res = nc_put_var_double(ncid, VARID_LAT, lat_v) ; logErr(res,"error putting lat_v");
			vardouble=(double*) calloc(1,sizeof(double));
			vardouble[0]=TimeVal;
            dimlen = 0;
			
			// create world file
			FILE *outfile;
			float dxCell=lon_v[2]-lon_v[1];
			float dyCell=lat_v[1]-lat_v[2];
			outfile=fopen(replace_str(name,".nc",".nxc"),"w");
			fprintf(outfile,"%f\n",dxCell);
			fprintf(outfile,"0.\n0.\n");
			fprintf(outfile,"%f\n",dyCell);
			fprintf(outfile,"%f\n",lon_v[0]-dxCell/2+1.e-6);
			fprintf(outfile,"%f\n",lat_v[nrows-1]-dyCell/2);
			fclose(outfile);
		}
		else
		{
          res = nc_open(name, NC_WRITE | NC_64BIT_OFFSET, &ncid);
		  logErr(res,"opening file");
            // need to determine time value index
		    if ( !compString(basefname,"NOAA_Bat"))
            res = nc_inq_varid(ncid, timeName, &VARID_TIME) ;
			logErr(res,"error imquiring time");

			res = nc_inq_varid(ncid, namevar, &VARID_HA);logErr(res,"Error INQUIRING valid HA");
			if ( !compString(basefname,"NOAA_Bat"))
			{	dimids=(int*) calloc(3,sizeof(int));
				natts=(int*) calloc(3,sizeof(int));
				ndims=(int*) calloc(3,sizeof(int));
				dimids[0]=DIMID_TIME[0];
				dimids[1]=DIMID_LAT[0];
				dimids[2]=DIMID_LON[0];
			}
			else
			{
				dimids=(int*) calloc(2,sizeof(int));
				natts=(int*) calloc(2,sizeof(int));
				ndims=(int*) calloc(2,sizeof(int));
				dimids[0]=DIMID_LAT[0];
				dimids[1]=DIMID_LON[0];
			}
			
            res = nc_inq_var(ncid, VARID_TIME, nameV, xtype, ndims, dimids, natts);logErr(res,"error with inq_var");
            res = nc_inq_dimlen(ncid, dimids[0], dimlen1);
            dimlen= (int) dimlen1[0];
			double *vardoubleOld=(double *) calloc(dimlen,sizeof(double));
			
            res = nc_get_var_double(ncid, VARID_TIME, vardoubleOld);
            
			vardouble=(double *) calloc(dimlen+1,sizeof(double));
			int i;
			for (i=0;i<dimlen;i++)
				vardouble[i]=vardoubleOld[i];

			vardouble[dimlen] = TimeVal;
			delete [] vardoubleOld;
			vardoubleOld=NULL;
		}
		
		
		//int * ha_v1;
		float * ha_v1;
		//ha_v1=(int *) calloc(ncols*nrows,sizeof(float));
		ha_v1=(float *) calloc(ncols*nrows,sizeof(float));
		//Dim ha_v1((UBound(lon_v) + 1) * (UBound(lat_v) + 1) - 1) As Single
        int i,k,n,j;
		n = -1;
        for (j = 0;j<nrows;j++)
		{
            for (i = 0;i<ncols;i++)
			{
                n += 1;
                ha_v1[n] = ha_v[j+2][i+2]*factor;
				//printf("%i  %i  %f\n",i+2,j+2,ha_v1[n]);
			}
		}
        
		static size_t  * startp ,*countp;
		if ( !compString(basefname,"NOAA_Bat"))
		{
			startp= (size_t *) calloc(3,sizeof(size_t));
			countp= (size_t *) calloc(3,sizeof(size_t));
			startp[0] = dimlen;
			startp[1] = 0;
			startp[2] = 0;
			countp[0] = 1;
			countp[1] = nrows;
			countp[2] = ncols;
		}
		else
		{
			startp= (size_t *) calloc(2,sizeof(size_t));
			countp= (size_t *) calloc(2,sizeof(size_t));
			startp[0] = 0;
			startp[1] = 0;
			countp[0] = nrows;
			countp[1] = ncols;
		}
        res = nc_put_vara_float(ncid, VARID_HA, startp, countp, ha_v1);logErr(res,"netcdf error writing h");
		//res = nc_put_vara_int(ncid, VARID_HA, startp, countp, ha_v1);logErr(res,"netcdf error writing h");
       if ( !compString(basefname,"NOAA_Bat"))
	   {
		res = nc_put_var_double(ncid, VARID_TIME, vardouble);logErr(res,"netcdf error writing h");
	   }
		delete [] ha_v1,vardouble;
		ha_v1=NULL;vardouble=NULL;
		res = nc_close(ncid);
}

int readVarDouble(int ncid, int VARID, double *q, int * nval, char * nameV, int Mode)
{
	// Mode=0  only read dimensions
	// Mode=1  read
		int  dimids [3],ndims[3], natts[3];
		size_t * dimlen=(size_t *) calloc(256, sizeof(size_t));
		nc_type * xtype=(nc_type *) calloc(256,sizeof(nc_type));

		int res;
		res = nc_inq_var(ncid, VARID, nameV, xtype, ndims, dimids, natts);
		res = nc_inq_dimlen(ncid, dimids[0], dimlen);
		if (Mode==1)
		{
			res = nc_get_var_double(ncid, VARID, q);
		}
		*nval = (int) dimlen[0];
		return 0;

}
/*
int readVarFloat(int ncid, int VARID, float *q, int * nval, char * nameV)
{
		int  dimids [3],ndims[3], natts[3];
		size_t * dimlen=(size_t *) calloc(256, sizeof(size_t));
		nc_type * xtype=(nc_type *) calloc(256,sizeof(nc_type));
		int res;
		res = nc_inq_var(ncid, VARID, nameV, xtype, ndims, dimids, natts);
		res = nc_inq_dimlen(ncid, dimids[0], dimlen);
		q=(float *) calloc((int) dimlen, sizeof(float));
		res = nc_get_var_float(ncid, VARID, q);
		*nval = (int) dimlen[0];
		return 0;

}
*/
void NETCDF_ReadTime(char * fname,char * basefname, int Mode, double  * TimeVals, int * nval_time )
{
		int res, ncid;
		int VARID_TIME, VARID_LAT,VARID_LON,VARID_HA;
        res = nc_open(fname, NC_CLOBBER, &ncid);

		if(compString(basefname,"NOAA_Bat") || compString(basefname,"NOAA_HA"))
			res = nc_inq_varid(ncid, "time", &VARID_TIME);
		else
			res = nc_inq_varid(ncid, "TIME", &VARID_TIME);

        double *x1;
    	char * nameV=(char*)calloc(256,sizeof(char));
		x1=(double*)calloc(1,sizeof(double));
	    res = readVarDouble(ncid, VARID_TIME, x1, nval_time,nameV,0);
		if (Mode==1)
				res = readVarDouble(ncid, VARID_TIME, TimeVals, nval_time,nameV,1);
		nc_close(ncid);
}

void NETCDF_Read(char * fname, char * basefname, int Mode, int IDTime, double * x1, int * nval_time, int * ncols, int * nrows, double *lon_v, double *lat_v, float ha_v[(NROWS+1)][(NCOLS+1)],float factor )
{
		int res, ncid;
		int VARID_TIME, VARID_LAT,VARID_LON,VARID_HA;
        res = nc_open(fname, NC_CLOBBER, &ncid);
		if (res != 0)
			printf("error opening netcdf file %s\n",fname);
		if(compString(basefname,"NOAA_Bat") || compString(basefname,"NOAA_HA"))
		{
			
			res = nc_inq_varid(ncid, "lat", &VARID_LAT) ;
			res = nc_inq_varid(ncid, "lon", &VARID_LON) ;
			if(compString(basefname,"NOAA_Bat"))
				res = nc_inq_varid(ncid, "bathy", &VARID_HA) ;
			else
			{
				res = nc_inq_varid(ncid, "ha", &VARID_HA) ;
				res = nc_inq_varid(ncid, "time", &VARID_TIME);
			}
		}
		else
		{
			res = nc_inq_varid(ncid, "TIME", &VARID_TIME);
			res = nc_inq_varid(ncid, "LAT", &VARID_LAT) ;
			res = nc_inq_varid(ncid, "LON", &VARID_LON) ;
			res = nc_inq_varid(ncid, "HA", &VARID_HA) ;
		}
		double *y1;
		char * nameV=(char*)calloc(256,sizeof(char));
		//x1=(double*)calloc(1,sizeof(double));
		y1=(double*)calloc(1,sizeof(double));
		if(!(compString(basefname,"NOAA_Bat") ))
        res = readVarDouble(ncid, VARID_TIME, x1, nval_time,nameV,0);
		
        int nval1, nval2;
        res = readVarDouble(ncid, VARID_LAT, lat_v, nrows,nameV,0);
        res = readVarDouble(ncid, VARID_LON, lon_v, ncols,nameV,0);
		if (Mode!=0)
		{   int k;
			static size_t  startp [3],countp[3];
			//x1=(double *) calloc((int) &nval_time, sizeof(double));
			if (Mode==1)
			{
			if(!(compString(basefname,"NOAA_Bat") || compString(basefname,"NOAA_HA")))
			res = readVarDouble(ncid, VARID_TIME, x1, nval_time,nameV,1);
			res = readVarDouble(ncid, VARID_LAT, lat_v, nrows,nameV,1);
			res = readVarDouble(ncid, VARID_LON, lon_v, ncols,nameV,1);
			}
			if (IDTime>=0)
			{
				float * ha_v1;
				ha_v1=(float *) calloc(*ncols * *nrows,sizeof(float));
				if (compString(basefname,"NOAA_Bat"))
				{
				startp[0] = 0;
				startp[1] = 0;
				countp[0] = *nrows ;
				countp[1] = *ncols ;
				
				}
				else
				{
				startp[0] = IDTime;
				startp[1] = 0;
				startp[2] = 0;
				countp[0] = 1;
				countp[1] = *nrows ;
				countp[2] = *ncols ;
				}
				res = nc_get_vara_float(ncid, VARID_HA, startp, countp, ha_v1) ;
				int i,j,n=-1;
				for (j = 0;j<*nrows;j++)
				{
					for (i = 0;i<*ncols;i++)
					{
						n += 1;
						ha_v[j+2][i+2]=ha_v1[n]*factor;
						//printf("%i  %i  %f\n",i,j,ha_v1[n]);
					}
				}
				delete [] ha_v1;
				ha_v1=NULL;
			}
			else
				printf("Time not found");
		}
		
		nc_close(ncid);
		delete [] y1;y1=NULL;
}
void CreateVar(int ncid,char * NameVar, int typeVar, double * quantity, int size, int * dimID, int * VarID, bool  unlimited, int * dimids, int ndims)
{
	int res;
        //*****************************
        //Define dimensions
        //*****************************
        if (unlimited) {
            res = nc_def_dim(ncid, NameVar, NC_UNLIMITED, &dimID[0]);
            res = nc_def_var(ncid, NameVar, (nc_type) typeVar, 1, dimID, VarID);
		}   
        else
		{
            if (dimids==NULL)
			{
                // individual variable
				//int size=sizeof(quantity)/sizeof(quantity[0]);
                res = nc_def_dim(ncid, NameVar, size, &dimID[0]);
                res = nc_def_var(ncid, NameVar, (nc_type) typeVar, 1, dimID, VarID);
                // to compress
				int shuffle = 1;
                int deflate = 1;
                int deflate_level  = 4;
                // res = nc_def_var_deflate(ncid, &VarID, shuffle, deflate, deflate_level)
			}
			else
			{
				
                // dependent variable
				int size=ndims; //sizeof(dimids)/sizeof(dimids[0]);
                res = nc_def_var(ncid, NameVar, (nc_type) typeVar, size , dimids, VarID);
			}
			
      
		}
        if (res !=0)
		{
			printf("** error in netcdf create: %i",res);
			return;
		}
	
	}
    void writeAttrib(int ncid, char * nameAttr, char * attrVal, int varID)
	{
	   int k, res;
	   	res = nc_put_att_text(ncid, varID, nameAttr, sl(attrVal), attrVal);
		logErr(res,"error in netcdf creating attribute");
	}
    
void NETCDFgetLine(int ix0, int dx, int iy0, int dy, char * fname, char * nameV, int mode, int *ncid, int IDTime, int *VARID_TIME, int *VARID_LAT, int *VARID_LON, int *VARID_HA, int *nrows, int *ncols, float *xmin, float *dxCell, float *ymax, float *dyCell, int *ierr, float * scanline)
{   
	if (mode==0 || mode==3)
	{
		int res;
		//int VARID_TIME, VARID_LAT,VARID_LON,VARID_HA;
        res = nc_open(fname, NC_CLOBBER, ncid);
		if (res != 0)
			printf("error opening netcdf file %s\n",fname);
		int vt,vla,vlo,vha;
        res = nc_inq_varid(*ncid, "TIME", VARID_TIME);
        res = nc_inq_varid(*ncid, "LAT", VARID_LAT) ;
        res = nc_inq_varid(*ncid, "LON", VARID_LON) ;
        res = nc_inq_varid(*ncid, nameV, VARID_HA) ;

		double *lat_v, *lon_v;
		char * nameV=(char*)calloc(256,sizeof(char));
		lat_v=(double*) calloc(1,sizeof(double));
		lon_v=(double*) calloc(1,sizeof(double));
		res = readVarDouble(*ncid, *VARID_LAT, lat_v, nrows,nameV,0);
        res = readVarDouble(*ncid, *VARID_LON, lon_v, ncols,nameV,0);
		lat_v=(double*) calloc(*nrows,sizeof(double));
		lon_v=(double*) calloc(*ncols,sizeof(double));
		res = readVarDouble(*ncid, *VARID_LAT, lat_v, nrows,nameV,1);
        res = readVarDouble(*ncid, *VARID_LON, lon_v, ncols,nameV,1);
		
		*dxCell=lon_v[2]-lon_v[1];
		*dyCell=lat_v[1]-lat_v[2];
		*xmin=lon_v[0]-(*dxCell)/2;
		*ymax=lat_v[*nrows-1]-(*dyCell/2);
		delete [] lon_v,lat_v;
		lon_v=NULL;lat_v=NULL;
	}
	if (mode==1 || mode==3)
	{  
		//nXSize=GDALGetRasterXSize( hDstDS ); 
		//nYSize=GDALGetRasterYSize( hDstDS );

		if (ix0<0 || ix0>=*ncols || iy0<0 || iy0>=*nrows)
		{
			*ierr=-1;
		}
		ierr=0;
		static size_t  startp [3],countp[3];
		int res;
		//ret=GDALRasterIO( dstband, GF_Read, ix0, iy0, dx,dy, 
		//				  scanline, dx, dy, GDT_Float64, 0, 0 );
		startp[0] = IDTime;
		startp[1] = iy0;
		startp[2] = ix0; //0;
		countp[0] = 1;
		countp[1] = dy; //1 ;
		countp[2] = dx; //*ncols ;
		int ncid1,varh;
		ncid1=*ncid;varh=*VARID_HA;

		float * scanline1;
		scanline1=(float *) calloc(dx*dy,sizeof(float));
		res = nc_get_vara_float(ncid1, varh, startp, countp, scanline1) ;
		int n;
		for (n=0;n<dy*dx;n++) 	
		{
			scanline[n]=scanline1[n];
			//if(scanline[n]>1.)
			//	printf("%i   %f\n",n,scanline[n]);
		}
		delete [] scanline1;
		scanline1=NULL;
		n=1;
	}
	if (mode==2 || mode==3)
	{
		nc_close(*ncid);
		//printf("Closed file: %s\n",pszSrc);
		
		
	}
}

