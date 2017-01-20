#include "definitions.h"
#include "gdal_int.h"
#include "util.h"
#include "stringlib.h"
#include "postProcessing.h"
#include "netcdf_int.h"

//long   getOkada0(long AddFlag);
tm * AddDate(tm * date0, int AddSeconds);
char * DateToString(tm  date0);
double *timeV,*lon_v,*lat_v;
char * code;
int Ntimes, IDtime;


tm StringaToDate(char *dateString);
//placedef * placeList;
//int * indexPlaceList;
placedef placeList[50000];
int indexPlaceList[50000];
placedef gaugeList[8000];
int indexGaugeList[8000];
placedef coastList[2000];
int indexCoastList[2000];

bool onlyCoastal=false;
bool onlyLocations=false;
int indexMax=-1;
int comCotLay=1;
bool nologo=false;
bool nolegend=false;
bool notimelabel=false;
// Argument variables
char * inputFile,*inputDir,*outputDir,*options,*batFile, *deformFile, *fnamePlaces, * fnameGauges;
float batFactor;
float NODATA=99999.;
// input variables
char Title[100], outPath[100], URL_calc[100], DateTsunamiString[32];
char * locFileName, *fname, *fname1;
float time1,time2;
tm DateTsunami;
double Lat,Lon;
float Mag,InTime,FinTime,Tsave,dtMax,fanning,watDep,cleanTT1;
int faultMode, faultform;
int compiler;
int AutomaticFault;

float flenght, fwidth, strike, slip, dip, rake, depth;
float width,batgrid,lonmin,lonmax,latmin,latmax;
float time0, *dta;

char * TemplateFiles, *digitTime;
int tstep,tend;
bool translateTif,commentsOn,doNotOverwriteMax;
char * GeometryAnimation;
char * CurrentDir, *configFile;
int CurrentFault;
int Nfaults;
sourcePar *faults;

inputCase CurrentInputCase;
int batMode;
float d[(NROWS+1)][(NCOLS+1)],  h[(NROWS+1)][(NCOLS+1)], u[(NROWS+1)][(NCOLS+1)], v[(NROWS+1)][(NCOLS+1)];
bool IsInDomainCalc(int ix, int iy,float h[(NROWS+1)][(NCOLS+1)]);
float mh[(NROWS+1)][(NCOLS+1)]; // max height
float h0[(NROWS+1)][(NCOLS+1)]; // initial height
float TarrH[(NROWS+1)][(NCOLS+1)]; // Arrival time (h)
float TarrMax[(NROWS+1)][(NCOLS+1)]; // Arrival time (h)
float ci[(NROWS+1)][(NCOLS+1)];

// options
// mode: self standing
bool  automscale; 

// TODO
//  WP e WT files
//  max height quando non c'e' il file relativo
//

// mode: post processing
bool  outTxt, outHTif, outuvTif,outBMP,outGIF,outJPG,checkWatchPoints,outkml,outGifAnimation,intermediateAnimation, inundation=false;
bool  tifH2netcdf,tifuv2netcdf,tarFiles, tarFilesAndDelete;
bool  outTTT,deleteTIF_atEnd=false, deleteGIF_atEnd=false;

bool writeWPtxtFiles,deleteAnimFiles;
float ddMaxWP = 5;   // distance in km to check WP
int iiMaxWP = 10;  // distance in km to check WP
float minDepthWP= 20;  // minimum depth to evaluate the WP max height
float minDepthTT= 10;  // minimum depth to evaluate the arrival time


char * p[60];
char *bathymetry;
char *inFormat, *outFormat;
int  ipTot,  jpTot;
int  ip2,  jp2,  * indexLimits;
float dx0,dy0;
float NewTPrint;
float NewMajorPrint;
float dxCell,   dyCell;
float hgrey = (float) 0.05;
float HCutOff = (float) 0.1;
float hmax = 3;
char * fnamenetcdf;
char * outputFile  ;
float minHeightTT;
		
#ifdef LINUX
	char * barra="/";
#endif
#ifdef WINDOWS
	char * barra="\\";
#endif


// to do : TTT support
//         creating a lits of locations from our locations
//
int main(int argc, char **argv)

{
	char * fname, * value;
	// calling convention
	//    postProcessing  -input inputFile -b bathymetryFile -dir folderToRead  -inFormat  [GeoTiff|xyfile|PCRaster|AAIgrid]

//gdalInfo("D:\testHF\tmp_hyflux\LatLon\tmp.tif");
	//placeList=(placedef*)  malloc(10000 * sizeof(placedef));
	//indexPlaceList=(int*) malloc(10000 * sizeof(int));
	int ncols, nrows;

	placedef *temp;
	temp=(placedef *) malloc(120 * sizeof(placedef));
	
	int nplaces,ngauges;
	int nXSize,nYSize;
	int ret;
    char * fnameDummy =(char *) malloc (256*sizeof(char));
	time1=-1e6;time2=1e20;	
	batFactor=1.;
	GeometryAnimation = (char *) calloc(512,sizeof(char));
	outputFile = (char *) calloc(512,sizeof(char));
	fname      = (char *) calloc(512,sizeof(char));
	fname1     = (char *) calloc(512,sizeof(char));
	code       = (char *) calloc(20 ,sizeof(char));
	options    = (char *) calloc(512,sizeof(char));
	inputFile  = (char *) calloc(512,sizeof(char));
	outputDir  = (char *) calloc(512,sizeof(char));
	batFile    = (char *) calloc(512,sizeof(char));
	CurrentDir = (char *) calloc(512,sizeof(char));
	inputDir   = (char *) calloc(512,sizeof(char));
	
	value      = (char *) calloc (256,sizeof(char));
	inFormat     = (char *) calloc(512,sizeof(char));
	outFormat    = (char *) calloc(512,sizeof(char));
	configFile   = (char *) calloc(512,sizeof(char));
	TemplateFiles = (char *) calloc(512,sizeof(char));
	digitTime    = (char *) calloc(512,sizeof(char));
	locFileName  = (char *) calloc(512,sizeof(char));
	faults      =(sourcePar*) calloc(100,sizeof(sourcePar));
	//minRatioTT=1.e-6;
	minHeightTT=0.05;
	cleanTT1=-1.;  // this is the ratio between velocity of propagation and sonic velocity. If -1 no cleaning is performed

	copystring("6",digitTime);
	translateTif= false;
	outTTT=true;
	copystring ("SWAN",code);
	/*barra =(char*) calloc(10,sizeof(char));
	#ifdef LINUX
		copystring("/",barra);
	#endif
	#ifdef WINDOWS
		copystring("\\",barra);
	#endif
	*/
	 int n;
	for (n=0;n<60;n++)
	  p[n]=(char *)calloc(512,sizeof(char));

	sprintf(inputFile, "calc_input_deck.txt");
	sprintf(batFile,   "bathymetry.tif");
	sprintf(inFormat,  "GeoTiff");

	CurrentDir=CurrentFolder(argv);
	readArguments(argc, argv,true);
	if (sl(configFile)==0)
	{
		AddString_s(configFile,CurrentDir,barra,"config.txt",NULL,NULL);
	}
	readFile(configFile,"config");
	
	
	readArguments(argc, argv, (bool) false);
	
	/*if (minHeightTT==-1.) 
		minHeightTT=1.e-4;*/
	/*
	if (minHeightTT>0.05)
		minHeightTT=minRatioTT;
	*/
	//printf("========== minRatioTT %f\n",minRatioTT);
	if (inundation) minDepthWP = -1000 ;  // in case of inundation=true max height is evaluated everywhere (also in the land)
	if (onlyLocations) minDepthTT=-1000;
	
	printf("==inundation=%i minDepthWP=%f    ======== minHeightTT %f\n",inundation,minDepthWP, minHeightTT);
	
	if (!compString(inputFile,"NULL"))
	{
		readFile(inputFile,"input");
		ncols =(int) (0.5 + (lonmax - lonmin) / (batgrid/60));
		nrows =(int) (0.5 + (latmax - latmin) / (batgrid/60));
		dxCell=batgrid/60;
	}
	else
	{
		gdalGetParam(batFile,&lonmin,&lonmax,&latmin,&latmax,&dxCell,&dyCell,&ncols,&nrows);
		batgrid=dxCell*60;
		Lon=(lonmax+lonmin)/2;
		Lat=(latmax+latmin)/2;
		if (dxCell != dyCell)
			printf("****** dxCell is not equal to dyCell");
	}

	if (sl(locFileName)>0)  // in the case I want to create names different from usual ones
	{
		copyFile(outputDir,"locations.txt",outputDir,"locations_A.txt",barra);
		copyFile(outputDir,"locations.kml",outputDir,"locations_A.kml",barra);
		copyFile(outputDir,"locations.xml",outputDir,"locations_A.xml",barra);
	}

	checkFolder(CurrentDir);
	
	fnamePlaces= (char *) calloc(512,sizeof(char));
	fnameGauges= (char *) calloc(512,sizeof(char));

	fnameDummy = (char *) calloc(512,sizeof(char));
	char * fnameDummy1=(char*) calloc(512,sizeof(char));
	AddString_s(fnameDummy,CurrentDir,barra,"..",barra,"DATA");
	AddString_s(fnameDummy1,fnameDummy,barra,"places.csv",NULL,NULL);
	int nc;
	solveDir(fnameDummy1,barra,nc,"..",CurrentDir,p,fnamePlaces);
	
	AddString_s(fnameDummy,CurrentDir,barra,"..",barra,"DATA");  // DATA is in the upper directory
	AddString_s(fnameDummy1,fnameDummy,barra,"gauges.csv",NULL,NULL);
	solveDir(fnameDummy1,barra,nc,"..",CurrentDir,p,fnameGauges);

	checkFolder(inputDir);
	checkFolder(outputDir);

	if (sl(TemplateFiles)>0)
	{
		// create list files from template
		//AddString_s(TemplateFiles,outputDir,barra,TemplateFiles,NULL,NULL);
		getFilename("listfiles","txt",-1,inputDir,fname);
		createListfilesFromTemplate(CurrentDir,inputDir,TemplateFiles, digitTime,tstep,tend,translateTif,fname,barra,p);
	}


	//*******************************
	//    j=>>>y     i=>>>> x
	//    h[j] [i]   =>>>  h[y][x]
	//*******************************
	
	ipTot=NCOLS;jpTot=NROWS;

    printf("informat=%s   outformat=%s\n",inFormat,outFormat);	
	printf("Reading bathymetry file %s\n",batFile);
	if (compString(inFormat,"xyfile"))
		readGridFile(batFile,d,ip2,jp2);
	else
	{

		if (compString(inFormat,"netcdf") || compString(inFormat,"netcdf_NOAA"))
		{
			copystring("bathymetry.nc",batFile);
			if (strstr(options,"rel")!=NULL)
				AddString_s(batFile,outputDir,barra,batFile,NULL,NULL);
	
			lon_v=(double*) calloc(ncols,sizeof(double));
			lat_v=(double*) calloc(nrows,sizeof(double));
			if (compString(inFormat,"netcdf_NOAA"))
				NETCDF_Read(batFile, "NOAA_Bat",0, 0,timeV,&Ntimes, &ncols,&nrows, lon_v, lat_v, d,-1.);
			else
				NETCDF_Read(batFile, "TIF_H",0, 0,timeV,&Ntimes, &ncols,&nrows, lon_v, lat_v, d,-1.);

			timeV=(double*) calloc(Ntimes,sizeof(double));
 			lon_v=(double*) calloc(ncols,sizeof(double));
            lat_v=(double*) calloc(nrows,sizeof(double));
			if (compString(inFormat,"netcdf_NOAA"))
				NETCDF_Read(batFile, "NOAA_Bat",1, 0,timeV,&Ntimes, &ncols,&nrows, lon_v, lat_v, d,1.);
			else
				NETCDF_Read(batFile,"TIF_H", 1, 0,timeV,&Ntimes, &ncols,&nrows, lon_v, lat_v, d,-1.);
			char * fnameTif=(char *) calloc(256, sizeof(char));
			getFilename("bathymetry","tif",-1,outputDir, fnameTif);
			gdalCreateFile(fnameTif,"GTiff",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,-1.,d,d,true,hgrey,HCutOff,hmax,-1);
		}
		else
		{
			nXSize=0;nYSize=0;
			ret=gdalReadFile(batFile,1,1,batFactor,d,true);
		}
	}
	ip2 = ncols + 2;
	jp2 = nrows + 2;

	dx0 = (lonmax - lonmin) / ncols;
	dy0 = (latmax - latmin) / nrows;

	if (compString(code,"COMCOT"))
	ConvertComcotFiles(outputDir, ncols, nrows, batgrid, lonmin, latmin, dtMax, Tsave,FinTime,comCotLay);

	if (!onlyCoastal) 
	{
	iiMaxWP=(int) (CalculateDegree(ddMaxWP,(float) Lat)/dx0);
	if (iiMaxWP<2) iiMaxWP=2;
	if (iiMaxWP>100) iiMaxWP=100;

	if (ip2 > NCOLS || jp2 > NROWS){
		printf("Too large size: ip2= %d jp2=%d", ip2,jp2);  
		return 0;}

	int ix,iy;
	getID((float) Lon,(float) Lat,lonmin,dx0,latmin,dy0,&ix,&iy);	
	if(iy>0 && ix>0 && iy<nrows && ix<ncols)
	{
		watDep=d[iy][ix];
		printf("watDept=%f\n",watDep);
	}
	
	lon_v=(double*) calloc(ncols,sizeof(double));
	lat_v=(double*) calloc(nrows,sizeof(double));
	initLatLon(dxCell,dxCell,ncols,nrows,lon_v,lat_v);

	CurrentInputCase=assignInput();
	//1. initialize time
	time0=InTime*3600;
	
	
	// here open listfileTif.txt and read the results 
	
	char * fnameTif;//=(char *) malloc (256*sizeof(char));
	//char * dummy=(char *) malloc (256*sizeof(char));
	//AddString_s(dummy,outputDir,barra,NULL,NULL,NULL);
	if (writeWPtxtFiles)
	{
		deleteFiles(outputDir,"WT_*.txt");
		deleteFiles(outputDir,"WG_*.txt");
		
	}
	deleteFiles(outputDir,"maxValues.txt");	
	
	if (outGifAnimation) deleteFiles(outputDir,"outres1.gif");
	//printf("tifH2netcdf:       %s\n",(tifH2netcdf)?"true":"false");
	printf("outputDir=%s\n",outputDir);
	if (!compString(inFormat,"netcdf") && tifH2netcdf) 
	{ printf("deleting");
	deleteFiles(outputDir,"NETCDF_*.*");
	}
	getFilename("listfiles","txt",-1,inputDir,fname);
	if (compString(inFormat,"netcdf") || compString(inFormat,"netcdf_NOAA"))
	{   //if input is from NETcdf I cannot write also Netcdf
		tifH2netcdf=false;tifuv2netcdf=false;
		char *fnamenetcdf_H=(char *) calloc(512,sizeof(char));
		char *fnamenetcdf_U=(char *) calloc(512,sizeof(char));
		char *fnamenetcdf_V=(char *) calloc(512,sizeof(char));
		if (compString(inFormat,"netcdf"))
		{
			AddString_s(fnamenetcdf_H,inputDir,barra,"NETCDF_H.nc",NULL,NULL);
			AddString_s(fnamenetcdf_U,inputDir,barra,"NETCDF_U.nc",NULL,NULL);
			AddString_s(fnamenetcdf_V,inputDir,barra,"NETCDF_V.nc",NULL,NULL);
			createListfiles("TIF_H",fnamenetcdf_H, fnamenetcdf_U, fnamenetcdf_V,fname);
			NETCDF_Read(fnamenetcdf_H,"TIF_H",0,-1,timeV,&Ntimes,&ncols,&nrows,lon_v,lat_v,h,1);
		}
		else
		{
			AddString_s(fnamenetcdf_H,inputDir,barra,"A_runup_ha.nc",NULL,NULL);
			AddString_s(fnamenetcdf_U,inputDir,barra,"A_runup_ua.nc",NULL,NULL);
			AddString_s(fnamenetcdf_V,inputDir,barra,"A_runup_va.nc",NULL,NULL);
			createListfiles("NOAA_HA",fnamenetcdf_H, fnamenetcdf_U, fnamenetcdf_V,fname);
			NETCDF_Read(fnamenetcdf_H,"NOAA_HA",0,-1,timeV,&Ntimes,&ncols,&nrows,lon_v,lat_v,h,1);
		}
		getFilename("listfiles","txt",-1,inputDir,fname);
		timeV=(double*) calloc(Ntimes,sizeof(double));
		IDtime=-1;
	}
	if(outGIF) deleteFiles(outputDir,"P1_*.gif");
	if(outBMP) deleteFiles(outputDir,"P1_*.bmp");
	if(outHTif) deleteFiles(outputDir,"TIF_H*.tif");
	
	
	
	FILE *infile;infile = fopen(fname, "r");
	char line[400];
	int nn=-1;
	char delimiter[2]=" ";
	int colt,colH, colVx, colVy;
	float maxx,maxa,maxd;
	bool firstTime=true;
	
	setInitialHeightTarr(h,h0,mh,TarrH,TarrMax,ip2,  jp2,NODATA);	
	
	colt=0;colH=1;colVx=2;colVy=3;
	// the first line should contain the header
	//  #    t     zsurf      vx     vy
	//
	while( fgets(line, sizeof(line), infile) != NULL) 
	//if (false)
	{
		if (line[0]=='#')  // read header
			getIndexOfvars(line,delimiter,colt,colH,colVx,colVy);
		if (line[0]!='*' && line[0]!='#')
		{
			getValueFromLine(line,delimiter,colt,value);
			time0=(float) atof(value); // ctrim(strtok(line," ")));
			if(time0>=time1 && time0<=time2)
			{
				// read results

				if (compString(inFormat,"netcdf") || compString(inFormat,"netcdf_NOAA"))
				{
					IDtime+=1;
					fnamenetcdf=(char *) calloc(512,sizeof(char));
					getValueFromLine(line,delimiter,colH,fnamenetcdf);
					//AddString_s(fnamenetcdf,inputDir,barra,"NETCDF_H.nc",NULL,NULL);
					if ( compString(inFormat,"netcdf_NOAA"))
						NETCDF_Read(fnamenetcdf,"NOAA_HA",2,IDtime,timeV,&Ntimes,&ncols,&nrows,lon_v,lat_v,h,0.01);
					else
						NETCDF_Read(fnamenetcdf,"TIF_H",2,IDtime,timeV,&Ntimes,&ncols,&nrows,lon_v,lat_v,h,1.);
				}
				else
				{
					fnameTif=(char *) calloc(512,sizeof(char));
					getValueFromLine(line,delimiter,colH,fnameDummy);
					AddString_s(fnameTif,inputDir,barra,fnameDummy,NULL,NULL);
					ret=gdalReadFile(fnameTif,1,1,1.0,h,true);
				}
				if(firstTime) 
				{
					//setInitialHeightTarr(h,h0,mh,TarrH,TarrMax,ip2,  jp2,NODATA);	
					
					if (fnamePlaces!="")
						{
							nplaces=selectPlaces(fnamePlaces,temp,lonmin,lonmax,latmin,latmax,true);
							selectPlaces(fnamePlaces,placeList,lonmin,lonmax,latmin,latmax,false);		
						}

						if (fnameGauges!="")
						{
							ngauges=selectGauges(fnameGauges,temp,lonmin,lonmax,latmin,latmax,true);
					//		placedef * placeList = new placedef[nplaces];
							selectGauges(fnameGauges,gaugeList,lonmin,lonmax,latmin,latmax,false);
						}
				}
				
				establishArrivalTime(time0,h,h0,TarrH,d, minDepthTT, minHeightTT,  ip2,  jp2, NODATA);
				establishMaxHeigth(time0,  h,mh,TarrMax,d, minDepthWP, minHeightTT, ip2, jp2, NODATA);
				//printf(" entro in results \n");
				results(time0, outputDir, nplaces,ngauges,ncols,nrows,0,firstTime);
				//printf(" esco da results \n");

				firstTime=false;
				maxx=getMaximum(d,mh,ip2,jp2,0);
			
				maxa=getMaximum(d,h,ip2,jp2,0);
				maxd=getMaximum(d,h,ip2,jp2,1);
				printf("time= %f maximum  h= %f   current max h=%f  current max hd=%f\n",time0,maxx,maxa,maxd);	
				updateMaxHeight( barra, time0, maxx, maxa, maxd, outputDir);
				if (intermediateAnimation)
				{ 
					nn++;
					if (nn>=20)
					{
						nn=0;
						printf("exporting rss,kml file\n");
						results(time0, outputDir, nplaces,ngauges,ncols,nrows,1,firstTime);
				
					}
				}
			}
		}
	}
	fclose(infile);
	results(time0, outputDir, nplaces,ngauges,ncols,nrows,0,false);
	results(time0, outputDir, nplaces,ngauges,ncols,nrows,1,false);
	results(time0, outputDir, nplaces,ngauges,ncols,nrows,2,false);
	if (sl(locFileName)>0)
	{
		AddString_s(outputFile,locFileName,".txt",NULL,NULL,NULL);
		copyFile(outputDir,"locations.txt",outputDir,outputFile,barra);
		deleteFiles(outputDir,"locations.txt");

		AddString_s(outputFile,locFileName,".xml",NULL,NULL,NULL);
		copyFile(outputDir,"locations.xml",outputDir,outputFile,barra);
		deleteFiles(outputDir,"locations.xml");

		AddString_s(outputFile,locFileName,".kml",NULL,NULL,NULL);
		copyFile(outputDir,"locations.kml",outputDir,outputFile,barra);
		deleteFiles(outputDir,"locations.kml");

		// here I copy back the names to the original ones
		copyFile(outputDir,"locations_A.txt",outputDir,"locations.txt",barra);
		copyFile(outputDir,"locations_A.kml",outputDir,"locations.kml",barra);
		copyFile(outputDir,"locations_A.xml",outputDir,"locations.xml",barra);
		deleteFiles(outputDir,"locations_A.kml");
		deleteFiles(outputDir,"locations_A.xml");
		deleteFiles(outputDir,"locations_A.txt");

	}
	if(deleteTIF_atEnd) deleteFiles(outputDir,"TIF_H*.tif");
	if(deleteGIF_atEnd) deleteFiles(outputDir,"P1_0*.gif");

	if (tarFiles || tarFilesAndDelete)
	{
		tar(CurrentDir,barra,p,outputDir,"WG_*.txt", "WGFiles.tar");
		tar(CurrentDir,barra,p,outputDir,"WT_*.txt", "WTFiles.tar");
		if (tarFilesAndDelete)
		{
			deleteFiles(outputDir,"WT_*.txt");
			deleteFiles(outputDir,"WG_*.txt");
		}
	}
	if (outTTT && !doNotOverwriteMax){
		
		getFilename("TIF_Arrival_Time_notCleaned","tif",-1,outputDir, fname);
		gdalCreateFile(fname,"GTiff",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,TarrH,d,true,hgrey,HCutOff,hmax,-1);

		/*printf("Cleaning Arrival time minHeight x 10= %f\n",minHeightTT*10);
		cleanValuesTT(TarrH,mh,d,minHeightTT*10,ncols,nrows);
		*/
		// removes TTT on land
		//filterValues(TarrH,d,0,1,ncols,nrows);

		if (cleanTT1>-1) cleanGF(TarrH,dx0, dy0, d, minDepthTT,cleanTT1,ip2,jp2,NODATA);
		getFilename("TIF_Arrival_Time","tif",-1,outputDir, fname);
		gdalCreateFile(fname,"GTiff",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,TarrH,d,true,hgrey,HCutOff,hmax,-1);
			
		getFilename("TIF_Arrival_Time","tif",-1,outputDir, fname);
		AddString_s(fname1,outputDir,barra,"ArrTime_5min",NULL,NULL);
		gdalcontour(CurrentDir,barra,p,fname,fname1,"TarrInt5min",float(5./60.));

		AddString_s(fname1,outputDir,barra,"ArrTime_15min",NULL,NULL);
		gdalcontour(CurrentDir,barra,p,fname,fname1,"TarrInt15min",float(15./60.));
		
		AddString_s(fname1,outputDir,barra,"ArrTime_60min",NULL,NULL);
		gdalcontour(CurrentDir,barra,p,fname,fname1,"TarrInt60min",1.);

		getFilename("ArrTime_5min/Tarrival","shp",-1,outputDir, fname);
		getFilename("Tarrival_5min","kml",-1,outputDir, fname1);
		shape2kml(CurrentDir, barra, p, fname,fname1);

		getFilename("ArrTime_15min/Tarrival","shp",-1,outputDir, fname);
		getFilename("Tarrival_15min","kml",-1,outputDir, fname1);
		shape2kml(CurrentDir, barra, p, fname,fname1);

		getFilename("ArrTime_60min/Tarrival","shp",-1,outputDir, fname);
		getFilename("Tarrival_60min","kml",-1,outputDir, fname1);
		shape2kml(CurrentDir, barra, p, fname,fname1);
	}
//#ifdef WINDOWS
//	if (outTTT)
		//CalculateTravelTime(Lon, Lat, Mag, lonmin,lonmax,latmin,latmax,outputDir,barra,CurrentDir,p);
//#endif
	}
	//computeCoastal(outputDir,lonmin,lonmax,latmin,latmax,ncols,nrows);

	getFilename("end-of-stuff","txt",-1,outputDir,fnameDummy);
	FILE *outfile;
	outfile = fopen(fnameDummy, "w");
	fprintf(outfile,"End of stuff");
	fclose(outfile);
	return 0;
}

//void getParameters(char * line,char * delimiter,char * p[60])
//{   char * label;
//	int n=-1;
//	label=strtok(line,delimiter);
//	while (label!=NULL)
//	{
//		n++;
//		label=cleanValue1(label);
//		copystring(label,p[n]);
//		p[n][sl(label)]=0;
//		label=strtok(NULL,delimiter);
//	}
//
//}


void results(float time0, char * outputDir, int nplaces,int ngauges, int ncols, int nrows, int mode, bool firstTime)
{
	char *fname, *fnameTif, * fname1;
	int i;
	fname= (char *) calloc(256,sizeof(char));
	fname1  = (char *) calloc(256,sizeof(char));
    fnameTif= (char *) calloc(256,sizeof(char));
	/*if (mode==-1)
	{
		if (outHTif){getFilename("listfiles","txt",-1,outputDir,fname);
		FILE *outfile;outfile = fopen(fname, "w");
		fprintf(outfile,"*list of files for current case %s\n",fname);
		fclose(outfile);}	
	}*/
	if (mode==0) // usual actions
	{
		if (outTxt)	dumpResults(time0, outputDir);
		if (outHTif && !onlyLocations ){
			getFilename("TIF_H_","tif",time0,outputDir, fnameTif);
			gdalCreateFile(fnameTif,"GTiff",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,h,d,true, hgrey,  HCutOff, hmax,-1);
			getFilename("listfilesTIF","txt",-1,outputDir,fname);
			FILE *outfile;outfile = fopen(fname, "a");
			fprintf(outfile,"%f %s\n",time0, fnameTif);
			fclose(outfile);

		}
		/*if (outuvTif){
			getFilename("TIF_U_","tif",time0,outputDir, fname);
			gdalCreateFile(fname,"GTiff",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,u,d,true);
			getFilename("TIF_V_","tif",time0,outputDir, fname);
			gdalCreateFile(fname,"GTiff",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,v,d,true);
		}*/
		if (outBMP  && !onlyLocations ){
			getFilename("P1_","bmp",time0,outputDir, fname);
			gdalCreateFile(fname,"BMP",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,h,d,true, hgrey,  HCutOff, hmax,-1);
			//getFilename("P1_","txt",time0,outputDir, fname);
			//gdalCreateFile(fname,"AAIGrid",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,h,d,true);
		}
		if (outJPG  && !onlyLocations ){

			getFilename("P1_","bmp",time0,outputDir, fname);
			gdalCreateFile(fname,"BMP",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,h,d,true, hgrey,  HCutOff, hmax,-1);
			
			getFilename("P1_","jpg",time0,outputDir, fname1);
			convertImage(barra,CurrentDir,p,fname,fname1,true,NULL);
			ApplyLogo(barra,CurrentDir,p, fname1);
			ApplyLegend(barra,CurrentDir,p, fname1);
			ApplyTimeLabel(barra,CurrentDir,outputDir,p, time0,fname1);
			getFilename("P1_","bmp.aux.xml",time0,outputDir, fname1);
			remove(fname1);
/*
// to create all transparent images, to be used for open day GLOBE
				if (compString(code,"HYFLUX") && inundation ) 
					{
					printf("1\n");
					gdalCreateFile(fname,"BMP",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,mh,d,true,hgrey,HCutOff,hmax,1128);
					}
				else 	
					gdalCreateFile(fname,"BMP",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,mh,d,true,hgrey,HCutOff,hmax,128);

  			       getFilename("P1_","png",time0,outputDir, fname1);

				#ifdef WINDOWS
					convertImage(barra, CurrentDir,p, fname,fname1,true," -transparent rgb(128,128,128)"); // #808080");
				#else
					convertImage(barra, CurrentDir,p, fname,fname1,true," -transparent 'rgb(128,128,128)'"); // #808080");
				#endif
				getFilename("P1_","png.aux.xml",time0,outputDir, fname1);
				remove(fname1);  */
		}

		if (outGIF  && !onlyLocations ){

			getFilename("P1_","bmp",time0,outputDir, fname);
			gdalCreateFile(fname,"BMP",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,h,d,true, hgrey,  HCutOff, hmax,-1);
			
			getFilename("P1_","gif",time0,outputDir, fname1);
			convertImage(barra,CurrentDir,p,fname,fname1,true,NULL);
			//printf("-1\n");
			if (!nologo)
			ApplyLogo(barra,CurrentDir,p, fname1);
			if (!nolegend)
			ApplyLegend(barra,CurrentDir,p, fname1);
			if (!notimelabel)
			ApplyTimeLabel(barra,CurrentDir,outputDir,p, time0,fname1);
			//printf("-2\n");
			getFilename("P1_","bmp.aux.xml",time0,outputDir, fname1);
			remove(fname1);
			//printf("-3\n");
			//getFilename("P1_","txt",time0,outputDir, fname);
			//gdalCreateFile(fname,"AAIGrid",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,h,d,true);
		}

		if (checkWatchPoints ||  onlyLocations )
		{
			//printf("-4 wp in\n");
			//indexMax=checkWP(nplaces);//placeList,nplaces,time0,h,d);
			indexMax=checkWP(nplaces,placeList,indexPlaceList,iiMaxWP,minHeightTT);//placeList,nplaces,time0,h,d);
			//CreateRSS(placeList,indexPlaceList,indexMax,outputDir,CurrentInputCase,DateTsunami);
			//printf("-4 wp out\n");
		}

		
		//printf ("tifH2netcdf=%s\n",tifH2netcdf);
		if (tifH2netcdf  && !onlyLocations )
		{//	printf("-5 in\n");
			getFilename("NETCDF_H","nc",-1,outputDir, fname);
			if (firstTime)
			{
				printf("Creating netcdf file for height\n");
				NetCDF_Write(fname,"TIF_H_",NC_WRITE,time0,ncols,nrows,lon_v,lat_v,h,1.);
			}
			else
				
				NetCDF_Write(fname,"TIF_H_",NC_SHARE,time0,ncols,nrows,lon_v,lat_v,h,1.);
			//printf("-5 out\n");
		}
		if (writeWPtxtFiles  && !onlyLocations )
		{  for (i=0; i<=nplaces;i++)
			{if (placeList[i].ArrivalTime!=-1)
			   {
				//printf("%i  %s\n",placeList[i].IDplace,placeList[i].place);
				updateWPtimelines("WT",barra,placeList[i],time0,h,outputDir,iiMaxWP);
				}
			}
		   for (i=0; i<=ngauges;i++)
			{  // gauges are always written to have always the whole curve
				updateWPtimelines("WG",barra,gaugeList[i],time0,h,outputDir,iiMaxWP);
			}
		}

		if (tifuv2netcdf  && !onlyLocations )
		{//printf("-6 in\n");
			getFilename("NETCDF_U","nc",-1,outputDir, fname);
			if (firstTime)
			{
				printf("Creating netcdf file for velocities\n");
				NetCDF_Write(fname,"TIF_U_",NC_WRITE,time0,ncols,nrows,lon_v,lat_v,u,1);
			}
			else
				NetCDF_Write(fname,"TIF_U_",NC_SHARE,time0,ncols,nrows,lon_v,lat_v,u,1);
			
			getFilename("NETCDF_V","nc",-1,outputDir, fname);
			if (firstTime)
				NetCDF_Write(fname,"TIF_V_",NC_WRITE,time0,ncols,nrows,lon_v,lat_v,v,1);
			else
				NetCDF_Write(fname,"TIF_V_",NC_SHARE,time0,ncols,nrows,lon_v,lat_v,v,1);
			//printf("-6 out\n");
		}
		

	}
	else if (mode==1)  //dtPrint major actions
	{
			if (outBMP  && !onlyLocations )
			{
				getFilename("P1_MAXHEIGHT_","bmp",time0,outputDir, fname);
				gdalCreateFile(fname,"BMP",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,mh,d,true, hgrey,  HCutOff, hmax,-1);
				getFilename("TIF_MAXHEIGHT_","tif",time0,outputDir, fname);
				gdalCreateFile(fname,"GTiff",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,mh,d,true, hgrey,  HCutOff, hmax,-1);
				getFilename("P1_MAXHEIGHT_END","bmp",-1,outputDir, fname);
				gdalCreateFile(fname,"BMP",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,mh,d,true, hgrey,  HCutOff, hmax,-1);

			}
			//printf("-7\n");
			if (!onlyLocations && !doNotOverwriteMax)
			{
				getFilename("P1_MAXHEIGHT_END","bmp",-1,outputDir, fname);
				gdalCreateFile(fname,"BMP",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,mh,d,true, hgrey,  HCutOff, hmax,-1);
				//printf("-8\n");
				getFilename("P1_MAXHEIGHT_END","jpg",-1,outputDir, fname1);
				convertImage(barra,CurrentDir,p,fname,fname1,true,NULL);
				ApplyLogo(barra,CurrentDir, p,fname1);
				ApplyLegend(barra,CurrentDir,p, fname1);
				//ApplyTimeLabel(barra,CurrentDir,outputDir,p, time0,fname1);

				//ApplyLogo(fname1);
				//ApplyLegend(fname1);
				//ApplyTimeLabel(time0,fname1);

				getFilename("P1_MAXHEIGHT_END","bmp.aux.xml",-1,outputDir, fname1);
				remove(fname1);

				getFilename("P1_MAXHEIGHT_END","bmp",-1,outputDir, fname);
                        
				//printf("code %s  inundation %i  checkWatchPoints=%i \n",code,inundation,checkWatchPoints);
				if (compString(code,"HYFLUX") && inundation ) 
					{
					printf("1\n");
					gdalCreateFile(fname,"BMP",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,mh,d,true,hgrey,HCutOff,hmax,1128);
					}
				else 	
					gdalCreateFile(fname,"BMP",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,mh,d,true,hgrey,HCutOff,hmax,128);

				getFilename("P1_MAXHEIGHT_END","png",-1,outputDir, fname1);
				#ifdef WINDOWS
					convertImage(barra, CurrentDir,p, fname,fname1,true," -transparent rgb(128,128,128)"); // #808080");
				#else
					convertImage(barra, CurrentDir,p, fname,fname1,true," -transparent 'rgb(128,128,128)'"); // #808080");
				#endif
				getFilename("P1_MAXHEIGHT_END","bmp.aux.xml",-1,outputDir, fname1);
				remove(fname1);

				if (inundation ) 
				{   getFilename("P1_MAXHEIGHT_END","png",-1,outputDir, fname1);
					gdalCreateFile(fname,"BMP",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,mh,d,true,hgrey,HCutOff,hmax,-1128);
					getFilename("P1_INUNDATION_END","png",-1,outputDir, fname1);
					#ifdef WINDOWS
						convertImage(barra, CurrentDir,p, fname,fname1,true," -transparent rgb(128,128,128)"); // #808080");
					#else
						convertImage(barra, CurrentDir,p, fname,fname1,true," -transparent 'rgb(128,128,128)'"); // #808080");
					#endif
					getFilename("P1_INUNDATION_END","bmp.aux.xml",-1,outputDir, fname1);
					remove(fname1);
				}
				
				getFilename("TIF_MAXHEIGHT_END","tif",-1,outputDir, fname);
				gdalCreateFile(fname,"GTiff",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,mh,d,true, hgrey,  HCutOff, hmax,-1);

				getFilename("TIF_Arrival_Time","tif",-1,outputDir, fname);
				gdalCreateFile(fname,"GTiff",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,TarrH,d,true,hgrey,HCutOff,hmax,-1);

				getFilename("TIF_Maximum_Time","tif",-1,outputDir, fname);
				gdalCreateFile(fname,"GTiff",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,TarrMax,d,true,hgrey,HCutOff,hmax,-1);

			}
			if (checkWatchPoints ||  onlyLocations )
			{
				//printf("-9\n");
				//indexMax=checkWP(nplaces);//placeList,nplaces,time0,h,d);
				indexMax=checkWP(nplaces,placeList,indexPlaceList,iiMaxWP,minHeightTT);//placeList,nplaces,time0,h,d);
				CreateRSS("locations",placeList,indexPlaceList,indexMax,outputDir,CurrentInputCase,DateTsunami, faults, Nfaults);
				CreateTxt("locations",placeList,indexPlaceList,indexMax,outputDir,CurrentInputCase,DateTsunami);
			}
			if (outkml || onlyLocations ) {
				//printf("-10\n");
				Createkml(placeList,indexPlaceList, indexMax, time0, outputDir, CurrentInputCase,DateTsunami);
			}
	}
	else if (mode==2)  //final actions
	
	{  //printf("-2  %s\n",outGifAnimation);
		//printf("outGIFanimation=%s\n",outGifAnimation);
		if (outGifAnimation  && !onlyLocations )
			{	printf("Creating animation file.... will take a while\n");
				getFilename("outres1","gif",-1,outputDir, fname1);
				//getFilename("P1_0*","jpg",-1,outputDir, fname);
				getFilename("P1_0*","gif",-1,outputDir, fname);
				if (ncols>1200 || nrows>1000)
					convertImage(barra,CurrentDir,p,fname,fname1,false," -delay 10 -loop 0 -geometry 1200x1000");
				else
					convertImage(barra,CurrentDir,p,fname,fname1,false," -delay 10 -loop 0 ");
				if (deleteAnimFiles)
				{
				getFilename("P1_0*","gif",-1,outputDir, fname);
			        printf("removing anim files: %s\n",fname);	
                                deleteFiles(outputDir,"P1_*.gif");				
				}
			}
//#ifdef WINDOWS
//			if (tifH2netcdf)
//			{
//				printf("Creating netcdf file for height");
//				createNetcdf(CurrentDir, outputDir,outputDir,"H");
//			}
//			if (tifuv2netcdf)
//			{
//				printf("Creating netcdf file for height");
//				createNetcdf(CurrentDir, inputDir,outputDir,"UV");
//			}
//#endif
	}

}
void dumpResults(float t, char *outDir)
{
	char * fullname=(char *) malloc (256*sizeof(char));
	getFilename("TXT_H_","txt",t,outDir,fullname);

	FILE *outfile;
	
	/* Open the file.  If NULL is returned there was an error */
	printf("Printing output in %s\n",fullname);
	outfile = fopen(fullname, "w");
	if(outfile!=NULL)
	{
		int i,j;	
		char cr=13;
		char lf=10;
		for (j = 1;j<=jp2;j++)
		{    
			char *sep="";
			for (i = 1;i<=ip2;i++)
			{
				
				fprintf(outfile, "%s%f", sep, h[j][i]);
				sep=", ";
				
			}
			fprintf(outfile,"\n");
		}
		fclose(outfile);
	}
}

float CalculateDegree(float distancekm, float lat)
{
	float EarthRadiuskm = 6340;
	return (float) (distancekm/EarthRadiuskm *180./3.1415927);
}
char * checkFolder(char *outDir)
{
	if (outDir[strlen(outDir)-1]=='\\')
	outDir[strlen(outDir)-1]=0;
	return outDir;
}
char * CurrentFolder(char * argv[])
{int i;
 char * cf=(char*) calloc(500,sizeof(char));
 char * prg;
 prg=argv[0];
    int slp=strlen(prg);
	for (i=0;i<slp;i++)
	{
		if(prg[strlen(prg)-i]==barra[0])
		{
			prg[strlen(prg)-i]=0;
			return prg;
		}
	}
	return barra;
}

void readArguments(int argc, char *argv[], bool updateFiles)
{
	int k;
	printf("\n Reading inline arguments\n");
	for (k=0; k<argc; k++)
	{
		if ((int)((k+1) /2)*2==(k+1))
     	printf("%s %s\n",argv[k],argv[k+1]);
     	if (compString(argv[k],"-inDir"))
		{
     		copystring(argv[k+1],inputDir);
			k++;
		}
     	if (compString(argv[k],"-code"))
		{
     		copystring(argv[k+1],code);
			k++;
		}

		if (compString(argv[k],"-i") && updateFiles)
		{
			copystring(argv[k+1],inputFile);
			k++;
		}
		if (compString(argv[k],"-o"))
		{
			copystring(checkFolder(argv[k+1]),outputDir);
			k++;
		}
		if (compString(argv[k],"-b") && updateFiles)
		{
			copystring(argv[k+1],batFile);
			k++;
		}
		if (compString(argv[k],"-places"))
		{
			copystring(argv[k+1],fnamePlaces);
			k++;
		}

		if (compString(argv[k],"-def") && updateFiles)
		{
			copystring(argv[k+1],deformFile);
			k++;
		}
		if (compString(argv[k],"-opt"))
		{
			copystring(argv[k+1],options);
			k++;
		}
		/*if (compString(argv[k],"-minRatioTT"))
		{
			minRatioTT=(atof)(argv[k+1]);
			k++;
		}*/
		if (compString(argv[k],"-minHeightTT"))
		{
			minHeightTT=(atof)(argv[k+1]);
			k++;
		}
		if (compString(argv[k],"-inFormat"))
		{
			copystring(argv[k+1],inFormat);
			k++;
		}
		if (compString(argv[k],"-batFactor"))
		{
			batFactor=(float) atof(argv[k+1]);
			k++;
		}
		if (compString(argv[k],"-outHTif"))
		{
			outHTif=(strcmp(argv[k+1],"true")==0);
			k++;
		}

		if (compString(argv[k],"-tifH2netcdf"))
		{
			tifH2netcdf=(strcmp(argv[k+1],"true")==0);
			k++;
		}
		if (compString(argv[k],"-nologo"))
		{
			nologo=(strcmp(argv[k+1],"true")==0);
			k++;
		}
		if (compString(argv[k],"-nolegend"))
		{
			nolegend=(strcmp(argv[k+1],"true")==0);
			k++;
		}
		if (compString(argv[k],"-notimelabel"))
		{
			notimelabel=(strcmp(argv[k+1],"true")==0);
			k++;
		}

		if (compString(argv[k],"-outHTif"))
		{
			outHTif=(strcmp(argv[k+1],"true")==0);
			k++;
		}
		if (compString(argv[k],"-doNotOverwriteMax"))
		{
			doNotOverwriteMax=(strcmp(argv[k+1],"true")==0);
			k++;
		}
		if (compString(argv[k],"-outGIF"))
		{
			outGIF=(strcmp(argv[k+1],"true")==0);
			k++;
		}
		if (compString(argv[k],"-outGifAnimation"))
		{
			outGifAnimation=(strcmp(argv[k+1],"true")==0);
			k++;
		}
		if (compString(argv[k],"-writeWPtxtFiles"))
		{
			writeWPtxtFiles=(strcmp(argv[k+1],"true")==0);
			k++;
		}

		if (compString(argv[k],"-onlyCoastal"))
		{
			onlyCoastal=(strcmp(argv[k+1],"true")==0);
			k++;
		}

		if (compString(argv[k],"-inundation"))
		{
			inundation=(strcmp(argv[k+1],"true")==0);
			k++;
		}
		if (compString(argv[k],"-hgrey"))
		{
			hgrey=(atof)(argv[k+1]);
			k++;
		}
		if (compString(argv[k],"-HCutOff"))
		{
			HCutOff=(atof)(argv[k+1]);
			k++;
		}
		if (compString(argv[k],"-hmax"))
		{
			hmax=(atof)(argv[k+1]);
			k++;
		}

		if (compString(argv[k],"-latmin"))
		{
			latmin=(atof)(argv[k+1]);
			k++;
		}

		if (compString(argv[k],"-latmax"))
		{
			latmax=(atof)(argv[k+1]);
			k++;
		}

		if (compString(argv[k],"-lonmin"))
		{
			lonmin=(atof)(argv[k+1]);
			k++;
		}

		if (compString(argv[k],"-lonmax"))
		{
			lonmax=(atof)(argv[k+1]);
			k++;
		}

		if (compString(argv[k],"-deleteGIF_atEnd"))
		{
			deleteGIF_atEnd=(strcmp(argv[k+1],"true")==0);
			k++;
		}
		if (compString(argv[k],"-DateTsunami"))
		{
			/*char * taglabel0;
			//taglabel0=(char *) calloc(500,sizeof(char));
			replace(argv[k+1],"_"," ",taglabel0);
			copystring(taglabel0,argv[k+1]);
			replace(argv[k+1],"_"," ",taglabel0);
			copystring(taglabel0,argv[k+1]);
			replace(argv[k+1],"_"," ",taglabel0);
			copystring(taglabel0,argv[k+1]);*/

			DateTsunami=StringaToDate(argv[k+1]);
			k++;
		}
		
		if (compString(argv[k],"-deleteTIF_atEnd"))
		{
			deleteTIF_atEnd=(strcmp(argv[k+1],"true")==0);
			k++;
		}

		if (compString(argv[k],"-COMCOT_lay"))
		{
			comCotLay=atoi(argv[k+1]);
			k++;
		}
		if (compString(argv[k],"-tarFiles"))
		{
			tarFiles=(strcmp(argv[k+1],"true")==0);
			k++;
		}
		if (compString(argv[k],"-tarFilesAndDelete"))
		{
			tarFilesAndDelete=(strcmp(argv[k+1],"true")==0);
			k++;
		}
		if (compString(argv[k],"-configFile"))
		{
			copystring(argv[k+1],configFile);
			k++;
		}
		if (compString(argv[k],"-templateFiles"))
		{
			copystring(argv[k+1],TemplateFiles);
			k++;
		}

		if (compString(argv[k],"-digitTime"))
		{
			copystring(argv[k+1],digitTime);
			k++;
		}
		if (compString(argv[k],"-tstep"))
		{
			tstep=(atoi) (argv[k+1]);
			k++;
		}
		if (compString(argv[k],"-tend"))
		{
			tend=(atoi) (argv[k+1]);
			k++;
		}

		if (compString(argv[k],"-time1"))
		{
			time1=(atoi) (argv[k+1]);
			k++;
		}
		if (compString(argv[k],"-time2"))
		{
			time2=(atoi) (argv[k+1]);
			k++;
		}
		if (compString(argv[k],"-commentsOn"))
		{
			commentsOn=(strcmp(argv[k+1],"true")==0);
			k++;
		}
		if (compString(argv[k],"-outTTT"))
		{
			outTTT=(strcmp(argv[k+1],"true")==0);
			k++;
		}

		if (compString(argv[k],"-locFileName"))
		{
			copystring(argv[k+1],locFileName);
			k++;
		}
		if (compString(argv[k],"-onlyLocations"))
		{
			onlyLocations=(strcmp(argv[k+1],"true")==0);
			k++;
		}

		if (compString(argv[k],"-translateToTif"))
		{
			translateTif=(strcmp(argv[k+1],"true")==0);
			k++;
		}
		if (compString(argv[k],"-addparams"))
		{
			parsequerystring(argv[k+1]);
			k++;
		}
	}
	if (strstr(options,"rel")!=NULL && updateFiles)
	{
		if (deformFile!=NULL) AddString_s(deformFile,outputDir,barra,deformFile,NULL,NULL);
		if (batFile!=NULL)    AddString_s(batFile,outputDir,barra,batFile,NULL,NULL);
		if (inputFile!=NULL && !compString(inputFile,"NULL"))  AddString_s(inputFile,outputDir,barra,inputFile,NULL,NULL);
	}
	if (compString(inputDir,"")){
		inputDir=(char *) calloc(256,sizeof(char));
		strcpy(inputDir,outputDir);
		
	}
}

void parsequerystring(char * qs)
{
	//dip$EQ$33$AND$rake$EQ$78$AND$width$EQ$230$AND$length$EQ$120$AND$slip$EQ$5
	printf("Parsing addparams string: %s\n",qs);
	char * dummy,*label,*value;
	dummy= (char *) calloc(256, sizeof(char));
	label= (char *) calloc(256, sizeof(char));
	value= (char *) calloc(256, sizeof(char));
	copystring(qs,dummy);
	
	label=strtok(dummy,"_"); 

	while (label !=NULL)
	{
		value=strtok(NULL,"_");
        printf("%s=%s\n",label,value);
		if (compString(label,"-minHeightTT"))
		                      
		{
			minHeightTT=(atof) (value);
			printf("min height TTT=%f\n",minHeightTT);
		}
	
		label=strtok(NULL,"_"); 
	}

}

void readFile(char *inputFile, char * mode)
{
	FILE *infile;
	int lcount=0;
	char line[100];

	printf("Reading file: %s\n",inputFile);
	/* Open the file.  If NULL is returned there was an error */
	if((infile = fopen(inputFile, "r")) == NULL) {
		printf("Error Opening File.\n");
		return;
		;
	}

	while( fgets(line, sizeof(line), infile) != NULL ) {
		/* Get each line from the infile */
		lcount++;
		/* print the line number and data */
		printf("Line %d: %s", lcount, line);
		if (strcmp(mode,"input")==0)
			interpretCalcInput(line);
		if (strcmp(mode,"config")==0)
			interpretConfigLine(line);

	}

	fclose(infile);  /* Close the file */
	printf("\n\nEnd of Reading file: %s\n",inputFile);
}
void interpretCalcInput(char * line)
{
	char *label;
	char *value;

	if (line[0]=='*' || line[0]==' ' || strlen(line)==1) return;
	
	label=ctrim(strtok(line,"=*"));
	value=cleanValue1(strtok(NULL,"=*"));
	if(value!=NULL)
	value=ctrim(value);
	
	if (strcmp(label,"Title")==0)
	copystring(value,Title);
	if (strcmp(label,"outPath")==0)
	//outPath=value;
	//copystring(value,outPath);
	if (strcmp(label,"URL_calc")==0)
	copystring(value,URL_calc);
	if (strcmp(label,"DateTsunami")==0)
	{
		copystring(value,DateTsunamiString);
		// form is  dd MMM yyyy hh:mm:ss
//#ifdef WINDOWS
		if (!compString(DateTsunamiString,""))
		DateTsunami=StringaToDate(DateTsunamiString);
//#endif
	}

	if (strcmp(label,"FaultNumber")==0)
	{
		CurrentFault=atoi(value);
		Nfaults +=1;
	}
	if (strcmp(label,"Lat")==0)
	{  
		if(Nfaults==0) {Nfaults=1;CurrentFault=1;}
		faults[CurrentFault].lat =atof(value);
	}
	if (strcmp(label,"Lon")==0)
	{  
		if(Nfaults==0) {Nfaults=1;CurrentFault=1;}
		faults[CurrentFault].lon =atof(value);
	}
	
	if (strcmp(label,"Mag")==0)
	faults[CurrentFault].mag=(float) atof(value);
	if (strcmp(label,"timeOfRupture")==0)
	faults[CurrentFault].timeOfRupture=(float) atof(value);
	if (strcmp(label,"InTime")==0)
	InTime=(float) atof(value);
	if (strcmp(label,"FinTime")==0)
	FinTime=(float) atof(value);
	if (strcmp(label,"dtMax")==0)
	dtMax=(float) atof(value);
	if (strcmp(label,"Tsave")==0)
	{  
		Tsave=(float) atof(value);
	}
	if (strcmp(label,"fanning")==0)
	fanning=(float) atof(value);
	if (strcmp(label,"faultMode")==0)
	faultMode=atoi(value);
	if (strcmp(label,"faultform")==0)
	faultform=atoi(value);
	if (strcmp(label,"compiler")==0)
	compiler=atoi(value);
	if (strcmp(label,"AutomaticFault")==0)
	AutomaticFault=atoi(value);
	if (strcmp(label,"flenght")==0)
	faults[CurrentFault].flenght=(float)atof(value);
	if (strcmp(label,"fwidth")==0)
	faults[CurrentFault].fwidth=(float)atof(value);
	if (strcmp(label,"flenghtFact")==0)
	faults[CurrentFault].flenghtFactor=(float)atof(value);
	if (strcmp(label,"fwidthFact")==0)
	faults[CurrentFault].fwidthFactor=(float)atof(value);
	if (strcmp(label,"slip")==0)
	faults[CurrentFault].slip=(float)atof(value);
	if (strcmp(label,"strike")==0)
	faults[CurrentFault].strike=(float)atof(value);
	if (strcmp(label,"depth")==0)
	faults[CurrentFault].depth=(float)atof(value);
	if (strcmp(label,"ipodepth")==0)
	faults[CurrentFault].ipodepth=(float)atof(value);
	if (strcmp(label,"dip")==0)
	faults[CurrentFault].dip=(float)atof(value);
	if (strcmp(label,"rake")==0)
	faults[CurrentFault].rake=(float)atof(value);

	if (strcmp(label,"batgrid")==0)
	batgrid=(float) atof(value);
	if (strcmp(label,"lonmin")==0)
	lonmin=(float) atof(value);
	if (strcmp(label,"lonmax")==0)
	lonmax=(float) atof(value);
	if (strcmp(label,"latmin")==0)
	latmin=(float) atof(value);
	if (strcmp(label,"latmax")==0)
	latmax=(float) atof(value);
	if (strcmp(label,"bathymetry")==0)
	{
		int c=strcmp(value,"ETOPO2");
		int cc=strcmp(value,"GEBCO");
		if (strcmp(value,"ETOPO2")>=0) batMode=0;
		if (strcmp(value,"GEBCO")>=0) batMode=1;
	}

	//strcpy(Title,pch[1]);
}


void interpretConfigLine(char * line)
{
	char *tagLabel,*tagLabel0,*code1;
	char *value;
	tagLabel0=(char *) calloc(500,sizeof(char));
	tagLabel =(char *) calloc(500,sizeof(char));
	code1 =(char *) calloc(20,sizeof(char));
	
	AddString_s(code1,"_",code,NULL,NULL,NULL);
	if (line[0]=='*' || line[0]==' ' || strlen(line)==1) return;
	
	tagLabel0=ctrim(strtok(line,"=*"));
	replace(tagLabel0,"_post","",tagLabel);
	copystring(tagLabel,tagLabel0);
	replace(tagLabel0,code1,"",tagLabel);

	value=cleanValue1(strtok(NULL,"=*"));
	value=ctrim(value);
	if (strcmp(tagLabel,"outTxt")==0)
	   outTxt=(strcmp(value,"true")==0);
	
	if (strcmp(tagLabel,"outHTif")==0)
		outHTif=(strcmp(value,"true")==0);

	if (strcmp(tagLabel,"outuvTif")==0)
		outuvTif=(strcmp(value,"true")==0);

	if (strcmp(tagLabel,"outBMP")==0)
		outBMP=(strcmp(value,"true")==0);

	if (strcmp(tagLabel,"outJPG")==0)
		outJPG=(strcmp(value,"true")==0);

	if (strcmp(tagLabel,"outGIF")==0)
		outGIF=(strcmp(value,"true")==0);

		if (strcmp(tagLabel,"checkWatchPoints")==0)
		checkWatchPoints=(strcmp(value,"true")==0);

	if (strcmp(tagLabel,"outkml")==0)
		outkml=(strcmp(value,"true")==0);

	if (strcmp(tagLabel,"outGifAnimation")==0)
		outGifAnimation=(strcmp(value,"true")==0);

	if (strcmp(tagLabel,"deleteAnimFiles")==0)
		deleteAnimFiles=(strcmp(value,"true")==0);
		
	if (strcmp(tagLabel,"intermediateAnimation")==0)
		intermediateAnimation=(strcmp(value,"true")==0);

	if (strcmp(tagLabel,"tifH2netcdf")==0)
		tifH2netcdf=(strcmp(value,"true")==0);

	if (strcmp(tagLabel,"tifuv2netcdf")==0)
		tifuv2netcdf=(strcmp(value,"true")==0);

	if (strcmp(tagLabel,"writeWPtxtFiles")==0)
		writeWPtxtFiles=(strcmp(value,"true")==0);
	
	if (strcmp(tagLabel,"tarFiles")==0)
		tarFiles=(strcmp(value,"true")==0);
		
	if (strcmp(tagLabel,"tarFilesAndDelete")==0)
		tarFilesAndDelete=(strcmp(value,"true")==0);

	if (strcmp(tagLabel,"inFormat")==0)
		copystring(value,inFormat);

	if (strcmp(tagLabel,"outFormat")==0)
		copystring(value,outFormat);

}

void ConvertComcotFiles(char * inputDir, int ncols, int nrows, double cellsize, float lonMin, float latMin, float dtMax, float Tsave, float FinTime, int Lay)
{
	char * prefix = (char*)calloc(256,sizeof(char));
	char * fname  = (char*)calloc(256,sizeof(char));
	char * inpFile= (char*)calloc(256,sizeof(char));
	char * outFile= (char*)calloc(256,sizeof(char));
	char * grdFile= (char*)calloc(256,sizeof(char));
	char * listFile= (char*)calloc(256,sizeof(char));
	
	getFilename("header","txt",-1,inputDir,fname);
	FILE *headfile;
	headfile=fopen(fname,"w");
	fprintf(headfile,"ncols        %i\n",ncols+1);
	fprintf(headfile,"nrows        %i\n",nrows+1);
	fprintf(headfile,"xllcorner    %f\n",lonMin);
	fprintf(headfile,"yllcorner    %f\n",latMin);
	fprintf(headfile,"dx           %f\n",cellsize/60);
	fprintf(headfile,"dy           %f\n",cellsize/60);
	fprintf(headfile,"NODATA_value  99999\n");
	fclose(headfile);
	
	getFilename("header","txt",-1,inputDir,fname);
	
	float time0;
	int dSteps=((Tsave*60.0)/dtMax)+1;
	if (Tsave*60.0/dtMax-(int)(Tsave*60.0/dtMax)<1.e-6)
	dSteps +=1;
	float Tsave1= dSteps*dtMax;
	int Nsteps= ((FinTime*3600.0)/dtMax);
	float FinTime1=  Nsteps*dtMax;
	int Nmaps=Nsteps/dSteps;
	int i0=-dSteps;
	FILE * listFiles;
	getFilename("listfiles","txt",-1,inputDir, listFile);
	listFiles=fopen(listFile,"w");
	
	for(int k=0;k<=Nmaps;k++)
	{
		i0 +=dSteps;
		sprintf(inpFile,"%s%szj_%02u_%06u.%s",inputDir,barra,Lay,i0, "dat");  //input file
		printf("%s\n",inpFile);
		time0=i0*dtMax;
		
		getFilename("zj_","grd",i0,inputDir, grdFile);
		getFilename("TIF_H_","tif",time0,inputDir, outFile);
		
		combineFiles(fname,inpFile,grdFile);
		gdaltranslate(CurrentDir,barra,p,grdFile,outFile);
		deleteFiles(inputDir,grdFile);
		fprintf(listFiles,"%f     %s\n",time0,outFile);
	}
	fclose(listFiles);
}
void readGridFile(char * fname, float q[(NROWS+1)][(NCOLS+1)], int ip2, int jp2)
{
	//   the file will be a series of xyz as with ComCot
	FILE *infile;
	int lcount=0;
	char line[100];

	/*printf("Enter to continue");
	//scanf("%s",resp);
* Open the file.  If NULL is returned there was an error */
	if((infile = fopen(fname, "r")) == NULL) {
		printf("Error Opening File or file not existing.\n");
		;
	}

	printf ("Reading file: "); printf(fname);printf("\n");
	int j,i;
	j=1;i=2;
	float xx,yy,dd;
	while( fgets(line, sizeof(line), infile) != NULL ) {
		/* Get each line from the infile */
		lcount++;
		/* print the line number and data */
		//printf("Line %d: %s", lcount, line);  
		j++;
		if (j==jp2)
		{
			j=2;
			i++;
		}	
		xx=(float)atof(ctrim(strtok(line," ")));
		yy=(float)atof(strtok(NULL," "));
		dd=(float)atof(strtok(NULL," "));
		//if (setvalue !=-1)
		//	dd=4000;
		q[j][i]=dd;
		//printf ("%u  %u %f %f %f \n",j,i,xx,yy,dd);
	}
	fclose(infile);
	// set borders
	for (i=1;i<=ip2;i++)
	{
		q[1][i]    =q[2][i];
		q[jp2][i]=q[jp2-1][i];
	}
	for (j=1;j<=jp2;j++)
	{
		q[j][1]    =q[j][2];
		q[j][ip2]=q[j][ip2-1];
	}


}
int selectGauges(char * fname, placedef  * place, float lonmin, float lonmax, float latmin, float latmax, bool countOnly)
{
	FILE *infile;
	char * ch;
	ch=(char*) calloc(1,sizeof(char));
	ch[0]=10;
	ch[1]=0;
	int lcount=0,i,n,ix,iy,IDplace,popest,cityclass;
	char line[1000];
	char * dummy, * location, * country;
	float lat,lon;
	printf("Selecting measuring gauges %s\n",fname);
	/* Open the file.  If NULL is returned there was an error */
	if((infile = fopen(fname, "r")) == NULL) {
		printf("Error Opening File.\n");
		return -1;
		;
	}
	fgets(line, sizeof(line), infile); //labels
	n=-1;
	location= (char *) calloc(512,sizeof(char));
	while( fgets(line, sizeof(line), infile) != NULL ) {
		//ID,lat,lon,latDegMin,lonDegMin,name
		IDplace=atoi(strtok(line,","));
		if (IDplace==2632)
			IDplace=IDplace;
		lat=(float) atof(ctrim(strtok(NULL,",")));
		lon=(float) atof(ctrim(strtok(NULL,",")));
//		for (i=1;i<=2;i++) dummy=strtok(NULL,",");
		location=ctrim(cleanValue1(strtok(NULL,",")));
		//replace(location,dummy,ch," ");
		//location=ctrim(location);
		//for (i=1;i<=13;i++) dummy=strtok(NULL,"/t");
		if ((lonmin>180 || lonmax>180) && lon<0) lon +=360;
		if ((lonmin<-180 || lonmax<=-180) && lon>0) lon -=360;
		//printf("%f %f %s %s  %i  %i\n",lat,lon,place, country,popest,cityclass);
		//if (lat>latmax) 
		//{
		//	break;  // places are ordered by lat,lon
		//}
		
		if (lon>=lonmin && lon <= lonmax && lat>=latmin && lat<=latmax)
		{
			getID(lon,lat,lonmin,dx0,latmin,dy0,&ix,&iy);
			if (h[iy][ix]<99999.)
			{
				n++;
				if (!countOnly)
				{
					//placeList[n]="                                                                                              ";
					place[n].IDplace=IDplace;

					place[n].lon=lon;
					place[n].lat=lat;
					place[n].ArrivalTime =-1;
					place[n].MaxHeight =-1;
					place[n].TimeMaxHeight =-1;
					getID(lon,lat,lonmin,dx0,latmin,dy0,&ix,&iy);
					place[n].ix =ix;
					place[n].iy =iy;
					//place[n].popEst=popest;
					//place[n].cityClass=cityclass;
					strcpy(place[n].place,location);
					//strcpy(place[n].country,country);
					if (commentsOn) printf("%f %f %i %i %s\n",lat,lon,ix,iy,location);
				}			
				//sprintf(place[n],"%s,%s,%f,%f",place,country,lon,lat);
			}
		}
	}
	fclose(infile);
	printf("Gauges names found in area: %i \n" ,n);
	return n;
}

int selectPlaces(char * fname, placedef  * place, float lonmin, float lonmax, float latmin, float latmax, bool countOnly)
{
	FILE *infile;
	int lcount=0,i,n,ix,iy,IDplace,cityclass,popest;
	char line[1000];
	char * dummy, * location, * country;
	float lat,lon;
	printf("Selecting place names");
	/* Open the file.  If NULL is returned there was an error */
	if((infile = fopen(fname, "r")) == NULL) {
		printf("Error Opening File.\n");
		return -1;
		;
	}
	fgets(line, sizeof(line), infile); //labels
	n=-1;

	while( fgets(line, sizeof(line), infile) != NULL ) {
		//2,8,14,15
		IDplace=atoi(strtok(line,","));
		location=ctrim(strtok(NULL,","));
		for (i=1;i<=5;i++) dummy=strtok(NULL,",");
		country=ctrim(strtok(NULL,","));
		for (i=1;i<=3;i++) dummy=strtok(NULL,",");
		cityclass=atoi(strtok(NULL,","));
		popest=atoi(strtok(NULL,","));
		lon=(float)atof(ctrim(strtok(NULL,",")));
		//if (lon==-1)
		//lon=atof(ctrim(strtok(NULL,",")));
		lat=(float)atof(strtok(NULL," "));
		if ((lonmin>180 || lonmax>180) && lon<0) lon +=360;
		if ((lonmin<-180 || lonmax<=-180) && lon>0) lon -=360;
//		printf("%f %f %s %s\n",lat,lon,location, country);
	/*	if (lat>latmin)
		{
		lat=lat;
		}
	*/	if (lat>latmax) 
		{
			//break;  // places are ordered by lat,lon
		}
		
		if (lon>=lonmin && lon <= lonmax && lat>=latmin && lat<=latmax)
		{
			getID(lon,lat,lonmin,dx0,latmin,dy0,&ix,&iy);
			if (checkDepthWP(ix,iy,d,minDepthWP) && IsInDomainCalc(ix,iy,h))
			{
				n++;
				if (!countOnly)
				{
					//placeList[n]="                                                                                                    ";
					place[n].IDplace=IDplace;
					place[n].lon=lon;
					place[n].lat=lat;
					place[n].ArrivalTime =-1;
					place[n].MaxHeight =-1;
					place[n].TimeMaxHeight =-1;
					getID(lon,lat,lonmin,dx0,latmin,dy0,&ix,&iy);
					place[n].ix =ix;
					place[n].iy =iy;
					place[n].cityClass=cityclass;
					place[n].popEst=popest;
					strcpy(place[n].place,location);
					strcpy(place[n].country,country);
					if (commentsOn) printf("%f %f %i %i %s %s\n",lat,lon,ix,iy,location, country);
					
				}			
				//sprintf(place[n],"%s,%s,%f,%f",place,country,lon,lat);
			}
		}
	}
	fclose(infile);
	printf("Place names found in area: %i \n" ,n);
	return n;
}

void getID(float lon, float lat, float lonmin, float dxCell, float latmin, float dyCell, int * idx, int *idy)
{
	*idx =(int) (((lon - lonmin) / dxCell + 1) + 1);
    *idy =(int) (((lat - latmin) / dyCell + 1) + 1);
    if (lon < 0)  *idx -=1;
    if(lat < 0) *idy -= 1;
}

void getCoord(int idx, int idy, float lonmin,float dxCell, float  latmin, float dyCell , float * lon,float * lat)
{
	*lon = (float) (lonmin + dxCell * (idx - 1.5));
    *lat = (float) (latmin + dyCell * (idy - 1.5));
}
    
bool checkDepthWP(int ix, int iy,float d[(NROWS+1)][(NCOLS+1)],float minDeptWP)
{
		int ix0,iy0,ix1,iy1,kx,ky;
		float dep;
		bool found=false;
		ix0=ix-iiMaxWP;
		ix1=ix+iiMaxWP;
		iy0=iy-iiMaxWP;
		iy1=iy+iiMaxWP;
		for (kx=ix0;kx<=ix1;kx++)
		{
			for (ky=iy0;ky<=iy1;ky++)
			{
				dep=d[ky][kx];
				if (dep>minDeptWP) 
				{
					found=true;
					break;
				}
			}
		}
		return found;
}

bool IsInDomainCalc(int ix, int iy,float h[(NROWS+1)][(NCOLS+1)])
{
		int ix0,iy0,ix1,iy1,kx,ky;
		float hh;
		bool found=false;
		ix0=ix-iiMaxWP;
		ix1=ix+iiMaxWP;
		iy0=iy-iiMaxWP;
		iy1=iy+iiMaxWP;
		for (kx=ix0;kx<=ix1;kx++)
		{
			for (ky=iy0;ky<=iy1;ky++)
			{
				hh=h[ky][kx];
				if (hh<10000) 
				{
					found=true;
					break;
				}
			}
		}
		return found;
}

int checkWP(int nplaces, placedef * place, int * indexPlace, int imax, float setpoint)
{
	int ix0,iy0,ix1,iy1,kx,ky;
	float dep,hei,ahei, currMaxHei;
	//1. go through placenames and
	//    if h>0.01 >>  arrival time
	//    if h>h0   >>  max height and time of maxheight
	for (int i=0; i<=nplaces;i++)
	{
		ix0=place[i].ix-imax;
		ix1=place[i].ix+imax;
		iy0=place[i].iy-imax;
		iy1=place[i].iy+imax;
		currMaxHei=0;
		for (kx=ix0;kx<=ix1;kx++)
		{
			for (ky=iy0;ky<=iy1;ky++)
			{
				if (ky>0 && kx>0)
				{
					dep=d[ky][kx];
					hei=h[ky][kx];
					ahei=fabs(hei-h0[ky][kx]);
					if(hei != 99999.0)
					{
						if(hei>currMaxHei)
							currMaxHei=hei;
						
						if(ahei>setpoint) 
						{
							if (place[i].ArrivalTime==-1 && dep > minDepthTT ) 
							{
									place[i].ArrivalTime=time0;
									indexMax +=1;
									indexPlace[indexMax]=i;

							}
							if(hei>place[i].MaxHeight && dep > minDepthWP )
							{
								place[i].MaxHeight =hei;
								place[i].TimeMaxHeight =time0;
							}
						}
					}	
				}
			}		
		}
	
		if(currMaxHei>1 && currMaxHei!=place[i].h1m)
		{
			place[i].h1m=currMaxHei;				
			place[i].TimeLowerThan1m =time0;
		}
	}
	return indexMax;
}


//void checkWP(placedef placeList[], int nplaces, float time0, float h[(NROWS+1)][(NCOLS+1)],float d[(NROWS+1)][(NCOLS+1)])
int checkWP0(int nplaces)
{
	
	float maxhei,minhei;
	//1. go through placenames and
	//    if h>0.01 >>  arrival time
	//    if h>h0   >>  max height and time of maxheight
	for (int i=0; i<=nplaces;i++)
	{
		maxhei=getMaxValue(&placeList[i],h,iiMaxWP);
		minhei=getMinValue(&placeList[i],h,iiMaxWP);
		
		if(fabs(maxhei)>minHeightTT || fabs(minhei)>minHeightTT) 
		{
			if (placeList[i].ArrivalTime==-1 ) 
			{
					placeList[i].ArrivalTime=time0;
					indexMax +=1;
					indexPlaceList[indexMax]=i;

			}
			if(maxhei>placeList[i].MaxHeight )
			{
				placeList[i].MaxHeight =maxhei;
				placeList[i].TimeMaxHeight =time0;
			}
			if(maxhei>1)
			{
				placeList[i].TimeLowerThan1m =time0;
			}
		}
		
	}
	return indexMax;
}



inputCase assignInput()
{
	inputCase CurrentInputCase;
	copystring(Title, CurrentInputCase.Title);
	copystring(outPath,CurrentInputCase.outPath);
	copystring(URL_calc,CurrentInputCase.URL_calc );
	copystring(DateTsunamiString,CurrentInputCase.DateTsunamiString );
	CurrentInputCase.Lat=Lat;
	CurrentInputCase.Lon=Lon;
	CurrentInputCase.Mag=Mag;
	CurrentInputCase.InTime=InTime;
	CurrentInputCase.FinTime=FinTime;
	CurrentInputCase.Tsave=Tsave;
	CurrentInputCase.dtMax=dtMax;
	CurrentInputCase.fanning=fanning;
	CurrentInputCase.watDep=watDep;
	CurrentInputCase.faultMode=faultMode;
	CurrentInputCase.faultform=faultform;
	CurrentInputCase.compiler=compiler;
	CurrentInputCase.AutomaticFault=AutomaticFault;

	CurrentInputCase.flenght=flenght;
	CurrentInputCase.fwidth=fwidth;
	CurrentInputCase.strike=strike;
	CurrentInputCase.slip=slip;
	CurrentInputCase.dip=dip;
	CurrentInputCase.rake=rake;
	CurrentInputCase.depth=depth;
	CurrentInputCase.width=width;
	CurrentInputCase.batgrid=batgrid;
	CurrentInputCase.lonmin=lonmin;
	CurrentInputCase.lonmax=lonmax;
	CurrentInputCase.latmin=latmin;
	CurrentInputCase.latmax=latmax;
	CurrentInputCase.time0=time0;

	return CurrentInputCase;
}

void initLatLon(float dx0, float dy0, int ncols, int nrows,double * xx, double *yy)
{
	int j,i;	
	//c      init cell quantities
	xx[0] = (lonmin + dx0 / 2);
	yy[0] = (latmin + dy0 / 2);
	
	for (i = 0;i<ncols-1;i++)
	xx[i + 1] =  (xx[i] + dx0);
	
	for (j = 0;j< nrows-1;j++){
		yy[j + 1] = (yy[j] + dy0);
	}
	
}

void interpolateBathymetry(char * fnameBat, float lonmin, float lonmax, float latmin, float latmax, int ncols, int nrows)
{
	float lon, lat, xmin,  dxCell, ymax,  dyCell;
	char * prefix=(char *) calloc(256,sizeof(char));
	AddString_s(prefix,"..",barra,"DATA",barra,NULL); //00.tif"; // if is in the upper directory
	//char * prefix=AddString("",barra,"DATA",barra,NULL); //00.tif";

	char *fnameTif;
	char *fnameTifOld;
	char *fnameBatOld;
	

	fnameTifOld= (char *) calloc(256,sizeof(char));
	fnameTif= (char *) calloc(256,sizeof(char));
    fnameBatOld= (char *) calloc(256,sizeof(char));
   
	
	int ix,iy; 
	float ret, lon1,lat1;
	GDALRasterBandH  dstband;
	GDALDatasetH  hDstDS;
	int nXSize,  nYSize, ierr;
//	*dxCell=0.;*dyCell=0.;*xmin=0;*ymax=0;*nXSize=0;*nYSize=0;
    lon=0;lat=0.;
   
    printf("*** Interpolating . Dataset \n");
	printf("ncols=%i, nrows=%i\n",ncols,nrows);
	dx0 = (lonmax - lonmin) / ncols;
	dy0 = (latmax - latmin) / nrows;
    int nc;
	for (ix=1; ix<ncols+1; ix++)
	{
		if((int)(ix/10)*10==ix || ix==0)
		printf("ix=%i/%i\n",ix,ncols);
		for (iy=1; iy<nrows+1; iy++)
		{
			lon=lonmin+dx0*(float) ix- dx0/2;
			lat=latmin+dy0*(float) iy- dy0/2;
				
				if (!compString(fnameBat,fnameBatOld))
				{
					char * fnameDummy=(char*) calloc(512,sizeof(char));
					AddString_s(fnameDummy,CurrentDir,barra,prefix,fnameBat,NULL);
					solveDir(fnameDummy,barra,nc,"..",CurrentDir,p,fnameTif);

					if (CurrentDir[0]==barra[0] && fnameTif[0] !=barra[0])
					{       copystring(fnameTif,fnameDummy);
							AddString_s(fnameTif,barra,fnameDummy,NULL,NULL,NULL);
					}
					if (strlen(fnameBatOld)!=0)
						 ret=gdalGetValue( lon,  lat, fnameTifOld, 2, dstband, hDstDS, nXSize, nYSize, xmin, dxCell, ymax, dyCell, ierr);
				
					ret=gdalGetValue( lon,  lat, fnameTif, 0, dstband, hDstDS, nXSize, nYSize, xmin, dxCell, ymax, dyCell,ierr);
				}
				lon1=lon;lat1=lat;
				if (lon1> 180) lon1 -=360;
				if (lon1<-180) lon1 +=360;
			
				ci[iy+1][ix+1]=gdalGetValue( lon1,  lat1, fnameTif, 3, dstband, hDstDS, nXSize, nYSize, xmin, dxCell, ymax, dyCell,ierr);
				if (ci[iy+1][ix+1]!=NODATA)
					lon1=lon1;
				if (ci[iy+1][ix+1]==-1)
					lon1=lon1;
				copystring(fnameBat,fnameBatOld);
				copystring(fnameTif,fnameTifOld);

			
				//if(iy==1) printf("%f %f  %f \n",lon1,lat1,d[iy+1][ix+1]);
		}
	}
	
	ret=gdalGetValue( lon,  lat, fnameTif, 2, dstband, hDstDS, nXSize, nYSize, xmin, dxCell, ymax, dyCell,ierr);
}
