#include "definitions.h"
#include "gdal_int.h"
#include "util.h"
#include "stringlib.h"
#include "coastalIndex.h"
#include "netcdf_int.h"

char * code;
placedef coastList[50000];
int indexCoastList[50000];

int indexMax=-1;

// Argument variables
char * inputFile,*inputDir,*outputDir,*options,*batFile, *deformFile, *fnamePlaces, * fnameGauges;
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
float hgrey = (float) 0.05;
float HCutOff = (float) 0.1;
float hmax = 3;

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
float d[(NROWS+1)][(NCOLS+1)]; // max height
float mh[(NROWS+1)][(NCOLS+1)]; // max height
float TarrH[(NROWS+1)][(NCOLS+1)]; // Arrival time (h)
float TarrMax[(NROWS+1)][(NCOLS+1)]; // Arrival time (h)
float ci[(NROWS+1)][(NCOLS+1)];

bool interp=true;

char * p[60];
char *bathymetry;
char *inFormat, *outFormat;
int  ipTot,  jpTot;
int  ip2,  jp2,  * indexLimits;
float dx0,dy0;
float NewTPrint;
float NewMajorPrint;
float dxCell,   dyCell;
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
	outputFile = (char *) calloc(512,sizeof(char));
	fname      = (char *) calloc(512,sizeof(char));
	fname1      = (char *) calloc(512,sizeof(char));
	code       = (char *) calloc(20 ,sizeof(char));
	options    = (char *) calloc(512,sizeof(char));
	inputFile  = (char *) calloc(512,sizeof(char));
	outputDir  = (char *) calloc(512,sizeof(char));
	batFile    = (char *) calloc(512,sizeof(char));
	CurrentDir = (char *) calloc(512,sizeof(char));
	inputDir   = (char *) calloc(512,sizeof(char));
	fname      = (char *) calloc (256,sizeof(char));
	value      = (char *) calloc (256,sizeof(char));
	inFormat     = (char *) calloc(512,sizeof(char));
	outFormat    = (char *) calloc(512,sizeof(char));
	configFile   = (char *) calloc(512,sizeof(char));
	TemplateFiles = (char *) calloc(512,sizeof(char));
	digitTime    = (char *) calloc(512,sizeof(char));
	locFileName  = (char *) calloc(512,sizeof(char));
	faults      =(sourcePar*) calloc(100,sizeof(sourcePar));
	//minRatioTT=1.e-6;
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
	
	readFile(inputFile,"input");
	CurrentInputCase=assignInput();
	
	ncols =(int) (0.5 + (lonmax - lonmin) / (batgrid/60));
	nrows =(int) (0.5 + (latmax - latmin) / (batgrid/60));
	dxCell=batgrid/60;
	checkFolder(CurrentDir);
	checkFolder(inputDir);
	checkFolder(outputDir);
	
	ipTot=NCOLS;jpTot=NROWS;

	ip2 = ncols + 2;
	jp2 = nrows + 2;

	dx0 = (lonmax - lonmin) / ncols;
	dy0 = (latmax - latmin) / nrows;

	computeCoastal(outputDir,lonmin,lonmax,latmin,latmax,ncols,nrows);

	getFilename("end-of-stuff","txt",-1,outputDir,fnameDummy);
	FILE *outfile;
	outfile = fopen(fnameDummy, "w");
	fprintf(outfile,"End of stuff");
	fclose(outfile);
	return 0;
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

		if (compString(argv[k],"-commentsOn"))
		{
			commentsOn=(strcmp(argv[k+1],"true")==0);
			k++;
		}
		
		if (compString(argv[k],"-interp"))
		{
			interp=(strcmp(argv[k+1],"true")==0);
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
		/*if (strcmp(mode,"config")==0)
			interpretConfigLine(line);*/

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
	faults[CurrentFault].lat =atof(value);
	if (strcmp(label,"Lon")==0)
	faults[CurrentFault].lon=atof(value);
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

void computeCoastal(char * outputDir,  float lonmin, float lonmax, float latmin, float latmax,int ncols, int nrows)
{   
	char * fname1 ;
	
		fname1=(char*) calloc(250,sizeof(char));

	printf("Reading bathymetry\n");
	getFilename("bathymetry","tif",-1,outputDir, fname1);
	int ret=gdalReadFile(fname1,1,1,1,d,true);

	printf("Reading Arrival Time\n");
	getFilename("TIF_Arrival_Time","tif",-1,outputDir, fname1);
    ret=gdalReadFile(fname1,1,1,1,TarrH,true);

	printf("Reading Max Height\n");
	getFilename("TIF_MAXHEIGHT_END","tif",-1,outputDir, fname1);
	 ret=gdalReadFile(fname1,1,1,1,mh,true);

	printf("Reading Time of Max Height\n");
	getFilename("TIF_Maximum_Time","tif",-1,outputDir, fname1);
	 ret=gdalReadFile(fname1,1,1,1,TarrMax,true);

	int i,j;
	//read coastal indices
	if (interp)
	{
		interpolateBathymetryGebco30("coastalIndices.tif",lonmin,lonmax,latmin,latmax,ncols,nrows,ci);
		getFilename("coastalIndex","tif",-1,outputDir, fname1);
		printf("fname=%s\n",fname1);
		printf("%f %f %f %f %i %i\n",lonmin,lonmax,latmin,latmax,ncols,nrows);
		gdalCreateFile(fname1,"GTiff",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,ci,ci,true,0.0,0.0,0.0,-1);
	}
	else
	{
		getFilename("coastalIndex","tif",-1,outputDir, fname1);
		ret=gdalReadFile(fname1,1,1,1,ci,true);

	}
	printf("end of interpolation\n");
	
	printf("end of creation\n");

	char * dummy=(char *) calloc(100,sizeof(char));
	copystring("na",dummy);
	
	for (j=0;j<=50000;j++)
	{
		coastList[j].ArrivalTime=1e9;
		coastList[j].MaxHeight=-1;
		coastList[j].TimeMaxHeight=1e9;
		coastList[j].IDplace=-1;
		copystring(dummy,coastList[j].place);
		copystring(dummy,coastList[j].country);
	}

	int index;
	index=-1;
		for (j = 2;j<=jp2-1;j++)
		{    
			for (i = 2;i<=ip2-1;i++)
			{
				if (ci[j][i] != NODATA && mh[j][i]>0.01 )
				{	int n=ci[j][i];		
					
					if (coastList[n].IDplace ==-1)
					{
						index +=1;
						indexCoastList[index]=n;
						coastList[n].IDplace=n;
					}
					
					if ((coastList[n].ArrivalTime>TarrH[j][i] ||  coastList[n].ArrivalTime==1e9) && TarrH[j][i] !=NODATA)
					coastList[n].ArrivalTime =TarrH[j][i]*3600.;

					if (coastList[n].MaxHeight<mh[j][i] ||  coastList[n].MaxHeight==-1)
					coastList[n].MaxHeight =mh[j][i];

					if ((coastList[n].TimeMaxHeight>TarrMax[j][i] ||  coastList[n].TimeMaxHeight==1e9) && TarrMax[j][i] != NODATA)
					coastList[n].TimeMaxHeight =TarrMax[j][i]*3600.;
				}
			}
			
		}
		//for (i=0;i<=index;i++)
		//{
		//			printf("%i    %i   %f \n",i,indexCoastList[i],coastList[indexCoastList[i]].MaxHeight);
		//}
		printf("Interpoation completed\n");
		CreateRSS("coastPlaces",coastList,indexCoastList,index,outputDir,CurrentInputCase,DateTsunami,faults,Nfaults);
		CreateTxt("coastPlaces",coastList,indexCoastList,index,outputDir,CurrentInputCase,DateTsunami);

		for (j = 2;j<=jp2-1;j++)
		{    
			for (i = 2;i<=ip2-1;i++)
			{
				if(ci[j][i] == NODATA || mh[j][i]<0.01)
					mh[j][i]=NODATA;
			}
		}

		//Coastal Impact represents the maximum height in the coastal locations as calculated. It just put NODATA where there is no calculation
		getFilename("coastalImpact","tif",-1,outputDir, fname1);
		printf("fname=%s\n",fname1);
		gdalCreateFile(fname1,"GTiff",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,mh,mh,true,0.0,0.0,0.0,-1);
		
		for (j = 2;j<=jp2-1;j++)
		{    
			for (i = 2;i<=ip2-1;i++)
			{
				if(ci[j][i] == NODATA || mh[j][i]<0.01)
					mh[j][i]=0.0;
			}
		}
		
		getFilename("coastalImpact","bmp",-1,outputDir, fname1);
		gdalCreateFile(fname1,"BMP",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,mh,d,true,hgrey,HCutOff,hmax,128);
		char * fname2=(char *) calloc(256,sizeof(char));
		getFilename("coastalImpact","png",-1,outputDir, fname2);
		#ifdef WINDOWS
			convertImage(barra, CurrentDir,p, fname1,fname2,true," -transparent rgb(128,128,128)"); // #808080");
		#else
			convertImage(barra, CurrentDir,p, fname1,fname2,true," -transparent 'rgb(128,128,128)'"); // #808080");
		#endif
		
		//Coastal Impact Max writes in each pixel the maximum height related to the coastal section
		for (j = 2;j<=jp2-1;j++)
		{    
			for (i = 2;i<=ip2-1;i++)
			{
				if(ci[j][i] == NODATA || mh[j][i]<0.01)
					mh[j][i]=NODATA;
				else
				{
					int index=ci[j][i];
					float max=coastList[index].MaxHeight;
					mh[j][i]=max;
				}
			}
		}
		
		getFilename("coastalImpactMax","tif",-1,outputDir, fname1);
		printf("fname=%s\n",fname1);

		gdalCreateFile(fname1,"GTiff",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,mh,mh,true,0.0,0.0,0.0,-1);

		for (j = 2;j<=jp2-1;j++)
		{	for (i = 2;i<=ip2-1;i++)
			{	if(ci[j][i] == NODATA || mh[j][i]<0.01)
					mh[j][i]=0.;
			}
		}
		getFilename("coastalImpactMax","bmp",-1,outputDir, fname1);
		
		gdalCreateFile(fname1,"BMP",lonmin,lonmax,latmin,latmax,ncols,nrows,1,1,1.,mh,d,true,hgrey,HCutOff,hmax,128);
		getFilename("coastalImpactMax","png",-1,outputDir, fname2);
		#ifdef WINDOWS
			convertImage(barra, CurrentDir,p, fname1,fname2,true," -transparent rgb(128,128,128)"); // #808080");
		#else
			convertImage(barra, CurrentDir,p, fname1,fname2,true," -transparent 'rgb(128,128,128)'"); // #808080");
		#endif
		
		
		
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
					printf("opening %s\n",fnameTif);
					ret=gdalGetValue( lon,  lat, fnameTif, 0, dstband, hDstDS, nXSize, nYSize, xmin, dxCell, ymax, dyCell,ierr);
					copystring(fnameBat,fnameBatOld);
					copystring(fnameTif,fnameTifOld);	
				}
				lon1=lon;lat1=lat;
				if (lon1> 180) lon1 -=360;
				if (lon1<-180) lon1 +=360;
			
				ci[iy+1][ix+1]=gdalGetValue( lon1,  lat1, fnameTif, 3, dstband, hDstDS, nXSize, nYSize, xmin, dxCell, ymax, dyCell,ierr);
				
		}
	}
	
	ret=gdalGetValue( lon,  lat, fnameTif, 2, dstband, hDstDS, nXSize, nYSize, xmin, dxCell, ymax, dyCell,ierr);
}


void interpolateBathymetryGebco30(char * fnameBat, float lonmin, float lonmax, float latmin, float latmax, int ncols, int nrows, float d[(NROWS+1)][(NCOLS+1)])
{
	float lon, lat, xmin,  dxCell, ymax,  dyCell;
	char * prefix=(char *) calloc(256,sizeof(char));
	AddString_s(prefix,"..",barra,"DATA",barra,NULL); //00.tif"; // if is in the upper directory
	//char * prefix=AddString("",barra,"DATA",barra,NULL); //00.tif";

	char *fnameTif;
	char *fnameInp;
	char *fnameClip;
	int nc;	
	fnameInp= (char *) calloc(256,sizeof(char));
	fnameTif= (char *) calloc(256,sizeof(char));
   fnameClip= (char *) calloc(256,sizeof(char));
	
	char * fnameDummy=(char*) calloc(512,sizeof(char));
	AddString_s(fnameDummy,CurrentDir,barra,prefix,fnameBat,NULL);
	solveDir(fnameDummy,barra,nc,"..",CurrentDir,p,fnameTif);

	getFilename("clipFileCI","tif",-1,outputDir, fnameClip);
	
	int ix1, ix2, iy1, iy2;
	float dx=360.0/43200.0;
	ix1 = (lonmin - (-180)) / dx - 1;
	ix2 = (lonmax - (-180)) / dx + 1;
	iy1 = (90 - latmax) / dx - 1;
	iy2 = (90 - latmin) / dx + 1;

	if (ix1<0  || ix2>43200   )
	{	ix1=0;
		ix2=43200;
	}
	if (iy1<0 || iy2>21600    )
	{
		iy1=0;
		iy2=21600;
	}
	sprintf(fnameInp,"-srcwin %i %i %i %i",ix1,iy1,ix2-ix1,iy2-iy1);
	AddString_s(fnameDummy,fnameInp," ",fnameTif,NULL,NULL);
	
	gdaltranslate(CurrentDir, barra, p,  fnameDummy, fnameClip);
	
	//Debug.Print "gdal_translate -srcwin " & ix1 & " " & iy1 & " " & ix2 - ix1 & " " & iy2 - iy1 & " gebco30.tif clip.tif"

	
	int ix,iy; 
	float ret, lon1,lat1;
	GDALRasterBandH  dstband;
	GDALDatasetH  hDstDS;
	int nXSize,  nYSize, ierr;
//	*dxCell=0.;*dyCell=0.;*xmin=0;*ymax=0;*nXSize=0;*nYSize=0;
    lon=0;lat=0.;
    
    printf("*** Interpolating clipped bathymetry. Dataset %s \n",fnameClip);
	printf("ncols=%i, nrows=%i\n",ncols,nrows);
	dx0 = (lonmax - lonmin) / ncols;
	dy0 = (latmax - latmin) / nrows;
	bool first=true;
	for (ix=1; ix<ncols+1; ix++)
	{
		if((int)(ix/10)*10==ix || ix==0)
		printf("ix=%i/%i\n",ix,ncols);
		for (iy=1; iy<nrows+1; iy++)
		{
			lon=lonmin+dx0*(float) ix- dx0/2;
			lat=latmin+dy0*(float) iy- dy0/2;
			if (first)
					ret=gdalGetValue( lon,  lat, fnameClip, 0, dstband, hDstDS, nXSize, nYSize, xmin, dxCell, ymax, dyCell, ierr);
			first=false;
			lon1=lon;lat1=lat;
			if (lon1> 180) lon1 -=360;  
			if (lon1<-180) lon1 +=360;
			d[iy+1][ix+1]=gdalGetValue( lon1,  lat1, fnameClip, 3, dstband, hDstDS, nXSize, nYSize, xmin, dxCell, ymax, dyCell,ierr);
			//if(iy==1) printf("%f %f  %f \n",lon1,lat1,d[iy+1][ix+1]);
		}
	}
	
	ret=gdalGetValue( lon,  lat, fnameClip, 2, dstband, hDstDS, nXSize, nYSize, xmin, dxCell, ymax, dyCell,ierr);
}
