void cleanValuesTT(float TarrH[(NROWS+1)][(NCOLS+1)],float mh[(NROWS+1)][(NCOLS+1)],float d[(NROWS+1)][(NCOLS+1)], float minDepthTT,float setpoint,int ncols, int nrows, float NODATA);
void cleanGF(float TarrH[(NROWS+1)][(NCOLS+1)],float dx0,float dy0,float d[(NROWS+1)][(NCOLS+1)], float minDepthTT,float setpoint,int ip2, int jp2, float NODATA);

void getFilename(char * prefix, char * extension, float t, char * outDir, char * fullname);
void solveDir(char * stringa , char * delimiter, int &n, char * searchCar,char * CurrentDir,char * pp[60], char * stringaOut);
void split(char * stringa , char * delimiter, int &n, char * searchCar, char **p);
struct tm  StringaToDate(char *dateString0);
void DateToString(struct tm * date0, char * ts);
void updateMaxHeight(char * barra, float time0, float OverallHmax, float CurrentMax, float CurrentMaxDepth, char * outDir);
void writeEpicenter(FILE * outfile, inputCase inp, sourcePar * faults, int Nfaults);
void writeItem(FILE * outfile, placedef place, struct tm DateTsunami);
void writeItemTxt(FILE * outfile, placedef place, struct tm DateTsunami);
float Mod(float a, float module);

void imposePressureField(float time0,float &timeNew, float & timeOld,char * preField, char * vxField, char * vyField, float lonmin, float lonmax, float latmin,float latmax,int ncols,int nrows,float pres[(NROWS+1)][(NCOLS+1)] , float windx[(NROWS+1)][(NCOLS+1)] , float windy[(NROWS+1)][(NCOLS+1)]);
double HoursSince1900(char * dat);

void CreateRSS(char * fname,placedef placeList[],int * indexPlaceList, int indexMax, char * outDir, inputCase inp, struct tm DateTsunami, sourcePar * faults, int Nfaults);
void CreateTxt(char * fname,placedef placeList[],int * indexPlaceList, int indexMax, char * outDir, inputCase inp, struct tm DateTsunami);
void Createkml(placedef * places,int * indexPlaceList, int indexMax, float time0, char * outDir, inputCase inp, struct tm DateTsunami);
void AddImageOverlay(FILE * outfile, inputCase inp);
void colorIcon(float hei, char * colore);
struct tm  AddDate(struct tm  date0, int AddSeconds);
void convertImage(char * barra, char * CurrentDir,char * pp[60], char * fnameSrc, char * fnameDest, bool deleteSrc, char * options);
float getGlobalMaxValue(float h[(NROWS+1)][(NCOLS+1)],int ip2, int jp2);

int month(char * a);
void createListfilesFromTemplate(char * CurrentDir, char * outDir, char * pathFiles, char * digitFiles, int tstep, int tfin, bool
translateTif, char * fname, char * barra, char * p[60]);
void deleteFiles(char * outputDir, char * fileDef);
void copyFile(char * DirInp, char * fileInp, char * DirOut,char * fileOut, char * barra);

void ApplyTimeLabel(char * barra, char * CurrentDir,char * outputDir,char * p[60], float time0, char * fname);
void ApplyLogo(char * barra, char * CurrentDir, char * p[60], char * fname);
void ApplyLegend(char * barra, char * CurrentDir,char * p[60], char * fname);

//void establishMaxHeigth(float h[(NROWS+1)][(NCOLS+1)],float mh[(NROWS+1)][(NCOLS+1)], int ip2, int jp2);
void updateWPtimelines(char * prefix,char * barra, placedef  place, float time0, float h[(NROWS+1)][(NCOLS+1)], char * outDir, int iiMaxWP);
float getMaxValue(placedef *place,float h[(NROWS+1)][(NCOLS+1)],int iiMaxWP);

#ifdef WINDOWS
	void createNetcdf(char * CurrentDir, char *in, char *out, char * opt);
	void MeasureMemory(long * Phys, long * Virt, long * totMem);
#endif

void filterValues(float hin[(NROWS+1)][(NCOLS+1)],float d[(NROWS+1)][(NCOLS+1)],float setpoint, int comparison, int ip2, int jp2);

void establishMaxHeigth(float time0, float h[(NROWS+1)][(NCOLS+1)],float mh[(NROWS+1)][(NCOLS+1)], float TarrMax[(NROWS+1)][(NCOLS+1)],float d[(NROWS+1)][(NCOLS+1)], float minDepthWP,  float setpoint, int ip2, int jp2, float NODATA);

void updateWPtimelines(char * barra, placedef  place, float time0, float h[(NROWS+1)][(NCOLS+1)], char * outDir, int iiMaxWP);
float getMaxValue(placedef *place,float h[(NROWS+1)][(NCOLS+1)],int iiMaxWP);
float getMinValue(placedef *place,float h[(NROWS+1)][(NCOLS+1)],int iiMaxWP);
bool fileExists(char * fname);
float getMaximum(float d[(NROWS+1)][(NCOLS+1)],float h[(NROWS+1)][(NCOLS+1)], int ip2, int jp2,int mode);
void ReadInputFromNETCDF(int IDTime, char * fold, float h[(NROWS+1)][(NCOLS+1)], float lonmin, float lonmax, float latmin, float latmax,int ip2, int jp2,int &nx,int &ny);
void ReadInputFromTIF(float time0, char * fold, float h[(NROWS+1)][(NCOLS+1)], float lonmin, float lonmax, float latmin, float latmax,int ip2, int jp2, int &nx, int &ny);

bool setBoundaryUV_gdal(float time0, float &timeNew, float & timeOld, char * fold, float u[(NROWS+1)][(NCOLS+1)], float v[(NROWS+1)][(NCOLS+1)], float lonmin, float lonmax, float latmin, float latmax,float dx, float dy, int ip2, int jp2, int nx, int ny,double *vbBottom0,double *vbTop0,double *ubLeft0,double *ubRight0,double *vbBottom,double *vbTop,double *ubLeft,double *ubRight,float & lonMinInt,float & lonMaxInt,float & latMinInt, float &latMaxInt);
bool setBoundaryUV_NETCDF(float time0, float &timeNew, float & timeOld, char * fold, float u[(NROWS+1)][(NCOLS+1)], float v[(NROWS+1)][(NCOLS+1)], float lonmin, float lonmax, float latmin, float latmax,float dx, float dy, int ip2, int jp2,int nx, int ny,float *vbBottom0,float *vbTop0,float *ubLeft0,float *ubRight0,float *vbBottom,float *vbTop,float *ubLeft,float *ubRight, float & lonMinInt,float & lonMaxInt,float & latMinInt, float &latMaxInt);

float getNextTime(float time0, char * inputDir, int * IDTime);
float getPreviousTime(float time0, char * inputDir, int * IDTime);

void getIndexOfvars(char * line,char * delimiter, int &colt,int &colH,int &colVx,int &colVy);
void getValueFromLine(char * line,char * delimiter,int col, char * value);

void CalculateTravelTime(float lon, float lat, float mag, float lonmin, float lonmax, float latmin, float latmax, char * outDir, char * barra, char * CurrentDir, char * p[60]);
void execPreHyflux(char * CurrentDir, char * barra, char * inputFile, char * outputDir);
void tar(char * CurrentDir, char * barra, char * p[60], char * outputDir, char * fileDef, char * fileOut);
void createListfiles(char * prefix,char * fnamenetcdf_H, char * fnamenetcdf_U, char * fnamenetcdf_V, char * fname);

void ReadInputFromNETCDF(int IDTime, char * fold, float h[(NROWS+1)][(NCOLS+1)], float lonmin, float lonmax, float latmin, float latmax,int ip2, int jp2,int &nx,int &ny);

void mergeRasters(float h[(NROWS+1)][(NCOLS+1)], float lonmin, float lonmax, float latmin, float latmax,int nx, int ny, double NoDL,
			 	  float hSmall[(NROWS+1)][(NCOLS+1)], float lonminS, float lonmaxS, float latminS, float latmaxS,int nxS, int nyS, double NoDS);


float getMultipleTime(float dtSec, float TmaxMin);

void establishArrivalTime(float time0, float h[(NROWS+1)][(NCOLS+1)],float h0[(NROWS+1)][(NCOLS+1)],float Tarrh[(NROWS+1)][(NCOLS+1)],float d[(NROWS+1)][(NCOLS+1)], float minDepthTT, float setpoint, int ip2, int jp2, float NODATA);

void setInitialHeightTarr(float h[(NROWS+1)][(NCOLS+1)],float h0[(NROWS+1)][(NCOLS+1)],float mh1[(NROWS+1)][(NCOLS+1)],float Tarrh[(NROWS+1)][(NCOLS+1)], float TarrMax[(NROWS+1)][(NCOLS+1)],int ip2, int jp2,float NDATA);
void gdalcontour(char * CurrentDir, char * barra, char * p[60], char * fileInp, char * fileOut, char * nameLayer, float stepH);
void shape2kml(char * CurrentDir, char * barra, char * p[60], char * fileInp, char * fileOut);

void gdaltranslate(char * CurrentDir, char * barra, char * p[60], char * fileInp, char * fileOut);
void combineFiles(char * file1, char * file2, char * file3);