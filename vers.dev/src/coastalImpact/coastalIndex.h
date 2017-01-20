
//void updateWPtimelines(placedef  place, float time0, float h[(NROWS+1)][(NCOLS+1)], char * outDir, int iiMaxWP);
inputCase assignInput();
//float CalculateDegree(float distancekm, float lat);
//bool checkDepthWP(int ix, int iy,float d[(NROWS+1)][(NCOLS+1)],float minDeptWP);
char * checkFolder(char *outDir);
//int checkWP0(int nplaces);
//int checkWP(int nplaces, placedef * place, int * indexPlace, int imax, float setpoint);
//void dumpResults(float t, char *outDir);
//
//void getID(float lon, float lat, float lonmin, float dxCell, float latmin, float dyCell, int * idx, int *idy);
//void getCoord(int idx, int idy, float lonmin,float dxCell, float  latmin, float dyCell , float * lon,float * lat);
//void results(float time0, char * outputDir, int nplaces, int ngauges, int ncols, int nrows, int mode, bool ft);
void interpretCalcInput(char * line);
void interpretConfigLine(char * line);
//
void readArguments(int argc, char *argv[], bool updateFiles);
//void parsequerystring(char * qs);
void readFile(char *inputFile, char * mode);
//int selectPlaces(char * fname, placedef *placeList, float lonmin, float lonmax, float latmin, float latmax,bool count);
//int selectGauges(char * fname, placedef *placeList, float lonmin, float lonmax, float latmin, float latmax,bool count);
//void  readGridFile(char * batfile, float q[(NROWS+1)][(NCOLS+1)], int ip2, int jp2);
//
//void getParameters(char * line,char * delimiter,char * p[60]);
char * CurrentFolder(char * argv[]);
//void initLatLon(float dx0, float dy0, int ncols, int nrows,double * x, double *y);

void computeCoastal(char * outputDir,  float lonmin, float lonmax, float latmin, float latmax,int ncols, int nrows);
void interpolateBathymetry(char * fnameBat, float lonmin, float lonmax, float latmin, float latmax, int ncols, int nrows);
void interpolateBathymetryGebco30(char * fnameBat, float lonmin, float lonmax, float latmin, float latmax, int ncols, int nrows, float d[(NROWS+1)][(NCOLS+1)]);