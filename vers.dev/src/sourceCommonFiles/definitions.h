
//#define WINDOWS
#define LINUX

#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <stdlib.h>
#include <time.h>
#include <locale.h>
#include <math.h>

//#ifdef LINUX
	typedef unsigned char boolean;
	#define false (0)
	#define true (1)
//#endif

//#ifdef WINDOWS
//#define _TM_DEFINED
//typedef struct tm
//{
//    int tm_sec;     /* seconds after the minute - [0,59] */
//    int tm_min;     /* minutes after the hour - [0,59] */
//    int tm_hour;    /* hours since midnight - [0,23] */
//    int tm_mday;    /* day of the month - [1,31] */
//    int tm_mon;     /* months since January - [0,11] */
//    int tm_year;    /* years since 1900 */
//    int tm_wday;    /* days since Sunday - [0,6] */
//    int tm_yday;    /* days since January 1 - [0,365] */
//    int tm_isdst;   /* daylight savings time flag */
//};
//#endif
typedef struct 
{
	char place[100];    /* name of the location */
	char country[100];  /*Country name */
	int ix;             /*bit in x direction*/
	int iy;
	float lat;
	float lon;
	float ArrivalTime; /* first time that h>0.01 m (s) */
	float MaxHeight;   
	int popEst;
	int cityClass;
	float TimeMaxHeight;   //time of Max Height (s)
	int   IDplace;
	float TimeLowerThan1m; //time of last time was higher than 1m
	float h1m;
} placedef;

typedef struct 
{
	char name[100];    /* name of the location, optional */
	char fname[200];  // filename where to write the file
	float lat;  //epicenter 
	float lon;
	float mag;
	float depth; //depth in km, top of the fault
	float fwidth,flenght; //width and lenght in km
	float slip, dip, rake, strike; // okada parameters
	float timeOfRupture; // default is 0 s
	float flenghtFactor,fwidthFactor;
	float ipodepth;
} sourcePar;

typedef struct
{
	char Title[100];
	char outPath[100];
	char URL_calc[100];
	char DateTsunamiString[32];
	double Lat;
	double Lon;
	float Mag;
	float InTime;
	float FinTime;
	float Tsave;
	float dtMax;
	float fanning;
	float watDep;
	int faultMode;
	int faultform;
	int compiler;
	int AutomaticFault;
	float flenght;
	float fwidth;
	float strike;
	float slip;
	float dip;
	float rake;
	float depth;
	float width;
	float batgrid;
	float lonmin;
	float lonmax;
	float latmin;
	float latmax;
	float time0;

} inputCase;
