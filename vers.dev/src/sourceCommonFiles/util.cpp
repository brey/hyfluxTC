//AA
#include "definitions.h"
#include "util.h"
#include "stringlib.h"
#include "gdal_int.h"
#include "netcdf_int.h"
#include <libgen.h>

void deleteFiles(char * outputDir, char * fileDef)
{ 
	char * fnameDummy=(char *) calloc (512,sizeof(char));

	#ifdef WINDOWS
		AddString_s(fnameDummy,"del \"",outputDir,"\\",fileDef,"\"");
		system(fnameDummy); // DOS 
	
	#endif
	#ifdef LINUX
		AddString_s(fnameDummy,"rm -rf ",outputDir,"/",fileDef,NULL);
		system(fnameDummy); // unix
	#endif

		delete [] fnameDummy;
		fnameDummy=NULL;
}

void combineFiles(char * file1, char * file2, char * file3)
{ 
	char * cmd=(char *) calloc (512,sizeof(char));

	#ifdef WINDOWS
		
	
	#endif
	#ifdef LINUX
		AddString_s(cmd,"cat ",file1,"  ",file2," >");
		AddString_s(cmd,cmd,file3,NULL,NULL,NULL);
		printf("%s\n",cmd);
		system(cmd); // DOS 
	#endif
    
		delete [] cmd;
		cmd=NULL;
}


void copyFile(char * DirInp, char * fileInp, char * DirOut, char * fileOut, char * barra)
{

	char * fnameDummy	=(char *) calloc (512,sizeof(char));
	char * fnameDummy1	=(char *) calloc (512,sizeof(char));
	char * command		=(char *) calloc (512,sizeof(char));
	//   ?procdir ?
#ifdef WINDOWS
	AddString_s(fnameDummy,"copy \"",DirInp,barra,fileInp,"\" \"");
#else
	AddString_s(fnameDummy,"cp -u \"",DirInp,barra,fileInp,"\" \"");
#endif
	AddString_s(fnameDummy1,fnameDummy,DirOut,barra,fileOut,"\"");
	
	printf("%s\n",fnameDummy1);
	system(fnameDummy1); // unix

}

void execPreHyflux(char * CurrentDir, char * barra, char * inputFile, char * outputDir)
{
	char * fnameDummy=(char *) calloc (512,sizeof(char));
	char * fnameDummy1=(char *) calloc (512,sizeof(char));
	char * command=(char *) calloc (512,sizeof(char));
	#ifdef WINDOWS
		// AddString_s(fnameDummy1,CurrentDir,"\\..\\tar\\tar.exe -cf",NULL,NULL,NULL);
	
		// //solveDir(fnameDummy,barra,n,"..",CurrentDir,p,command);

		// getFilename("tmptar","bat",-1,outputDir,fnameDummy);

		// FILE *outfile;
	
		// /* Open the file.  If NULL is returned there was an error */
		// outfile = fopen(fnameDummy, "w");
		// char drive[2];
		// cmid(outputDir,1,1,drive);
		// fprintf(outfile,"%s\:\n",drive);
		// fprintf(outfile,"cd %s\n",outputDir);
		// fprintf(outfile,"%s %s %s\n",fnameDummy1,fileOut,fileDef);
		// fclose(outfile);

		// system(fnameDummy); // DOS 
	
	#endif
	#ifdef LINUX
		printf("inputFile =%s\n");
		AddString_s(fnameDummy,CurrentDir,barra,"pre_HyFlux2 -i ",inputFile,NULL);
		AddString_s(fnameDummy1,fnameDummy," -o ",outputDir," -w ",outputDir);
		AddString_s(fnameDummy,fnameDummy1,barra,"work",NULL,NULL);

		printf("%s\n",fnameDummy);
		system(fnameDummy); // unix
	#endif
	
}

void tar(char * CurrentDir, char * barra, char * p[60], char * outputDir, char * fileDef, char * fileOut)
{ 
	char * fnameDummy=(char *) calloc (512,sizeof(char));
	char * fnameDummy1=(char *) calloc (512,sizeof(char));
	char * command=(char *) calloc (512,sizeof(char));
    int n;
	#ifdef WINDOWS
		AddString_s(fnameDummy1,CurrentDir,"\\..\\tar\\tar.exe -cf",NULL,NULL,NULL);
	
		//solveDir(fnameDummy,barra,n,"..",CurrentDir,p,command);

		getFilename("tmptar","bat",-1,outputDir,fnameDummy);

		FILE *outfile;
	
		/* Open the file.  If NULL is returned there was an error */
		outfile = fopen(fnameDummy, "w");
		char drive[2];
		cmid(outputDir,1,1,drive);
		fprintf(outfile,"%s\:\n",drive);
		fprintf(outfile,"cd %s\n",outputDir);
		fprintf(outfile,"%s %s %s\n",fnameDummy1,fileOut,fileDef);
		fclose(outfile);

		system(fnameDummy); // DOS 
		remove(fnameDummy);	
	#endif
	#ifdef LINUX
		AddString_s(fnameDummy,"tar -cf ", fileOut, " ",NULL,NULL);
		AddString_s(fnameDummy1,fnameDummy,fileDef,NULL,NULL,NULL);
		printf("%s\n",fnameDummy1);
		system(fnameDummy1); // unix
	#endif
}

void gdaltranslate(char * CurrentDir, char * barra, char * p[60], char * fileInp, char * fileOut)
{ 
	char * fnameDummy=(char *) calloc (512,sizeof(char));
	char * fnameDummy1=(char *) calloc (512,sizeof(char));
	char * command=(char *) calloc (512,sizeof(char));
    int n;
	#ifdef WINDOWS
		AddString_s(fnameDummy1,CurrentDir,"\\..\\gdalbin\\gdal_translate.exe ",fileInp," ",fileOut);
	
		solveDir(fnameDummy1,barra,n,"..",CurrentDir,p,command);
		system(command); // DOS 
		
	#endif
	#ifdef LINUX
		AddString_s(fnameDummy,"gdal_translate ", fileInp," ",fileOut," " );
		printf("%s\n",fnameDummy);
		system(fnameDummy); // unix
	#endif
}


void gdalcontour(char * CurrentDir, char * barra, char * p[60], char * fileInp, char * fileOut, char * nameLayer, float stepH)
{ 
	char * fnameDummy=(char *) calloc (512,sizeof(char));
	char * fnameDummy1=(char *) calloc (512,sizeof(char));
	char * stepHS=(char *) calloc (512,sizeof(char));
	char * command=(char *) calloc (512,sizeof(char));
    int n;
	sprintf(stepHS,"%f",stepH);
	#ifdef WINDOWS
		AddString_s(fnameDummy1,CurrentDir,"\\..\\gdalbin\\gdal_contour.exe -a ", nameLayer, " -nln ", nameLayer);
		AddString_s(fnameDummy,fnameDummy1," -i ",stepHS," \"",fileInp);
		AddString_s(fnameDummy1,fnameDummy,	"\" \"",fileOut,"\"",NULL);
	
		solveDir(fnameDummy1,barra,n,"..",CurrentDir,p,command);
		printf("%s\n",command);
		system(command); // DOS 
		
	#endif
	#ifdef LINUX
		AddString_s(fnameDummy,"gdal_contour.exe -a " , nameLayer, " -nln ", nameLayer,NULL);
		AddString_s(fnameDummy,fnameDummy1," -i ",stepHS," \"",fileInp);
		AddString_s(fnameDummy1,fnameDummy,	"\" \"",fileOut,"\"",NULL);

		AddString_s(fnameDummy1,"gdal_contour -a Tarrival -nln Tarrival -i ",stepHS," \"",fileInp,NULL);
		AddString_s(fnameDummy,fnameDummy1,"\" \"",fileOut,"\"",NULL);

		printf("%s\n",fnameDummy);
		system(fnameDummy); // unix
		printf("launched command for countour\n");
	#endif
}

void shape2kml(char * CurrentDir, char * barra, char * p[60], char * fileInp, char * fileOut)
{ 
	char * fnameDummy=(char *) calloc (512,sizeof(char));
	char * fnameDummy1=(char *) calloc (512,sizeof(char));
	char * stepHS=(char *) calloc (512,sizeof(char));
	char * command=(char *) calloc (512,sizeof(char));
    int n;
	#ifdef WINDOWS
		AddString_s(fnameDummy1,CurrentDir,"\\ogr2ogr -f KML ",  fileOut, " ", fileInp);
		solveDir(fnameDummy1,barra,n,"..",CurrentDir,p,command);
		printf("%s\n",command);
		system(command); // DOS 
		
	#endif
	#ifdef LINUX
		AddString_s(fnameDummy,"ogr2ogr -f KML " , fileOut, " ", fileInp,NULL);
		printf("%s\n",fnameDummy);
		system(fnameDummy); // unix
	#endif
}

void ApplyTimeLabel(char * barra, char * CurrentDir, char *outputDir, char * p[60],float time0, char * fname)
{
	char * command=(char *) calloc(512,sizeof(char));

	int hh,mm;
	hh=(int)time0/3600;
	mm=(int)((time0-hh*3600)/60);

	char * timeImage=(char*) calloc(512,sizeof(char*));
	getFilename("timeimage","png",-1,outputDir,timeImage);

	char * fontName=(char*) calloc(512,sizeof(char*));
	char * UpDir=(char*) calloc(512,sizeof(char*));
	copystring(CurrentDir,UpDir);
	sprintf(fontName,"%s%sDATA%sLucidaBrightRegular.ttf",dirname(UpDir),barra,barra);

	#ifdef WINDOWS
		sprintf(command,"%s%sconvert -background navy -fill yellow  -pointsize 30 label:\" Time=%02u:%02u \"  \"%s\" ",CurrentDir,barra, hh,mm,timeImage);
	#else
		sprintf(command,"%s%sconvert -size 190x40 xc:navy  -font %s  -pointsize 30 -fill yellow  -draw \"text 5,30 'Time=%02u:%02u'\"  %s ",CurrentDir,barra,fontName, hh,mm,timeImage);
	#endif
	//printf("command=%s\n",command); 
	system(command);

	sprintf(command,"%s%sconvert \"%s\" -compose over -gravity NorthEast \"%s\" -composite \"%s\" ",CurrentDir,barra, fname,timeImage,fname);
	//printf("command=%s\n",command); 
	system(command);

	remove(timeImage);

	delete []command;
	command=NULL;
	}

void ApplyLogo(char * barra, char * CurrentDir, char * p[60], char * fname)
{
	char * 	command=(char *) calloc(1000,sizeof(char));
	char * logo=(char *) calloc(1000, sizeof(char));
	char * UpDir=(char*) calloc(512,sizeof(char*));
	copystring(CurrentDir,UpDir);
	sprintf(logo,"%s%sDATA%sJRClogo.gif",dirname(UpDir),barra,barra);
	
	sprintf(command,"%s%sconvert  \"%s\" -compose over -gravity SouthWest \"%s\" -composite \"%s\" ",CurrentDir,barra,fname,logo,fname);
	//printf("command=%s\n",command); 
	system(command);

	//printf(" apply logo command  %s\n%i\n",command, sl(command));
	
	delete [] command, logo;
	command=NULL; logo=NULL;
}

void ApplyLegend(char * barra, char * CurrentDir,char * p[60], char * fname)
{
	char * command=(char *) calloc(1000,sizeof(char));
	char * legend=(char *) calloc(1000,sizeof(char));
	char * UpDir=(char*) calloc(512,sizeof(char*));
	copystring(CurrentDir,UpDir);

	sprintf(legend,"%s%sDATA%slegend.bmp",dirname(UpDir),barra,barra);
	
	sprintf(command,"%s%sconvert  \"%s\" -compose over -gravity NorthEast -geometry +5+55 \"%s\" -composite \"%s\" ",CurrentDir,barra,fname,legend,fname);
	//printf("command=%s\n",command); 
	system(command);
		
	delete []command, legend;
	command=NULL; legend=NULL;
}

void convertImage(char * barra, char * CurrentDir,char * p[60], char * fnameSrc, char * fnameDest, bool deleteSrc, char * options)
{
	char * command=(char *) calloc(512, sizeof(char));
	
	#ifdef WINDOWS
	
		if(options==NULL) 
			sprintf(command,"%s%sconvert.exe \"%s\" \"%s\" ",CurrentDir,barra,fnameSrc,fnameDest);
		else
			sprintf(command,"%s%sconvert.exe  %s \"%s\" \"%s\" ",CurrentDir,barra,options,fnameSrc,fnameDest);
	#else
		
		if (options==NULL)
			sprintf(command,"%s%sconvert \"%s\" \"%s\" ",CurrentDir,barra,fnameSrc,fnameDest);
		else
			sprintf(command,"%s%sconvert %s \"%s\" \"%s\" ",CurrentDir,barra,options,fnameSrc,fnameDest);
		
	#endif
	//printf("command=%s\n",command); 
	system(command);

	if (deleteSrc)
		remove(fnameSrc);
	delete []command;
	command=NULL;

}


void createNetcdf(char * CurrentDir, char *in, char *out, char * opt)
{
	char * dummy=(char*) calloc(512,sizeof(char*));
	char * command=(char*) calloc(512,sizeof(char*));
	AddString_s(dummy, CurrentDir,"\\..\\tif2netcdf\\tif2netcdf.exe -in \"",in, "\" -out \"",out);
	AddString_s(command,dummy,"\""," -opt ", opt,NULL);
	system(command);
	delete []command,dummy;
	command=NULL;dummy=NULL;

}

void checkDir(char *Dir)
{
	if (Dir[strlen(Dir)-1]=='\\')
	Dir[strlen(Dir)-1]=0;
}
void CreateRSS(char * fname, placedef placeList[],int * indexPlaceList, int indexMax, char * outDir, inputCase inp, struct tm DateTsunami, sourcePar * faults, int Nfaults)
{
	//int ix0,iy0,ix1,iy1,kx,ky;
	//float dep,hei,ahei;

	char fullname[]="                                                                                                                " ;
	getFilename(fname,"xml",-1,outDir,fullname);

	FILE *outfile;
	
	/* Open the file.  If NULL is returned there was an error */
	//printf("Printing locations in %s\n",fullname);
	outfile = fopen(fullname, "w");
	fprintf(outfile,"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n");
	fprintf(outfile,"<rss version=\"0.91\" xmlns:geo=\"http://www.w3.org/2003/01/geo/wgs84_pos#\">\n");
	fprintf(outfile,"<channel>\n");
	fprintf(outfile,"   <title>TSUNAMI</title>\n");
	//  print epicenter ....  TODO
	writeEpicenter(outfile,inp,faults, Nfaults);
	int k,i;
	for ( i=0; i<=indexMax;i++)
	{
		k=indexPlaceList[i];
		writeItem(outfile,placeList[k], DateTsunami);
	}
	fprintf(outfile,"</channel>\n");
	fprintf(outfile,"</rss>\n");
	fclose(outfile);
}

void solveDir(char * stringa , char * delimiter, int &n, char * searchCar,char * CurrentDir,char * p[60],  char * stringaOut1)
{
	/*char * p[10];
	//for (n=0;n<=10;n++)
	//  p[n]=(char *)calloc(512,sizeof(char));*/
	char * label=(char *)calloc(512,sizeof(char));
	char * stringaOut0=(char*) calloc(512,sizeof(char));
	char * stringaOut=(char*) calloc(512,sizeof(char));
	char * st=(char *)calloc(512,sizeof(char));
	n=-1;
	if(sl(stringa)>500)
		printf("string too long");

	copystring(stringa,st);
	label=strtok(st,delimiter);
	while (label!=NULL)
	{
		n++;
		label=cleanValue1(label);
	
		copystring(label,p[n]);
		p[n][sl(label)]=0;
		label=strtok(NULL,delimiter);
	}
	int i;
    //printf("n  %i\n",n);
	for (i=0;i<=n;i++)
	{
		if (i<n)
		if(compString(p[i+1],searchCar)) i=i+2;

		#ifdef WINDOWS
		if(sl(stringaOut)==0)
			copystring(p[i],stringaOut);
		else
			sprintf(stringaOut+sl(stringaOut),"%s%s",delimiter,p[i]);
		#else
			sprintf(stringaOut+sl(stringaOut),"%s%s",delimiter,p[i]);
		#endif
	}
	
	copystring(stringaOut,stringaOut1);
	delete [] stringaOut0,label,st;
	st=NULL;stringaOut0=NULL;label=NULL;
}

void CreateTxt(char * fname,placedef placeList[],int * indexPlaceList, int indexMax, char * outDir, inputCase inp, struct tm DateTsunami)
{
	//int ix0,iy0,ix1,iy1,kx,ky;
	//float dep,hei,ahei;

	char fullname[]="                                                                                                                " ;
	getFilename(fname,"txt",-1,outDir,fullname);

	FILE *outfile;
	
	/* Open the file.  If NULL is returned there was an error */
	//printf("Printing locations in %s\n",fullname);
	outfile = fopen(fullname, "w");
	fprintf(outfile,"* ID\tcountry\tplace\tMaxHei\tArrTime\ttMaxHei\tArrivalTS\tlon\tlat\tpopest\tcityclass\tTh1m\th1m\n");
	int k,i;
	for ( i=0; i<=indexMax;i++)
	{
		k=indexPlaceList[i];
		writeItemTxt(outfile,placeList[k], DateTsunami);
	}
	fclose(outfile);
}

void writeItemTxt(FILE * outfile, placedef place, struct tm DateTsunami)
{	
	int hh,mm;
	char DateArrivalS[32]="                               ";
	if (place.ArrivalTime !=99999)
	{
	//printf("DateTsunami %d %b %Y %H:%M:%S\n", DateTsunami); 
	struct tm Arrival=AddDate(DateTsunami,(int) place.ArrivalTime);
	//DateArrivalS=DateToString(Arrival);

	strftime(DateArrivalS, sizeof(DateArrivalS), "%d %b %Y %H:%M:%S", &Arrival);
	}
	char tabCar=9;
	
	//fprintf(outfile,"* country\tplace\tMaxHei\tArrTime\ttMaxHei\tArrivalTS\tlon\tlat\n");
	fprintf(outfile,"%i\t",place.IDplace);
	fprintf(outfile,"%s\t%s\t%3.2f\t",place.country,place.place ,place.MaxHeight );
	if (place.ArrivalTime<1e9)
	{
	hh=(int) place.ArrivalTime /3600;
	mm=(int) (place.ArrivalTime-hh*3600)/60;
	fprintf(outfile, "%02u:%02u\t", hh,mm);
	}
	else
		fprintf(outfile, "na\t");
	//fprintf(outfile, "%f\t", place.MaxHeight); 

	if (place.TimeMaxHeight<1e9)
	{
	hh=(int) place.TimeMaxHeight /3600;
	mm=(int) (place.TimeMaxHeight -hh*3600)/60;
	fprintf(outfile, "%02u:%02u\t", hh,mm);
	fprintf(outfile, "%s\t",DateArrivalS);
	}
	else
		fprintf(outfile, "na\tna\t");
	fprintf(outfile, "%f\t",place.lon );
	fprintf(outfile, "%f\t",place.lat );
	fprintf(outfile, "%i\t",place.popEst );
	fprintf(outfile, "%i\t",place.cityClass );
	hh=(int) place.TimeLowerThan1m /3600;
	mm=(int) (place.TimeLowerThan1m -hh*3600)/60;
	fprintf(outfile, "%02u:%02u\t", hh,mm);
	fprintf(outfile, "%f\t\n",place.h1m );
}


void solveDir1(char * stringaOut1,char * stringa , char * delimiter, int &n, char * searchCar,char * CurrentDir,char * p[60])
{
	/*char * p[10];
	for (n=0;n<=10;n++)
	  p[n]=(char *)calloc(512,sizeof(char));*/
	char * label=(char *)calloc(512,sizeof(char));
	char * stringaOut0=(char*) calloc(512,sizeof(char));
	char * stringaOut=(char*) calloc(512,sizeof(char));
	char * st=(char *)calloc(512,sizeof(char));
	n=-1;
	if(sl(stringa)>500)
		printf("string too long");

	printf("1\n");
	copystring(stringa,st);
	printf("2\n");
	label=strtok(st,delimiter);
	while (label!=NULL)
	{
		n++;
		label=cleanValue1(label);
	    printf("3 %i\n",n);
		copystring(label,p[n]);
		p[n][sl(label)]=0;
		printf("4  p(%i) %s\n",n,p[n]);
		label=strtok(NULL,delimiter);
	}
	int i;
    printf("n  %i\n",n);
	for (i=0;i<=n;i++)
	{
		if (i<n)
		if(compString(p[i+1],searchCar)) i=i+2;
		//if(sl(stringaOut)==0)
		//	copystring(p[i],stringaOut);
		//else
		//{
			sprintf(stringaOut+sl(stringaOut),"%s%s",delimiter,p[i]);
			printf("%s\n\n",stringaOut);
		//}

	}
	
	printf("stringaOut  %s\n",stringaOut);
	copystring(stringaOut,stringaOut1);
	printf("stringaOut1  %s\n",stringaOut1);
	/*
#ifdef LINUX

	if (CurrentDir[0]==delimiter[0] && stringaOut[0]!=delimiter[0])
	{
		stringaOut0[0]=0;
		AddString_s(stringaOut0,delimiter,stringaOut,NULL,NULL,NULL);
	    copystring(stringaOut0,stringaOut);
	}
#endif
*/
free(label);
free(st);
free(stringaOut0);
delete [] st,stringaOut0,label;
st=NULL;stringaOut0=NULL;label=NULL;

}

void split(char * stringa , char * delimiter, int &n, char * searchCar, char **p)
{
	// str=abct,wwer
	// char * p[10]
	// p=split(str,",")
	// p[0]=abct,  p[1]=wwer
	//
	char * label;
	//char * p[100];
	char * st=(char *)calloc(strlen(stringa),sizeof(char));
	n=-1;
	copystring(stringa,st);
	label=strtok(st,delimiter);
	while (label!=NULL)
	{
		n++;
		p[n]=(char *)calloc(strlen(label),sizeof(char));
		copystring(label,p[n]);
		label=strtok(NULL,delimiter);
	}
	delete [] st,label;
	st=NULL;label=NULL;
}

struct tm  StringaToDate(char *dateString0)
{
	
	struct tm tmp;
	
	int dd,MMM,yyyy,hh,mm,ss;
	char * hour="       ";
	char dateString[32];
	copystring(dateString0,dateString);
	// form is  dd MMM yyyy hh:mm:ss

	dd=atoi(ctrim(strtok(dateString," ")));
	MMM=month(ctrim(strtok(NULL," ")));
	yyyy=atoi(ctrim(strtok(NULL," ")));
	hour=ctrim(strtok(NULL," "));
		
	hh=atoi(ctrim(strtok(hour,":")));
	mm=atoi(ctrim(strtok(NULL,":")));
	if (strtok(NULL,":") !=NULL)
	{
		copystring(dateString0,dateString);
		dd=atoi(ctrim(strtok(dateString," ")));
		MMM=month(ctrim(strtok(NULL," ")));
		yyyy=atoi(ctrim(strtok(NULL," ")));
		hour=ctrim(strtok(NULL," "));
		hh=atoi(ctrim(strtok(hour,":")));
		mm=atoi(ctrim(strtok(NULL,":")));
		ss=atoi(ctrim(strtok(NULL,":")));
	}
	else
		ss=0;

	tmp.tm_hour=hh;
	tmp.tm_sec =ss;
	tmp.tm_min=mm;
	tmp.tm_mday =dd;
	tmp.tm_mon =MMM-1;  //  months since January (0-11)
	tmp.tm_year =yyyy-1900;
	tmp.tm_isdst = 0;
    return tmp;
	//time_t tmp1=StringToDate(dateString);
}

 
void DateToString(struct tm * date0, char * ts)
{
	//char ts[32]="                               ";
	strftime(ts, sizeof(ts), "%d %b %Y %H:%M:%S", date0);
}

int month(char * a)
{
	int mon=-1;
	if (strcmp(a,"Jan")==0) mon=1;
	if (strcmp(a,"Feb")==0) mon=2;
	if (strcmp(a,"Mar")==0) mon=3;
	if (strcmp(a,"Apr")==0) mon=4;
	if (strcmp(a,"May")==0) mon=5;
	if (strcmp(a,"Jun")==0) mon=6;
	if (strcmp(a,"Jul")==0) mon=7;
	if (strcmp(a,"Aug")==0) mon=8;
	if (strcmp(a,"Sep")==0) mon=9;
	if (strcmp(a,"Oct")==0) mon=10;
	if (strcmp(a,"Nov")==0) mon=11;
	if (strcmp(a,"Dec")==0) mon=12;
	return mon;
}

void writeEpicenter(FILE * outfile, inputCase inp, sourcePar * faults, int Nfaults)
{   int k;
   fprintf(outfile,"<item>\n");
   fprintf(outfile,"<title><![CDATA[%s]]></title>\n",inp.Title);
   fprintf(outfile,"<image>%s/P1_MAXHEIGHT_END.jpg</image>\n",inp.outPath );
   fprintf(outfile,"<location></location>\n");
   fprintf(outfile,"<depth></depth>\n");
   fprintf(outfile,"<dc:subject xmlns:dc=\"dc\">EPICENTER</dc:subject>\n");
   fprintf(outfile,"<pubDate>%s</pubDate>\n",inp.DateTsunamiString);
   fprintf(outfile,"<link></link>\n");
   fprintf(outfile,"<initialConditions>\n");
//float Mag,InTime,FinTime,Tsave,dtMax,fanning;
//int faultMode, faultform;
//int compiler;
//bool AutomaticFault;
//
//float flenght, fwidth, strike, slip, dip, rake, depth;
//float width,batgrid,lonmin,lonmax,latmin,latmax;
   float latAvg,lonAvg,totMag,magFault;
   latAvg=0;lonAvg=0;totMag=0;
   for (k=1;k<=Nfaults;k++)
	{
		lonAvg += faults[k].lon/(Nfaults);
		latAvg += faults[k].lat/(Nfaults);
		magFault=faults[k].fwidth*1000.0*100.0*faults[k].flenght *1000.0*100.0*faults[k].slip*100.0 *400000000000.0;
		totMag += magFault;
		if (magFault !=0.)
			magFault = 2./3.*log10(magFault)-10.7;
		else
			magFault=0.;
		fprintf(outfile,"    <Fault Nf=\"%i\">\n",k);
		fprintf(outfile,"       <Lenght>%4.0f</Lenght>\n",faults[k].flenght);
		fprintf(outfile,"	    <Width>%4.0f</Width>\n",faults[k].fwidth);
		fprintf(outfile,"       <Height>%4.2f</Height>\n",faults[k].slip);
		fprintf(outfile,"       <Form>%i</Form>\n",inp.faultform);
		fprintf(outfile,"       <faultMagnitude>%f</faultMagnitude>\n",magFault);
		fprintf(outfile,"       <faultLongitude>%f</faultLongitude>\n",faults[k].lon);
		fprintf(outfile,"       <faultLatitude>%f</faultLatitude>\n",faults[k].lat);
	
		fprintf(outfile,"       <Mode>%i</Mode>\n",inp.faultMode);
		float angle=Mod((450 - faults[k].strike) , 360);
		fprintf(outfile,"       <Angle type=\"trig angle: north=90, south=-90\">%4.1f</Angle>\n",angle);
		fprintf(outfile,"       <okadaPar>\n");
		fprintf(outfile,"          <strike type=\"geogr angle: north=0, south=180\">%4.1f</strike>\n",faults[k].strike);
		fprintf(outfile,"          <dip>%4.1f</dip>\n",faults[k].dip);
		fprintf(outfile,"          <rake>%4.1f</rake>\n",faults[k].rake);
		if (faults[k].depth==0 && faults[k].ipodepth != 0)
			fprintf(outfile,"          <depth  depthtype=\"middle of the fault\">%4.1f</depth>\n",faults[k].ipodepth);
		else
			fprintf(outfile,"          <depth  depthtype=\"top of the fault\">%4.1f</depth>\n",faults[k].depth);
		fprintf(outfile,"          </okadaPar>\n");
		fprintf(outfile,"   </Fault>\n");
	}
   totMag=2./3.*log10(totMag)-10.7;
 
	fprintf(outfile,"   <Bathym>%f</Bathym>\n",inp.batgrid);
    fprintf(outfile,"   <window>\n");
    fprintf(outfile,"       <lonmin>%f</lonmin>\n",inp.lonmin);
    fprintf(outfile,"       <lonmax>%f</lonmax>\n",inp.lonmax);
    fprintf(outfile,"       <latmin>%f</latmin>\n",inp.latmin);
    fprintf(outfile,"       <latmax>%f</latmax>\n",inp.latmax);
    fprintf(outfile,"   </window>\n");
    fprintf(outfile,"   <dtMax>%f</dtMax>\n",inp.dtMax);
    fprintf(outfile,"</initialConditions>\n");
   fprintf(outfile,"<geo:lat>%5.1f</geo:lat>\n",latAvg);
   fprintf(outfile,"<geo:long>%5.1f</geo:long>\n",lonAvg);
   fprintf(outfile,"<Magnitude>%2.1f</Magnitude>\n",totMag);	
   fprintf(outfile,"</item>\n");
   fprintf(outfile,"<description> Longitude=%5.1f&lt;BR&gt;\n",lonAvg);
   fprintf(outfile,"Latitude =%5.1f&lt;BR&gt;\n",latAvg);
   fprintf(outfile,"Magnitude =%4.2f&lt;BR&gt;\n",totMag);
   fprintf(outfile,"Water Depth    =%4.0f&lt;BR&gt;\n",inp.watDep);
   fprintf(outfile,"</description>\n");

}
void writeItem(FILE * outfile, placedef place, struct tm DateTsunami)
{	
	int hh,mm;
	char DateArrivalS[32]="                               ";
	if (place.ArrivalTime !=99999)
	{
	struct tm Arrival=AddDate(DateTsunami,(int) place.ArrivalTime);
	//DateArrivalS=DateToString(Arrival);
	
	strftime(DateArrivalS, sizeof(DateArrivalS), "%d %b %Y %H:%M:%S", &Arrival);
	}
	fprintf(outfile,"<item>\n");
	fprintf(outfile,"<title><![CDATA[%s: %s (%3.2f m)]]></title>\n",place.country,place.place ,place.MaxHeight );
	fprintf(outfile,"<description><![CDATA[");
	fprintf(outfile, "Country: %s\n", place.country);
	fprintf(outfile, "Location: %s\n", place.place);
	if (place.ArrivalTime<1e9)
	{
		hh=(int) place.ArrivalTime /3600;
		mm=(int) (place.ArrivalTime-hh*3600)/60;
		fprintf(outfile, "Time (hh:mm): %02u:%02u\n", hh,mm);
	}
	fprintf(outfile, "Maximum Height: %f m\n", place.MaxHeight); 
	if (place.TimeMaxHeight<1e9)
	{
		hh=(int) place.TimeMaxHeight /3600;
		mm=(int) (place.TimeMaxHeight -hh*3600)/60;
		fprintf(outfile, "Time Max (hh:mm): %02u:%02u\n", hh,mm);
	}
	fprintf(outfile, "]]></description>\n");	
	fprintf(outfile, "<pubDate>%s</pubDate>\n",DateArrivalS);
	fprintf(outfile, "<cityName>%s</cityName>\n", place.place);
	fprintf(outfile, "<country>%s</country>\n",place.country );
	fprintf(outfile, "<maxHeight>%f</maxHeight>\n",place.MaxHeight );
	fprintf(outfile, "<ID>%i</ID>\n",place.IDplace );

	if (place.TimeMaxHeight<1e9)
	{
	hh=(int) place.TimeMaxHeight/3600. ;
	mm=(int) (place.TimeMaxHeight -hh*3600)/60;
	fprintf(outfile, "<timeMaxH>%02u:%02u</timeMaxH>\n",hh,mm);
	fprintf(outfile, "<timeMaxH_value>%f</timeMaxH_value>\n",place.TimeMaxHeight/3600.);
	}
	else
	{
	fprintf(outfile, "<timeMaxH></timeMaxH>\n",hh,mm);
	fprintf(outfile, "<timeMaxH_value>-1</timeMaxH_value>\n",place.TimeMaxHeight/3600.);
	}	
	if (place.ArrivalTime<1e9)
	{
		hh=(int) place.ArrivalTime /3600;
		mm=(int) (place.ArrivalTime -hh*3600)/60;
		fprintf(outfile, "<timeArrival>%02u:%02u</timeArrival>\n",hh,mm);
		fprintf(outfile, "<timeArrival_value>%f</timeArrival_value>\n",place.ArrivalTime/3600.);
	}
	else
	{
		fprintf(outfile, "<timeArrival></timeArrival>\n",hh,mm);
		fprintf(outfile, "<timeArrival_value>-1</timeArrival_value>\n",place.ArrivalTime/3600.);

	}	
		fprintf(outfile, "<cityClass>%i</cityClass>\n",place.cityClass);  //todo
	fprintf(outfile, "<popEst>%i</popEst>\n",place.popEst);
	fprintf(outfile, "<geo:long>%f</geo:long>\n",place.lon );
	fprintf(outfile, "<geo:lat>%f</geo:lat>\n",place.lat );
	fprintf(outfile, "<dc:subject xmlns:dc=\"dc\">%i</dc:subject>\n",(int) place.MaxHeight );
	fprintf(outfile, "<cputime>9</cputime>\n"); //todo
	fprintf(outfile, "</item>\n");
}

float Mod(float a, float module)
{
	float sign;
	int n;
	if(a>0) sign=1;
	else sign=-1;
	float aa;
	aa=fabs(a);
	n=(int) (aa/module);
	aa=aa-n*module;
	return sign*a;
}


void Createkml(placedef * places,int * indexPlaceList, int indexMax, float time0, char * outDir, inputCase inp, struct tm DateTsunami)
{
	char fullname[]="                                                                                                                " ;
	getFilename("locations","kml",-1,outDir,fullname);

	FILE *outfile;
	
	/* Open the file.  If NULL is returned there was an error */
	//printf("Printing locations in %s\n",fullname);
	outfile = fopen(fullname, "w");

	fprintf(outfile,"<?xml version=\"1.0\" encoding=\"Windows-1252\" ?><kml>\n");
    fprintf(outfile,"<Folder>  <name>Case %s</name>  <open>1</open>\n",outDir);
	fprintf(outfile,"<Placemark>\n");
    fprintf(outfile,"  <name><![CDATA[%s (Epicenter)]]></name>\n",inp.Title);
    fprintf(outfile,"  <description><![CDATA[Longitude=%5.2f<BR><br /> Latitude =%5.2f<BR><br /> Magnitude =%4.2f<BR><br /> Water Depth    =%4.0f<BR>]]><br/></description>\n",inp.Lon,inp.Lat,inp.Mag,inp.watDep );
	fprintf(outfile,"  <styleUrl>root://styleMaps#default+nicon=0x304+hicon=0x314</styleUrl>\n");
    fprintf(outfile,"  <Style>\n");
    fprintf(outfile,"    <IconStyle>\n");
    fprintf(outfile,"      <color></color>\n");
    fprintf(outfile,"      <Icon>\n");
	fprintf(outfile,"      <href>http://maps.google.com/mapfiles/kml/pal3/icon46.png</href>        </Icon>\n");
    fprintf(outfile,"      <hotSpot x=\"32\" y=\"1\" xunits=\"pixels\" yunits=\"pixels\"/>\n");
    fprintf(outfile,"    </IconStyle>\n");
    fprintf(outfile,"  </Style>\n");
    fprintf(outfile,"  <Point>\n");
    fprintf(outfile,"     <coordinates>%f,%f</coordinates>\n",inp.Lon,inp.Lat );
    fprintf(outfile,"  </Point>\n");
	fprintf(outfile,"</Placemark>\n");

	AddImageOverlay(outfile,inp);
	
	int i,k,hh,mm,hh1,mm1;
	char colore[9];
	for (k=0;k<=indexMax;k++)
	{
		i=indexPlaceList[k];
		struct tm Arrival=AddDate(DateTsunami,(int) places[i].ArrivalTime);
		//DateArrivalS=DateToString(Arrival);
		char DateArrivalS[32]="                               ";
		strftime(DateArrivalS, sizeof(DateArrivalS), "%d %b %Y %H:%M:%S", &Arrival);

		
		fprintf(outfile,"<Placemark>\n");
	
		fprintf(outfile,"	<name><![CDATA[%s (%4.2f m)]]></name>\n",places[i].place,places[i].MaxHeight);
		hh1=(int)  places[i].TimeMaxHeight /3600;
		mm1=(int) (places[i].TimeMaxHeight -hh1*3600)/60;

		hh=(int) places[i].ArrivalTime /3600;
		mm=(int)(places[i].ArrivalTime -hh*3600)/60;

		fprintf(outfile,"	<description><![CDATA[<br />Country: %s<br />Location: %s<br />Arrival Time (hh:mm): %02u:%02u<br />Time of Maximum (hh:mm): %02u:%02u<br />Actual time: %s<br />Maximum Height: %f m<br />]]><br/></description>\n",places[i].country ,places[i].place,hh,mm,hh1,mm1,DateArrivalS,places[i].MaxHeight );
		//fprintf(outfile,"	<styleUrl>root://styleMaps#default+nicon=0x304+hicon=0x314</styleUrl>\n");
		fprintf(outfile,"	  <Style>\n");
		fprintf(outfile,"        <IconStyle>\n");
		colorIcon(places[i].MaxHeight,colore);
		fprintf(outfile,"          <color>%s</color>\n",colore );
		fprintf(outfile,"          <scale>1.0</scale>\n");
		//printf("%s\n",colore);
		fprintf(outfile,"          <Icon>\n");
		fprintf(outfile,"             <href>root://icons/palette-4.png</href>\n");
		fprintf(outfile,"                     <y>160</y><w>32</w><h>32</h>");
		fprintf(outfile,"          </Icon>\n");
		fprintf(outfile,"          <hotSpot x=\"32\" y=\"1\" xunits=\"pixels\" yunits=\"pixels\"/>\n");
		fprintf(outfile,"       </IconStyle>\n");
		fprintf(outfile,"    </Style>\n");
		fprintf(outfile,"    <Point>\n");
		fprintf(outfile,"        <coordinates>%f,%f,0</coordinates>\n",places[i].lon,places[i].lat);
		fprintf(outfile,"    </Point>\n");
		fprintf(outfile,"</Placemark>\n");
	}
	fprintf(outfile,"   </Folder>\n");
	fprintf(outfile,"   </kml>\n");
	fclose(outfile);
}



void AddImageOverlay(FILE * outfile, inputCase inp)
{
	char fname[]="                                                                                                                " ;
	getFilename("P1_MAXHEIGHT_END","png",-1,inp.outPath,fname);

	fprintf(outfile,"<GroundOverlay>\n");
	fprintf(outfile,"<name>Maximum height</name>\n");
	fprintf(outfile,"<Icon>\n");
	fprintf(outfile,"	<href>%s</href>\n",fname);
	fprintf(outfile,"	<viewBoundScale>0.75</viewBoundScale>\n");
	fprintf(outfile,"</Icon>\n");
	fprintf(outfile,"	<LatLonBox>\n");
	fprintf(outfile,"		<north>%f</north>\n",inp.latmax );
	fprintf(outfile,"		<south>%f</south>\n",inp.latmin );
	fprintf(outfile,"		<east>%f</east>\n",inp.lonmax);
	fprintf(outfile,"		<west>%f</west>\n",inp.lonmin);
	fprintf(outfile,"	</LatLonBox>\n");
	fprintf(outfile,"</GroundOverlay>\n");
	
	getFilename("P1_INUNDATION_END","png",-1,inp.outPath,fname);
	fprintf(outfile,"<GroundOverlay>\n");
	fprintf(outfile,"<name>Inundation extent</name>\n");
	fprintf(outfile,"<Icon>\n");
	fprintf(outfile,"	<href>%s</href>\n",fname);
	fprintf(outfile,"	<viewBoundScale>0.75</viewBoundScale>\n");
	fprintf(outfile,"</Icon>\n");
	fprintf(outfile,"	<LatLonBox>\n");
	fprintf(outfile,"		<north>%f</north>\n",inp.latmax );
	fprintf(outfile,"		<south>%f</south>\n",inp.latmin );
	fprintf(outfile,"		<east>%f</east>\n",inp.lonmax);
	fprintf(outfile,"		<west>%f</west>\n",inp.lonmin);
	fprintf(outfile,"	</LatLonBox>\n");
	fprintf(outfile,"</GroundOverlay>\n");

	}

void colorIcon(float hei, char * colore)
{  
	if (hei<1)
		copystring("ff00aa00\0",colore);  //verde     0-1 m
	if (hei>=1 && hei<2)	
		copystring("ff00ffff\0",colore);  //giallo    1-2 m
	if (hei>=2 && hei<3)	
		copystring("ff00aaff\0",colore);   //arancio  2-3 m
	if (hei>=3 && hei<4)	
		copystring("ff0066ff\0",colore);   //arancio-red    3-4 m
	//if (hei>=4 && hei<5)	
	//	copystring("ff00aaff\0",colore);   //arancio   4-5 m
	if (hei>=4)	
		copystring("ff0000ff\0",colore);   //rosso     >5m

	//return colore;
}
//struct tm  AddDate1(struct tm  date0, int AddSeconds)
//{
//	time_t tt = mktime(&date0);// &tm);
//	char * command=(char*) calloc(100,sizeof(char));
//	sprintf(command,"date --date=\"+%d %b %Y %H:%M:%S %i sec\" \"+\%d \%b \%Y \%H:\%M:\%S\"",date0,AddSeconds);
//	printf(command);
//
//}
double HoursSince1900(char * dat)
{
	struct tm tmp;
	tmp=StringaToDate(dat);

	time_t tt = mktime(&tmp);// &tm);  //number of seconds from 1 Jan 1970
	//  613657.0  number of hours from 1900 to 1970
	double hours = (tt/3600)+ 613657.0;
	
	return hours;
}


struct tm  AddDate(struct tm  date0, int AddSeconds)
{
	
	time_t tt = mktime(&date0);// &tm);
	//#ifdef WINDOWS	
	tt += AddSeconds;
	//#endif
	//#ifdef LINUX
	//tt += AddSeconds - 30*24*3600. -90000. +3600.;  //HPCI
	//tt += AddSeconds;//  -90000. +3600;  //Fluentsrv2
	//tt.gmt
	//#endif
	
	struct tm * newdate = gmtime(&tt);
	char *ts=(char *) calloc(32,sizeof(char));
	//AddDate1(date0,AddSeconds);
	strftime(ts, sizeof(ts), "%m %b %Y %H:%M:%S", newdate);
	//sprintf(ts,"%s",ctime(&tt));
	//newdate=StringaToDate(ts);
	return *newdate;
}

//struct tm  AddDate(struct tm  date0, int AddSeconds)
//{
//	struct tm retValue=date0;
//	retValue.tm_sec +=AddSeconds;
//	mktime(&retValue);
//	//time_t tt = mktime(&date0);// &tm);
//	//printf("in Adddate tt= %2d/%02d/%02d\n ",date0.tm_mday,date0.tm_mon,date0.tm_year);
//	//tt += AddSeconds;
//	//struct tm * newdate = localtime(&tt);
//	char ts[32];
//	//printf("AddDate new date: %m %b %Y %H:%M:%S  \n", newdate);
//	//strftime(ts, sizeof(ts), "%m %b %Y %H:%M:%S", newdate);
//	//return *newdate;
//	return retValue;
//}


float getGlobalMaxValue(float h[(NROWS+1)][(NCOLS+1)],int ip2, int jp2)
{
	int kx,ky;
	float hei,maxH;
	
    maxH=-1;	
	for (kx=2;kx<=ip2;kx++)
	{
		for (ky=2;ky<=jp2;ky++)
		{
			hei=h[ky][kx];
			if (hei>maxH) maxH=hei;
		}
	}
	return maxH;
}
void getFilename(char * prefix, char * extension, float t, char * outDir, char * fullname)
{
	#ifdef WINDOWS
			char * barra="\\";
	#else
			char * barra="/";
	#endif

			checkDir(outDir);
	if (t>=0)
		if (strlen(outDir)==0)
			sprintf(fullname,"%s%08u.%s",prefix,(int) t, extension);
		else
			sprintf(fullname,"%s%s%s%08u.%s",outDir,barra,prefix,(int) t, extension);
	else
		if (strlen(outDir)==0)
			sprintf(fullname,"%s.%s",prefix, extension);
		else

			sprintf(fullname,"%s%s%s.%s",outDir,barra,prefix, extension);

}

void establishMaxHeigth(float time0, float h[(NROWS+1)][(NCOLS+1)],float mh[(NROWS+1)][(NCOLS+1)], float TarrMax[(NROWS+1)][(NCOLS+1)],float d[(NROWS+1)][(NCOLS+1)], float minDepthWP,  float setpoint, int ip2, int jp2, float NODATA)
{
	int i,j;
	for (i=0;i<=ip2;i++)
	{
		for (j=0;j<=jp2;j++)
		{
			if (mh[j][i]<h[j][i] && h[j][i]!=NODATA  && d[j][i] > minDepthWP )
			{
				mh[j][i]=h[j][i];
				if (mh[j][i]>setpoint)
					TarrMax[j][i]=time0/3600;
			}
		}
	}
}

void filterValues(float hin[(NROWS+1)][(NCOLS+1)],float d[(NROWS+1)][(NCOLS+1)],float setpoint, int comparison, int ip2, int jp2)
{
	int i,j;
	for (i=0;i<=ip2;i++)
	{
		for (j=0;j<=jp2;j++)
		   if (comparison==-1)  //>
		   {
			if (d[j][i]>=setpoint)
				hin[j][i]=99999.0;
			}
			else if(comparison==0)
			{
			if (d[j][i]==setpoint)
				hin[j][i]=99999.0;
			}
			else if(comparison==1)
			{
			if (d[j][i]<=setpoint)
				hin[j][i]=99999.0;
			}
			
	}
}

void establishArrivalTime(float time0, float h[(NROWS+1)][(NCOLS+1)],float h0[(NROWS+1)][(NCOLS+1)],float Tarrh[(NROWS+1)][(NCOLS+1)],float d[(NROWS+1)][(NCOLS+1)], float minDepthTT, float setpoint, int ip2, int jp2, float NODATA)
{
	int i,j;
	for (i=0;i<=ip2;i++)
	{
		for (j=0;j<=jp2;j++)
		{ 

			if (Tarrh[j][i]==NODATA && fabs(h[j][i]-h0[j][i])>setpoint  && h[j][i] != NODATA && d[j][i] > minDepthTT )
			{
				i=i;
				Tarrh[j][i]=time0/3600;
			}
		}
	}
}

void setInitialHeightTarr(float h[(NROWS+1)][(NCOLS+1)],float h0[(NROWS+1)][(NCOLS+1)],float mh1[(NROWS+1)][(NCOLS+1)],float Tarrh[(NROWS+1)][(NCOLS+1)],float TarrMax[(NROWS+1)][(NCOLS+1)], int ip2, int jp2, float NODATA)
{
	int i,j;
	for (i=0;i<=ip2;i++)
	{
		for (j=0;j<=jp2;j++)
		{
				h0[j][i]=h[j][i];
				mh1[j][i]=0.0;
				Tarrh[j][i]=NODATA;
				TarrMax[j][i]=NODATA;
		}
	}
}

void updateWPtimelines(char * prefix, char * barra, placedef place, float time0, float h[(NROWS+1)][(NCOLS+1)], char * outDir, int iiMaxWP)
{
	char * fname =(char *) calloc(256,sizeof(char));
	char * dummy =(char *) calloc(256,sizeof(char));
	char * location =(char *) calloc(256,sizeof(char));
	FILE * outfile;
	float maxh,minh;
	location=replace_str(place.place ,"/","_");
	location=ctrim(replace_str(place.place ," ","_"));
	
	AddString_s(dummy,outDir,barra,prefix,"_",location);
	AddString_s(fname,dummy,".txt",NULL,NULL,NULL);
	//getFilename(fname,"txt",-1,outDir,fname);
	//printf("writing %s\n",fname);
	if (fileExists(fname))
		outfile = fopen(fname, "a");
	else
	{
		outfile = fopen(fname, "w");
		fprintf(outfile,"* Location: %s lat=%f - lon=%f  iy=%i  ix=%i\n",place.place,place.lat,place.lon,place.iy,place.ix);
	}
	if (compString(prefix,"WT"))
	{
		maxh=getMaxValue(&place,h,iiMaxWP);
		minh=getMinValue(&place,h,iiMaxWP);
		fprintf(outfile,"%f,%f,%f,%f\n",time0,maxh,minh,place.MaxHeight);
	}
	else
	{
		//maxh=getMaxValue(&place,h,0);
		maxh=h[place.iy][place.ix];
		fprintf(outfile,"%f,%f,%f,%f\n",time0,maxh,place.MaxHeight);
	}
	fclose(outfile);

	delete [] fname,location,outfile;
	fname=NULL;location=NULL;outfile=NULL;

}

void updateMaxHeight(char * barra, float time0, float OverallHmax, float CurrentMax, float CurrentMaxDepth, char * outDir)
{
	char * fname =(char *) calloc(256,sizeof(char));
	FILE * outfile;
	float maxh;
	AddString_s(fname,outDir,barra,"maxValues.txt",NULL,NULL);
	
	if (fileExists(fname))
		outfile = fopen(fname, "a");
	else
	{
		outfile = fopen(fname, "w");
		fprintf(outfile,"* time (s), OverallHmax,CurrentHmax\n");
	}
	fprintf(outfile,"%f,%f,%f,%f\n",time0,OverallHmax,CurrentMax,CurrentMaxDepth);
	fclose(outfile);

	delete [] fname,outfile;
	fname=NULL;outfile=NULL;

}

void logv(float t0,float value1, float value2, float value3)
{	
	FILE * outfile;
	char *fname="d:\\loguv.txt\0";
	//getFilename(fname,"txt",-1,outDir,fname);
	if (fileExists(fname))
		outfile = fopen(fname, "a");
	else
	{
		outfile = fopen(fname, "w");
	}
	fprintf(outfile,"%f     %f     %f   %f\n",t0,value1,value2,value3);
	fclose(outfile);

}
float getMaxValue(placedef *place,float h[(NROWS+1)][(NCOLS+1)],int iiMaxWP)
{
	int ix0,ix1,iy0,iy1,kx,ky;
	float hei;
	float maxhei=-999.;
	ix0=place->ix-iiMaxWP;
	ix1=place->ix+iiMaxWP;
	iy0=place->iy-iiMaxWP;
	iy1=place->iy+iiMaxWP;

	for (kx=ix0;kx<=ix1;kx++)
	{
		for (ky=iy0;ky<=iy1;ky++)
		{
			hei=h[ky][kx];
			if (hei>maxhei && hei<1000.) maxhei=hei;
		}
	}
	return maxhei;
}
float getMinValue(placedef *place,float h[(NROWS+1)][(NCOLS+1)],int iiMaxWP)
{
	int ix0,ix1,iy0,iy1,kx,ky;
	float hei;
	float minhei=999.;
	ix0=place->ix-iiMaxWP;
	ix1=place->ix+iiMaxWP;
	iy0=place->iy-iiMaxWP;
	iy1=place->iy+iiMaxWP;

	for (kx=ix0;kx<=ix1;kx++)
	{
		for (ky=iy0;ky<=iy1;ky++)
		{
			hei=h[ky][kx];
			if (hei<minhei && hei<1000.) minhei=hei;
		}
	}
	return minhei;
}

bool fileExists(char * fname)
{
	bool ex;
	FILE * outfile = fopen(fname, "r");
	if(outfile!=NULL)
	{
		ex=true;
		fclose(outfile);
	}
	else
		ex=false;

	return ex;
}

float getMaximum(float d[(NROWS+1)][(NCOLS+1)],float h[(NROWS+1)][(NCOLS+1)], int ip2, int jp2, int mode)
{
	 float maxx;
	 int i,j;
	 maxx=0;
	  for (j=0;j<=jp2;j++){
		  for (i=0;i<=ip2;i++){
			if (mode==0)
			{
				if (h[j][i]>maxx && h[j][i]<1000.) maxx=	h[j][i];  // to contrast NoDataValue
			}
			else
			{
				if (h[j][i]>maxx && d[j][i]>0 && h[j][i]<1000.) 
					maxx=	h[j][i];  // to contrast NoDataValue
			}
			
		  }
	  }
	  return maxx;
}

void ReadInputFromNETCDF(int IDTime, char * fold, float h[(NROWS+1)][(NCOLS+1)], float lonmin, float lonmax, float latmin, float latmax,int ip2, int jp2,int &nx,int &ny)
 {
	float lonMinInt, lonMaxInt, latMinInt, latMaxInt,  dxint, dyint;
	int ierr,kx,ky,i;
	float lon,lat;
	int ncid,VAR_T,VAR_LON,VAR_LAT,VAR_HA;
	GDALRasterBandH  dstband;
	GDALDatasetH  hDstDS;
    char * fname = (char*) calloc(512,sizeof(char));
    getFilename("NETCDF_H","nc",-1,fold, fname);
	if (fileExists(fname))
	{   float * values=(float*) calloc(1,sizeof(float));
	    NETCDFgetLine( 0,0,0,0, fname, "HA", 0, &ncid, IDTime, &VAR_T,&VAR_LAT, &VAR_LON, &VAR_HA, &ny, &nx, &lonMinInt, &dxint, &latMaxInt, &dyint, &ierr, values);
        lonMaxInt=lonMinInt+dxint*nx;
		latMinInt=latMaxInt+dyint*ny;
		if (lonmin > lonMinInt && lonmax < lonMaxInt && latmin > latMinInt && latmax < latMaxInt)
		{
			//interpolate inside
			float * values=(float*) calloc(nx,sizeof(float));
			for (ky = 1;ky<=jp2-1;ky++)
			{    lat=(float) (1-(ky-1)/(float)(jp2-1-2))*(latmax-latmin)+latmin;
				 int jj=(int) (((latMaxInt-lat))/(latMaxInt-latMinInt)*(float)ny);
				 jj=ny-jj;
//				 printf("ky=%i  jj=%i  lat=%f\n",ky,jj,lat);
				NETCDFgetLine( 0,nx,jj,1, fname, "HA", 1, &ncid, IDTime, &VAR_T,&VAR_LAT, &VAR_LON, &VAR_HA,  &ny, &nx, &lonMinInt, &dxint, &latMaxInt, &dyint, &ierr, values);
				for (kx = 2;kx<=ip2 - 1;kx++)
				{
					lon=((float) (kx-1)/(float) (ip2-2-2)*(lonmax-lonmin)+lonmin);
				
					
					i=(int)((float)((lon-lonMinInt)/(lonMaxInt-lonMinInt)* (float) nx));
                    //printf("%f %f  %i %i  %i %i  %f\n",lon,lat, ky,kx,i,jj,values[i]);
					h[jp2-ky][kx] =(float) values[i];
					//if (h[jp2-ky][kx] >1.)
					//	printf("jp2-ky  %i   kx: %i  %f\n",jp2-ky,kx,h[jp2-ky][kx] );
				}
			}
			NETCDFgetLine( 0,0,0,0, fname,"HA", 2, &ncid, IDTime, &VAR_T,&VAR_LAT, &VAR_LON, &VAR_HA,  &ny, &nx, &lonMinInt, &dxint, &latMaxInt, &dyint, &ierr, values);
			delete [] values;
			values=NULL;
		}
		else
			printf("outside range");
	}
    else
        printf("File not existing %s", fname);
    delete [] fname;
	fname=NULL;

 }


void ReadInputFromTIF(float time0, char * fold, float h[(NROWS+1)][(NCOLS+1)], float lonmin, float lonmax, float latmin, float latmax,int ip2, int jp2,int &nx,int &ny)
 {
	 float lonMinInt, lonMaxInt, latMinInt, latMaxInt,  dxint, dyint;
	int ierr,kx,ky,i,nx0,ny0;
	float lon,lat;
	nx0=nx;ny0=ny;
	GDALRasterBandH  dstband;
	GDALDatasetH  hDstDS;
    char * fname = (char*) calloc(512,sizeof(char));
    getFilename("TIF_H_","tif",time0,fold, fname);
	if (fileExists(fname))
	{   double * values=(double*) calloc(1,sizeof(double));
	    gdalGetLine( 0,0,0,0, fname, 0, dstband,1, hDstDS, nx, ny, lonMinInt, dxint, latMaxInt, dyint, ierr, values);
        lonMaxInt=lonMinInt+dxint*nx;
		latMinInt=latMaxInt+dyint*ny;
		if (lonmin >= lonMinInt && lonmax <= lonMaxInt && latmin >= latMinInt && latmax <= latMaxInt)
		{
			//interpolate inside
			double * values=(double*) calloc(nx,sizeof(double));
			if (nx0 != -1)
			{
				for (ky = 1;ky<=jp2-1;ky++)
				{    lat=(float) (1-(ky-1)/(float)(jp2-1-2))*(latmax-latmin)+latmin;
					 int jj=(int) (((latMaxInt-lat))/(latMaxInt-latMinInt)*(float)ny);
	//				 printf("ky=%i  jj=%i  lat=%f\n",ky,jj,lat);
					gdalGetLine(0,nx, jj,1, fname, 1, dstband,1, hDstDS, nx, ny, lonMinInt, dxint, latMaxInt, dyint, ierr, values);
					for (kx = 2;kx<=ip2 - 1;kx++)
					{
						lon=((float) (kx-1)/(float) (ip2-2-2)*(lonmax-lonmin)+lonmin);
						i=(int)((float)((lon-lonMinInt)/(lonMaxInt-lonMinInt)* (float) nx));
					  //  printf("%f %f  %i %i  %i %i  %f\n",lon,lat, ky,kx,i,jj,values[i]);
						h[jp2-ky][kx] =(float) values[i];
					}
				}
			}
			gdalGetLine(0,0,0,0,  fname, 2, dstband,1, hDstDS, nx, ny, lonMinInt, dxint, latMaxInt, dyint, ierr, values);
			delete []values;
			values=NULL;
		}
		else
			printf("outside range");
	}
    else
        printf("File not existing %s", fname);
    
 }

int Round(float a)
{
	float resto=a-(int) a;
	if (resto>=0.5)
		return (int)a+1;
	else
		return (int)a;
}
void getIDint(int kx, int ky, float lonmin, float lonmax, float lonMinInt, float lonMaxInt, float latmin, float latmax, float latMinInt, float latMaxInt,int ip2,int jp2,int nx,int ny, int &i, int &j)
{
	float lon=((float) (kx-1)/(float) (ip2-2-2)*(lonmax-lonmin)+lonmin);
	i=(int)((float)((lon-lonMinInt)/(lonMaxInt-lonMinInt)* (float) nx));
	float lat=(float) ((float) (ky-1)/(float)(jp2-1-2))*(latmax-latmin)+latmin;
	j=(int) (((latMaxInt-lat))/(latMaxInt-latMinInt)*(float)ny);

}

bool setBoundaryUV_gdal(float time0, float &timeNew, float & timeOld, char * fold, float u[(NROWS+1)][(NCOLS+1)], float v[(NROWS+1)][(NCOLS+1)], float lonmin, float lonmax, float latmin, float latmax,float dx, float dy, int ip2, int jp2,int nx, int ny,double *vbBottom0,double *vbTop0,double *ubLeft0,double *ubRight0,double *vbBottom,double *vbTop,double *ubLeft,double *ubRight, float & lonMinInt,float & lonMaxInt,float & latMinInt, float &latMaxInt)
{	char * fnameU = (char*) calloc(512,sizeof(char));
	char * fnameV = (char*) calloc(512,sizeof(char));
	float dxint, dyint;
	int ierr,kx,ky,i,jj;
	
	GDALRasterBandH  dstband;
	GDALDatasetH  hDstDS;
	double * values=(double*) calloc(1,sizeof(double));
		
	//if (timeOld == NULL)
	//	timeOld=0.;
	int IDTime;
	float time1=getNextTime(time0,fold,&IDTime);
	if (time1==-1)
	{
		printf("no new time exists");
		return false;
	}
	if (time1!=timeNew)
	{   if (timeNew!=-1)
		timeOld=timeNew;
		if (timeOld==0.0 || timeNew==-1)
		{
			// retrieve old velocities at the borders
			getFilename("TIF_U_","tif",timeOld,fold, fnameU);
			getFilename("TIF_V_","tif",timeOld,fold, fnameV);
			gdalGetLine( -1,0,0,0, fnameU, 0, dstband,1, hDstDS, nx, ny, lonMinInt, dxint, latMaxInt, dyint, ierr, values);
			lonMaxInt=lonMinInt+dxint*nx;
			latMinInt=latMaxInt+dyint*ny;

			ky=2;kx=2;
			getIDint(kx,ky,lonmin,lonmax,lonMinInt,lonMaxInt,latmin,latmax,latMinInt,latMaxInt,ip2,jp2,nx,ny,i,jj);
			gdalGetLine( 0,nx,jj,1, fnameV, 3, dstband,1, hDstDS, nx, ny, lonMinInt, dxint, latMaxInt, dyint, ierr, vbBottom0);
			gdalGetLine(i,1,0,ny, fnameU, 3, dstband,1, hDstDS, nx, ny, lonMinInt, dxint, latMaxInt, dyint, ierr, ubLeft0);

			ky=jp2-1;kx=ip2;
			getIDint(kx,ky,lonmin,lonmax,lonMinInt,lonMaxInt,latmin,latmax,latMinInt,latMaxInt,ip2,jp2,nx,ny,i,jj);
			gdalGetLine(0,nx, jj,1, fnameV, 3, dstband,1, hDstDS, nx, ny, lonMinInt, dxint, latMaxInt, dyint, ierr, vbTop0);
			gdalGetLine(i,1,0,ny, fnameU, 3, dstband, 1, hDstDS, nx, ny, lonMinInt, dxint, latMaxInt, dyint, ierr, ubRight0);
		}
		else
		{
			getFilename("TIF_V_","tif",timeNew,fold, fnameV);
			gdalGetLine( -1,0,0,0, fnameV, 0, dstband,1, hDstDS, nx, ny, lonMinInt, dxint, latMaxInt, dyint, ierr, values);
			gdalGetLine( -1,0,0,0, fnameV, 2, dstband,1, hDstDS, nx, ny, lonMinInt, dxint, latMaxInt, dyint, ierr, values);
			lonMaxInt=lonMinInt+dxint*nx;
			latMinInt=latMaxInt+dyint*ny;
			for (i=0;i<=nx;i++)
			{
				vbTop0[i]=vbTop[i];
				vbBottom0[i]=vbBottom[i];
			}
			for (i=0;i<=ny;i++)
			{
				ubLeft0[i]=ubLeft[i];
				ubRight0[i]=ubRight[i];
			}
		}

		timeNew=time1;

		// retrieve new velocities at the borders
		// retrieve old velocities at the borders
		getFilename("TIF_U_","tif",timeNew,fold, fnameU);
		getFilename("TIF_V_","tif",timeNew,fold, fnameV);
		//gdalGetLine( -1,0,0,0, fnameU, 0, dstband, hDstDS, nx, ny, lonMinInt, dxint, latMaxInt, dyint, ierr, values);
		
		ky=2;kx=2;
		getIDint(kx,ky,lonmin,lonmax,lonMinInt,lonMaxInt,latmin,latmax,latMinInt,latMaxInt,ip2,jp2,nx,ny,i,jj);
		gdalGetLine( 0,nx,jj,1, fnameV, 3, dstband,1, hDstDS, nx, ny, lonMinInt, dxint, latMaxInt, dyint, ierr, vbBottom);
		gdalGetLine( i,1,0,ny, fnameU, 3, dstband,1, hDstDS, nx, ny, lonMinInt, dxint, latMaxInt, dyint, ierr, ubLeft);

		ky=jp2-1;kx=ip2;
		getIDint(kx,ky,lonmin,lonmax,lonMinInt,lonMaxInt,latmin,latmax,latMinInt,latMaxInt,ip2,jp2,nx,ny,i,jj);
		gdalGetLine(0,nx, jj,1, fnameV, 3, dstband,1, hDstDS, nx, ny, lonMinInt, dxint, latMaxInt, dyint, ierr, vbTop);
		gdalGetLine(i,1,0,ny, fnameU, 3, dstband,1,  hDstDS, nx, ny, lonMinInt, dxint, latMaxInt, dyint, ierr, ubRight);
	}
	else
	{
		getFilename("TIF_V_","tif",timeNew,fold, fnameV);
		gdalGetLine( -1,0,0,0, fnameV, 0, dstband,1, hDstDS, nx, ny, lonMinInt, dxint, latMaxInt, dyint, ierr, values);
		gdalGetLine( -1,0,0,0, fnameV, 2, dstband,1, hDstDS, nx, ny, lonMinInt, dxint, latMaxInt, dyint, ierr, values);
		lonMaxInt=lonMinInt+dxint*nx;
		latMinInt=latMaxInt+dyint*ny;
	}
	    
    // Assign boundary conditions
    //top and bottom
    float f = (time0 - timeOld) / (time1 - timeOld);
	float fx,fy;
    int i1,i2;
	//top and bottom
    for (kx=2;kx<=ip2 - 1;kx++)
	{
		//i = Round((kx - 2) / (ip2 - 1 - 2) * (nx - 1));
        
		//printf("%i %i", (kx - 2) / (ip2 - 1 - 2) * (nx-1), i);
        getIDint(kx,1,lonmin,lonmax,lonMinInt,lonMaxInt,latmin,latmax,latMinInt,latMaxInt,ip2,jp2,nx,ny,i1,jj); 
		//i1 = (int) (((float) kx - 2) / ((float) ip2 - 1 - 2) * (nx - 1));
        i2 = i1 + 1;
		fx = ((kx-2)*dx+lonmin-(i1*dxint+lonMinInt))/dxint;		
        //fx = (float) (((float) kx - 2) / ((float) ip2 - 1 - 2) * (nx - 1) - i1);
        
        v[jp2][kx] = (float)( (vbTop[i1] * fx + vbTop[i2] * (1 - fx)) * f + (vbTop0[i1] * fx + vbTop0[i2]  * (1 - fx)) * (1 - f));
        v[2]  [kx] = (float)( (vbBottom[i1] * fx + vbBottom[i2] * (1 - fx)) * f + (vbBottom0[i1] * fx + vbBottom0[i2] * (1 - fx)) * (1 - f));
    }

	// right and left
    for (ky=2;ky<=jp2 - 1;ky++)
	{
     //   i = Round((ky - 2) / (jp2 - 1 - 2) * (deltaIyV - 1), 0)
        getIDint(1,ky,lonmin,lonmax,lonMinInt,lonMaxInt,latmin,latmax,latMinInt,latMaxInt,ip2,jp2,nx,ny,i,i1);
		//i1 = (int) ((float)(ky - 2) / (float) (jp2 - 1 - 2) * (float) (ny - 1));
        i2 = i1 + 1;
        //fy = (float) (ky - 2) / (float) (jp2 - 1 - 2) * (float) (ny - 1) - i1;
        fy = ((ky-2)*dy+latmin-((ny-i1)*(-dyint)+latMinInt))/dyint;		
        u[ky][2]   = (float) ((ubLeft[i1] * fy + ubLeft[i1] * (1 - fy)) * f + (ubLeft0[i1] * fy + ubLeft0[i2] * (1 - fy)) * (1 - f));
        u[ky][ip2] = (float) ((ubRight[i1] * fy + ubRight[i1] * (1 - fy)) * f + (ubRight0[i1] * fy + ubRight0[i2] * (1 - fy)) * (1 - f));
	}

	//timeOld = time1;

	delete [] values,fnameU,fnameV;
	values=NULL;fnameU=NULL;fnameV=NULL;
	return true;


}


bool setBoundaryUV_NETCDF(float time0, float &timeNew, float & timeOld, char * fold, float u[(NROWS+1)][(NCOLS+1)], float v[(NROWS+1)][(NCOLS+1)], float lonmin, float lonmax, float latmin, float latmax,float dx, float dy, int ip2, int jp2,int nx, int ny,float *vbBottom0,float *vbTop0,float *ubLeft0,float *ubRight0,float *vbBottom,float *vbTop,float *ubLeft,float *ubRight, float & lonMinInt,float & lonMaxInt,float & latMinInt, float &latMaxInt)
{	char * fnameU = (char*) calloc(512,sizeof(char));
	char * fnameV = (char*) calloc(512,sizeof(char));
	float dxint, dyint;
	int ierr,kx,ky,i,jj;
	bool split=true;
	float * values=(float*) calloc(1,sizeof(float));
		
	//if (timeOld == NULL)
	//	timeOld=0.;
	int IDTime;
	int ncid,VAR_T,VAR_LAT,VAR_LON,VAR_HA;
	float time1=getNextTime(time0,fold,&IDTime);
	if (time1==-1 || IDTime ==-1)
	{
		printf("no new time exists");
		return false;
	}
	getFilename("NETCDF_U","nc",-1,fold, fnameU);
	getFilename("NETCDF_V","nc",-1,fold, fnameV);

	if (time1!=timeNew)
	{   if (timeNew!=-1)
			timeOld=timeNew;
		if (timeOld==0.0 || timeNew==-1)
		{
			// retrieve old velocities at the borders
		    NETCDFgetLine( 0,0,0,0, fnameU,"U", 0, &ncid, IDTime, &VAR_T,&VAR_LAT, &VAR_LON, &VAR_HA, &ny, &nx, &lonMinInt, &dxint, &latMaxInt, &dyint, &ierr, values);
		    NETCDFgetLine( 0,0,0,0, fnameU,"U", 2, &ncid, IDTime, &VAR_T,&VAR_LAT, &VAR_LON, &VAR_HA, &ny, &nx, &lonMinInt, &dxint, &latMaxInt, &dyint, &ierr, values);

			lonMaxInt=lonMinInt+dxint*nx;
			latMinInt=latMaxInt+dyint*ny;

			ky=2;kx=2;
			getIDint(kx,ky,lonmin,lonmax,lonMinInt,lonMaxInt,latmin,latmax,latMinInt,latMaxInt,ip2,jp2,nx,ny,i,jj);
			if (split) jj=ny-jj;			
			NETCDFgetLine( 0,nx,jj,1, fnameV,"V", 3, &ncid, IDTime, &VAR_T,&VAR_LAT, &VAR_LON, &VAR_HA, &ny, &nx, &lonMinInt, &dxint, &latMaxInt, &dyint, &ierr, vbBottom0);
			NETCDFgetLine( i,1,0,ny, fnameU,"U", 3, &ncid, IDTime, &VAR_T,&VAR_LAT, &VAR_LON, &VAR_HA, &ny, &nx, &lonMinInt, &dxint, &latMaxInt, &dyint, &ierr, ubLeft0);


			ky=jp2-1;kx=ip2;
			getIDint(kx,ky,lonmin,lonmax,lonMinInt,lonMaxInt,latmin,latmax,latMinInt,latMaxInt,ip2,jp2,nx,ny,i,jj);
			if (split)  jj=ny-jj;
			NETCDFgetLine( 0,nx,jj,1, fnameV,"V", 3, &ncid, IDTime, &VAR_T,&VAR_LAT, &VAR_LON, &VAR_HA, &ny, &nx, &lonMinInt, &dxint, &latMaxInt, &dyint, &ierr, vbTop0);
			NETCDFgetLine( i,1,0, ny, fnameU,"U", 3, &ncid, IDTime, &VAR_T,&VAR_LAT, &VAR_LON, &VAR_HA, &ny, &nx, &lonMinInt, &dxint, &latMaxInt, &dyint, &ierr, ubRight0);

		}
		else
		{
			NETCDFgetLine( 0,0,0,0, fnameU,"U", 0, &ncid, IDTime, &VAR_T,&VAR_LAT, &VAR_LON, &VAR_HA, &ny, &nx, &lonMinInt, &dxint, &latMaxInt, &dyint, &ierr, values);
			NETCDFgetLine( 0,0,0,0, fnameU,"U", 2, &ncid, IDTime, &VAR_T,&VAR_LAT, &VAR_LON, &VAR_HA, &ny, &nx, &lonMinInt, &dxint, &latMaxInt, &dyint, &ierr, values);

			lonMaxInt=lonMinInt+dxint*nx;
			latMinInt=latMaxInt+dyint*ny;
			for (i=0;i<=nx;i++)
			{
				vbTop0[i]=vbTop[i];
				vbBottom0[i]=vbBottom[i];
			}
			for (i=0;i<=ny;i++)
			{
				ubLeft0[i]=ubLeft[i];
				ubRight0[i]=ubRight[i];
			}
		}

		timeNew=time1;

		// retrieve new velocities at the borders
		// retrieve old velocities at the borders
		
		ky=2;kx=2;
		getIDint(kx,ky,lonmin,lonmax,lonMinInt,lonMaxInt,latmin,latmax,latMinInt,latMaxInt,ip2,jp2,nx,ny,i,jj);
		if (split)  jj=ny-jj;
		NETCDFgetLine( 0,nx,jj,1, fnameV,"V", 3, &ncid, IDTime, &VAR_T,&VAR_LAT, &VAR_LON, &VAR_HA, &ny, &nx, &lonMinInt, &dxint, &latMaxInt, &dyint, &ierr, vbBottom);
		NETCDFgetLine( i,1,0,ny, fnameU,"U", 3, &ncid, IDTime, &VAR_T,&VAR_LAT, &VAR_LON, &VAR_HA, &ny, &nx, &lonMinInt, &dxint, &latMaxInt, &dyint, &ierr, ubLeft);

		ky=jp2-1;kx=ip2;
		getIDint(kx,ky,lonmin,lonmax,lonMinInt,lonMaxInt,latmin,latmax,latMinInt,latMaxInt,ip2,jp2,nx,ny,i,jj);
		if (split)  jj=ny-jj;
		NETCDFgetLine( 0,nx,jj,1, fnameV,"V", 3, &ncid, IDTime, &VAR_T,&VAR_LAT, &VAR_LON, &VAR_HA, &ny, &nx, &lonMinInt, &dxint, &latMaxInt, &dyint, &ierr, vbTop);
		NETCDFgetLine( i,1,0, ny, fnameU,"U", 3, &ncid, IDTime, &VAR_T,&VAR_LAT, &VAR_LON, &VAR_HA, &ny, &nx, &lonMinInt, &dxint, &latMaxInt, &dyint, &ierr, ubRight);

		//for(ky=0;ky<ny;ky++)
		//{
		//	if (ubRight[ky]>0.05 || ubRight[ky]<-0.05 )
		//		printf("%i  %f\n",ky, ubRight[ky]);
		//}
	}
	else
	{
		NETCDFgetLine( 0,0,0,0, fnameU,"U", 0, &ncid, IDTime, &VAR_T,&VAR_LAT, &VAR_LON, &VAR_HA, &ny, &nx, &lonMinInt, &dxint, &latMaxInt, &dyint, &ierr, values);
		NETCDFgetLine( 0,0,0,0, fnameU,"U", 2, &ncid, IDTime, &VAR_T,&VAR_LAT, &VAR_LON, &VAR_HA, &ny, &nx, &lonMinInt, &dxint, &latMaxInt, &dyint, &ierr, values);
		lonMaxInt=lonMinInt+dxint*nx;
		latMinInt=latMaxInt+dyint*ny;
	}
	    
    // Assign boundary conditions
    //top and bottom
    float f = (time0 - timeOld) / (time1 - timeOld);
	float fx,fy;
    int i1,i2;
	//top and bottom
    for (kx=2;kx<=ip2 - 1;kx++)
	{
		//i = Round((kx - 2) / (ip2 - 1 - 2) * (nx - 1));
        
		//printf("%i %i", (kx - 2) / (ip2 - 1 - 2) * (nx-1), i);
        getIDint(kx,1,lonmin,lonmax,lonMinInt,lonMaxInt,latmin,latmax,latMinInt,latMaxInt,ip2,jp2,nx,ny,i1,jj); 
		//i1 = (int) (((float) kx - 2) / ((float) ip2 - 1 - 2) * (nx - 1));
        i2 = i1 + 1;
		if (split)  jj=ny-jj;
		fx = ((kx-2)*dx+lonmin-(i1*dxint+lonMinInt))/dxint;		
        //fx = (float) (((float) kx - 2) / ((float) ip2 - 1 - 2) * (nx - 1) - i1);
        
        v[jp2][kx] = (float)( (vbTop[i1] * fx + vbTop[i2] * (1 - fx)) * f + (vbTop0[i1] * fx + vbTop0[i2]  * (1 - fx)) * (1 - f));
        v[2]  [kx] = (float)( (vbBottom[i1] * fx + vbBottom[i2] * (1 - fx)) * f + (vbBottom0[i1] * fx + vbBottom0[i2] * (1 - fx)) * (1 - f));
    }

	// right and left
    for (ky=2;ky<=jp2 - 1;ky++)
	{
     //   i = Round((ky - 2) / (jp2 - 1 - 2) * (deltaIyV - 1), 0)
        getIDint(1,ky,lonmin,lonmax,lonMinInt,lonMaxInt,latmin,latmax,latMinInt,latMaxInt,ip2,jp2,nx,ny,i,i1);
		//i1 = (int) ((float)(ky - 2) / (float) (jp2 - 1 - 2) * (float) (ny - 1));
        i2 = i1 + 1;
		fy = ((ky-2)*dy+latmin-((ny-i1)*(-dyint)+latMinInt))/dyint;		

		if (split) i1 = ny-i1;
        if (split) i2 = ny-i2;
		//fy = (float) (ky - 2) / (float) (jp2 - 1 - 2) * (float) (ny - 1) - i1;
        u[ky][2]   = (float) ((ubLeft[i1] * fy + ubLeft[i1] * (1 - fy)) * f + (ubLeft0[i1] * fy + ubLeft0[i2] * (1 - fy)) * (1 - f));
        u[ky][ip2] = (float) ((ubRight[i1] * fy + ubRight[i1] * (1 - fy)) * f + (ubRight0[i1] * fy + ubRight0[i2] * (1 - fy)) * (1 - f));
		//u[ky][ip2-1] = (float) ((ubRight[i1] * fy + ubRight[i1] * (1 - fy)) * f + (ubRight0[i1] * fy + ubRight0[i2] * (1 - fy)) * (1 - f));
		//if (u[ky][2]>0.01) printf("%i %f  %i %f \n",i1,ubLeft[i1], ky,u[ky][2]);
		/*if (ubLeft[i1]>0.01) 
		{
			printf ("%i %f   %i: %f\n",i1,ubLeft[i1],ky,u[ky][2]);
		}*/
	}
	//logv(timeOld,ubRight[277],u[(int)(1./4.*jp2)][ip2],u[(int)(3./4.*jp2)][ip2]);

	//timeOld = time1;

	delete [] values;values=NULL;
	return true;


}

void getValueFromLine(char * line,char * delimiter,int col, char * value)
{
   char * label;
   char * st=(char*) calloc(512,sizeof(char));
	int n=-1;
	copystring(line,st);
	label=strtok(st,delimiter);
	while (label!=NULL)
	{
		n++;
		label=cleanValue1(label);
		if (n==col)
		{
			copystring(label,value);
			return;
		}
		label=strtok(NULL,delimiter);
	}
	delete [] st;
	st=NULL;
}
float getNextTime(float time0, char * inputDir, int * IDTime)
{
	float time1;
	int colt,colH,colVx,colVy;
	char * fnameDummy=(char *) calloc(512,sizeof(char));
	char * fname=(char *) calloc(512,sizeof(char));
	char * value=(char *) calloc(512,sizeof(char));
	// reads listfiles.txt and determines the next time recorded after time0
	getFilename("listfiles","txt",-1,inputDir,fname);
	FILE *infile;infile = fopen(fname, "r");
	char delimiter[2]=" ";
	char line[100];
	*IDTime=-1;
	while( fgets(line, sizeof(line), infile) != NULL ) 
	{
		getIndexOfvars(line,delimiter,colt,colH,colVx,colVy);
		if (line[0]!='*' && line[0]!='#')
		{
			*IDTime +=1;
			getValueFromLine(line,delimiter,colt,value);
			time1=(float) atof(value);
			//time1=(float) atof(ctrim(strtok(line," ")));
			if (time1>time0)
			{   fclose(infile);
				return time1;
			}
		}
	}
	fclose(infile);
	delete [] fname,fnameDummy,value;
	fname=NULL;fnameDummy=NULL;value=NULL;
	delete [] line;
	
	return (float) -1.;
}
	
float getPreviousTime(float time0, char * inputDir, int * IDTime)
{
	float time1,time10=-1;
	char * fnameDummy=(char *) calloc(512,sizeof(char));
	char * fname=(char *) calloc(512,sizeof(char));
	// reads listfiles.txt and determines the next time recorded after time0
	getFilename("listfiles","txt",-1,inputDir,fname);
	FILE *infile;infile = fopen(fname, "r");
	char line[100];
	*IDTime=-1;
	while( fgets(line, sizeof(line), infile) != NULL ) 
	{
		if (line[0]!='*' && line[0]!='#')
		{
			*IDTime +=1;
			time1=(float) atof(ctrim(strtok(line," ")));
			if (time10==-1) time10=time1;
			if (time1>time0)
			{   fclose(infile);
				return (float) time10;
			}
			time10=time1;
		}
	}
	fclose(infile);
	delete [] fname,fnameDummy,infile;
	fname=NULL;fnameDummy=NULL;infile=NULL;
	delete [] line;
	return (float) -1.;
}

void getIndexOfvars(char * st,char * delimiter, int &colt,int &colH,int &colVx,int &colVy)
{	char * label;
	int n=-1;
	char * line=(char*) calloc(512,sizeof(char));
	copystring(st,line);

	line[0]=' ';
	label=strtok(line,delimiter);
	while (label!=NULL)
	{
		n++;
		label=ctrim(cleanValue1(label));
		if (strcmp(label,"time")==0 || strcmp(label,"t")==0)
			colt=n;
		if (strcmp(label,"zsurf")==0 || strcmp(label,"HA")==0 )
			colH=n;
		if (strcmp(label,"vx")==0 || strcmp(label,"U")==0)
			colVx=n;
		if (strcmp(label,"vy")==0 || strcmp(label,"V")==0)
			colVy=n;
		label=strtok(NULL,delimiter);
	}
    if (colt==-1) colt=0;
	if (colH==-1) colH=1;
	if (colVx==-1) colVx=2;
	if (colVy==-1) colVy=3;
	delete [] line;line=NULL;
	delete [] label;label=NULL;
}
#ifdef WINDOWS

void CalculateTravelTime(float lon, float lat, float mag, float lonmin, float lonmax, float latmin, float latmax, char * outDir, char * barra, char * CurrentDir, char * p[60])
{
	char * riga =(char *) calloc(512,sizeof(char));

	char fullname[]="                                                                                                                " ;
	getFilename("inputTTT","txt",-1,outDir,fullname);

	FILE *outfile;
	
	/* Open the file.  If NULL is returned there was an error */
	printf("Calculating Travel time");
	outfile = fopen(fullname, "w");
	fprintf(outfile,"%f\n",lat);
	fprintf(outfile,"%f\n",lon);
	fprintf(outfile,"%f\n",mag);
	fprintf(outfile,"%i\n%i\n%i\n%i\n%i\n%i\n",1,0,5,1,0,2);
	fprintf(outfile,"%f\n",latmin);
	fprintf(outfile,"%f\n",latmax);
	fprintf(outfile,"%f\n",lonmin);
	fprintf(outfile,"%f\n",lonmax);
	fclose(outfile); 

	char * command;
	char * command1;
	char * drive;
	drive=(char *) calloc(3,sizeof(char));
	command=(char *) calloc(1000,sizeof(char));
	command1=(char *) calloc(1000, sizeof(char));
	cmid(CurrentDir,1,2,drive);
	//' this is valid only for Windows
	sprintf(command,"%s\\..\\TTT\\TTT\\bin\\ttt_auto_swan.bat  %s  %s\\..\\TTT < %s ",CurrentDir,drive,CurrentDir,fullname);
	
	printf(" Travel Time command  %s\n",command);
	
	int n;
	//command1[0]=0;
	solveDir(command,barra,n,"..",CurrentDir,p,command1);

	printf("command1  %s\n",command1);
	system(command1);
    delete [] riga;riga=NULL;
	delete [] command;command=NULL;
	delete [] command1;command1=NULL;
}

#endif


void createListfilesFromTemplate(char * CurrentDir, char * outDir, char * pathFiles, char * digitFiles, int tstep, int tfin, bool translateTif, char * fname, char * barra, char * p[60])
{
	    // path is in the form  t*.grd  or  TIF_H_*.asc

		FILE *outfile;outfile = fopen(fname, "w");
		fprintf(outfile,"*time            HA\n");
		fprintf(outfile,"*list of files for current case %s\n",fname);
		int k;
		int timeV;
		char * fnameFile = (char *) calloc(500,sizeof(char));
		char * fnameTif  = (char *) calloc(500,sizeof(char));
		char * fnameFile1= (char *) calloc(500,sizeof(char));
		char * fnameTif1 = (char *) calloc(500,sizeof(char));
		char * timeStr   = (char*) calloc (10,sizeof(char));
		char * format    = (char*) calloc(10,sizeof(char));
		AddString_s(format,"%0",digitFiles,"d",NULL,NULL);
		int nval=tfin/tstep +1;
		for(k=0;k<nval;k++)
		{
			timeV=k*tstep;
			sprintf(timeStr,format,timeV);
			replace(pathFiles,"*",timeStr,fnameFile);
			if (translateTif)
			{
				getFilename("TIF_H_","tif",timeV,"", fnameTif);
				AddString_s(fnameTif1,outDir,barra,fnameTif,NULL,NULL);
				AddString_s(fnameFile1,outDir,barra,fnameFile,NULL,NULL);
				gdaltranslate(CurrentDir,barra,p,fnameFile1,fnameTif1);
				fprintf(outfile,"%i  %s\n",timeV,fnameTif);
				printf("Translated %s\n",fnameTif1);
			}
			else
				fprintf(outfile,"%i  %s\n",timeV,fnameFile);
		}
		fclose(outfile);	
	
}

void createListfiles(char * prefix,char * fnamenetcdf_H, char * fnamenetcdf_U, char * fnamenetcdf_V, char * fname)
{	int nval_time;
	double * timeV;

	if (fileExists(fnamenetcdf_H))
	{
		timeV=(double *) calloc(1,sizeof(double));
		NETCDF_ReadTime( fnamenetcdf_H,prefix, 0,  timeV,  &nval_time );
		timeV=(double*) calloc(nval_time,sizeof(double));
		NETCDF_ReadTime( fnamenetcdf_H,prefix, 1,  timeV,  &nval_time );
	}
	if (nval_time>0)
	{
		FILE *outfile;outfile = fopen(fname, "w");
		fprintf(outfile,"*time            HA          U           V       IDTime\n");
		fprintf(outfile,"*list of files for current case %s\n",fname);
		int k;
		for(k=0;k<nval_time;k++)
		{
			fprintf(outfile,"%f %s ",timeV[k],fnamenetcdf_H);
			if (fileExists(fnamenetcdf_U))
				fprintf(outfile," %s",fnamenetcdf_U);
			if (fileExists(fnamenetcdf_V))
				fprintf(outfile," %s", fnamenetcdf_V);
			fprintf(outfile," %i\n",k);
		}
		fclose(outfile);	

	}
	delete [] timeV;timeV=NULL;
}

void mergeRasters(float h[(NROWS+1)][(NCOLS+1)], float lonmin, float lonmax, float latmin, float latmax,int nx, int ny, double NoDataL,
			 	  float hSmall[(NROWS+1)][(NCOLS+1)], float lonminS, float lonmaxS, float latminS, float latmaxS,int nxS, int nyS, double NoDataS)
 {  // h is the large and hSmall is the small. 
	// loop to read all the lat/lon from Large in the window small and retrieve the small window related values

	// 1. search ix1,ix2, iy1,iy2 from the large window corresponding to the small window
	// 2. iterate to determine the ix,iy in the small corresponding to the centre of the cells in the large 
	// 3. substitute the value into the large
	int ix1,ix2,iy1,iy2;
	float lon,lat, dxCell,dxCellS, dyCell,dyCellS;
	dxCell =(lonmax -lonmin) /(float) nx;
	dyCell =(latmax -latmin) /(float) ny;
	dxCellS=(lonmaxS-lonminS)/(float) nxS;
	dyCellS=(latmaxS-latminS)/(float) nyS;

	// 1. search ix1,ix2, iy1,iy2 from the large window corresponding to the small window
	ix1=(int) (((-lonmin+lonminS))/(lonmax-lonmin)*(float) nx)+1;
	ix2=(int) (((-lonmin+lonmaxS))/(lonmax-lonmin)*(float) nx)-1;
	iy1=(int) (((-latmin+latminS))/(latmax-latmin)*(float) ny)+1;
	iy2=(int) (((-latmin+latmaxS))/(latmax-latmin)*(float) ny)-1;

	// 2. iterate to determine the ix,iy in the small corresponding to the centre of the cells in the large 
	int ix,iy,ixS,iyS;
	for (ix=ix1;ix<=ix2;ix++)
	{
		for (iy=iy1;iy<=iy2;iy++)
		{
			lon=lonmin+(float)(ix)*(lonmax-lonmin)/(float) nx+dxCell/2.;
			ixS=(int)((float)((lon-lonminS)/(lonmaxS-lonminS)* (float) nxS));
			lat=latmin+(float)(iy)*(latmax-latmin)/(float)ny+dyCell/2.;
			iyS=(int)((float)((lat-latminS)/(latmaxS-latminS)* (float) nyS));
			// 3. substitute the value into the large only if is not NoDataValue
			if (hSmall[iyS][ixS] != NoDataS && NoDataS != -1)
			h[iy][ix]=hSmall[iyS][ixS];
		}
	}

}

float getMultipleTime(float dtSec, float TmaxMin)
{
	int i,J;
	float resto;
	float mult=1.;
	if(dtSec<1) mult=10.;
	int N= (int) ((float)(dtSec*mult));
	for (J=1;J<N;J++)
	{
		i=N-J+1;
		resto=TmaxMin*60/i - (int)(TmaxMin*60/i);
		if ( resto<=1.e-9)
			return (float) i/mult;
		
	}
	return 1./mult;
}

void cleanValuesTT(float TarrH[(NROWS+1)][(NCOLS+1)],float mh[(NROWS+1)][(NCOLS+1)],float d[(NROWS+1)][(NCOLS+1)],  float minDepthTT,float setpoint,int ncols, int nrows, float NODATA)
{
	int i,j;

	printf("cleanValuesTT: minDepthWP=%f  minDepthTT=%f setpoint=%f \n",minDepthTT,setpoint);

	for (i=2;i<=ncols-1;i++)
	{
		for (j=2;j<=nrows-1;j++)
			if (TarrH[j][i]!=NODATA && (fabs(mh[j][i])<setpoint) || d[j][i]<minDepthTT )
				TarrH[j][i]=NODATA;
	}
}

void cleanGF(float TarrH[(NROWS+1)][(NCOLS+1)],float dx0,float dy0,float d[(NROWS+1)][(NCOLS+1)], float minDepthTT,float setpoint,int ip2, int jp2, float NODATA)
{
	int i,j;
	float slope, sonic, sx, sy;
	float gz=9.81, EarthRadius = 6340000.;
//	float radius = EarthRadius * (float) cos((y1 + y2) / 2. / 180. * 3.14);
	float radius = EarthRadius ;
	float dx = (float) (dx0 / 180. * 3.1415927 * radius);
	float dy = (float) (dy0 / 180. * 3.1415927 * EarthRadius);

	printf("cleanValuesTT1:   minDepthTT=%f setpoint=%f \n",minDepthTT,setpoint);

	for (i=2;i<=ip2-1;i++)
	{
		for (j=2;j<=jp2-1;j++)
		{
			if ( TarrH[j][i]!=NODATA && TarrH[j][i-1]!=NODATA && TarrH[j][i+1]!=NODATA && TarrH[j-1][i]!=NODATA && TarrH[j+1][i]!=NODATA && (d[j][i]>minDepthTT) ) 
			{
			sx = ((TarrH[j][i-1]-TarrH[j][i+1])/(dx*2)) ;
			sy = ((TarrH[j-1][i]-TarrH[j+1][i])/(dy*2)) ; 
			slope = sqrt( sx*sx + sy*sy ) ; 
			sonic = sqrt(d[j][i]*gz) ;
				if (1./MAX(slope*3600.,1.) <= sonic*setpoint) TarrH[j][i]=NODATA;
			} 		

		}
	}
}


#ifdef WINDOWS

#include <windows.h>  
#include <winuser.h>	// for Windows APIs too	[don't remove !!]

void MeasureMemory(long * Phys, long * Virt, long * totMem)
{
	_MEMORYSTATUS MS;
	GlobalMemoryStatus (&MS);
	*totMem=MS.dwMemoryLoad ;
	*Phys=MS.dwAvailPhys ;
	*Virt=MS.dwAvailVirtual ;
//	printf("Phys: %i   Virt: %i   tot: %i\n",*Phys,*Virt,*totMem);

}



#endif
