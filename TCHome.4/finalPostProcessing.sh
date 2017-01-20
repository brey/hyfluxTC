#!/bin/bash 


#-------------------------------------------------
. functions.sh
export cmdChain=$cmdChain/`CMD $0`
#-------------------------------------------------
# env variables

# prg
# firstWin  otherWin windowS input procDir someError keepNETCDF
#  workDir finalDir 

declare -p workDir finalDir
	if [ ! -d $finalDir/watch ] ; then mkdir -p $finalDir/watch ; fi

cd $finalDir

#-------------------------------------------------
#post processing
#-------------------------------------------------
if [ "$tmpPublish" = no ] ; then
  TSexecute postTimeRanges.sh
  	if [ $? -ne 0 ] ; then exit 1 ; fi
  TSexecute $prg/postProcessing -i $input -o .  -opt rel  -code $code  -inundation true  -tifuv2netcdf false 
fi
  
#-------------------------------------------------
# coastal impact
#-------------------------------------------------
TSexecute $prg/coastalImpact -i $input -o .  -opt rel   
TSexecute $prg/coastalImpact -i $input -o .  -opt rel -admin true

#NVSexecute rm -f coastalImpactMax.bmp.aux.xml coastalImpact.png coastalImpact.bmp.aux.xml coastalImpact.tif coastalIndex.tif clipFileCI.tif
NVSexecute rm -f coastalImpactMax.bmp.aux.xml coastalImpact.bmp.aux.xml coastalIndex.tif clipFileCI.tif
cp -v locations.txt locations_merge.txt 
cp -v locations.xml locations_merge.xml 
cp -v locations.kml locations_merge.kml 
	if [  "$keepNETCDF" != yes ] ; then NVSexecute rm -rf TIF_H_*.tif   ; fi

#-------------------------------------------------
# calcLimit map
#-------------------------------------------------
calcLimit=
for window in $windowS ; do
calcLimit="$calcLimit $window/cellBound.map"
done

NVSexecute map2image -i $calcLimit -pixsize 1 -o calcLimit.png -doLegend no no no no no  
	if [ $? -ne 0 ] ; then exit 1 ; fi
NVSexecute convert -composite P1_MAXHEIGHT_END.jpg calcLimit.png P1_MAXHEIGHT_END_LIMITS.jpg 
	if [ $? -ne 0 ] ; then exit 1 ; fi
NVSexecute rm $calcLimit

#-------------------------------------------------
# remove un-necessary files
#-------------------------------------------------
cd $finalDir
for file in cellBound.map ArrTime_15min  ArrTime_5min Tarrival_15min.kml Tarrival_5min.kml ; do 
if [ -e $file ] ; then rm -rf $file ; fi
done 

#-------------------------------------------------
# compress &  clean
#-------------------------------------------------
cd $finalDir
if [ "$finalClean" = yes ] ; then 
NVSexecute rm -rf */NETCDF_H.nc
zip -r nestedFolders.zip $windowS watch/*.txt  1> /dev/null
NVSexecute rm -rf $windowS watch  Tarrival_60min.kml 
NVSexecute rm -rf old_* 

else
zip -r nestedFolders.zip $windowS watch/*.txt  1> /dev/null
fi 

#-------------------------------------------------
# publish
#-------------------------------------------------
SSexecute publishWeb.sh "final postProcessing"
