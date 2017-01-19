#!/bin/bash

#-------------------------------------------------
. $procDir/functions.sh
export cmdChain=$cmdChain/`CMD $0`
#-------------------------------------------------
 
if [ $# -ne 5 ] ; then
echo enter $0  window workDir finalDir windowToClean lastcc
echo window: currect calculatin window
echo workDir: path to the folder of the calculated window
echo finalDir: path to the folder of the final products
echo windowToClean:  windows in workDir to remove
echo lastcc: condition code of last calculation: if lastcc ne 0 the merge map is not performed 
fi

echo external variables:
declare -p calcType procDir keepNETCDF tmpClean prg firstWin
if [ $? -ne 0 ] ; then exit 1 ; fi

 
export window=$1
export workDir=$2
export finalDir=$3
export windowToClean=$4
export lastcc=$5 

export win=copy
 
declare -p window workDir finalDir windowToClean lastcc
 
#-------------------------------------------------
 
if [ ! -d $finalDir/watch ] ; then mkdir -p $finalDir/watch ; fi
if [ ! -d $finalDir/$window ] ; then mkdir -p $finalDir/$window ; fi

cd $workDir
echo
echo +++++++++++++++++++++++++++++++++++++++
echo first  window
echo files needed for mergeMap \& postProcessing
echo +++++++++++++++++++++++++++++++++++++++

if [ $window = $firstWin ] ; then

fileS="$input listfiles.txt bathymetry.tif outres1.gif NETCDF_H.nc NETCDF_H.nxc P1_MAXHEIGHT_END.jpg  P1_MAXHEIGHT_END.png  TIF_MAXHEIGHT_END.tif coastalImpactMax.png coastalImpactMax.tif  "

  if [ "$calcType" = TSUNAMI ] ; then fileS="$fileS base.jpg deformation.tif" 
    if [ "$tttFirstWin" = yes ] ; then fileS="$fileS ArrTime_15min ArrTime_5min ArrTime_60min  \
    Tarrival_15min.kml  Tarrival_5min.kml  Tarrival_60min.kml \
    TIF_Arrival_Time_notCleaned.tif \
    TIF_Arrival_Time.tif TIF_Maximum_Time.tif "
    fi
  fi

# generate in any case a cellBound.map and put in finalDir: later it will be copyed from work folder (if exist) 
TSexecute vpl -i $procDir/doMap.pr $window/bathymetry.tif $finalDir/$window/cellBound.map 
 
for file in $fileS ; do 
cp -uvrf $window/$file $finalDir
done

echo Done copy firstWin = $window

fi


echo
echo +++++++++++++++++++++++++++++++++++++++
echo all windows files for nestedFolders
echo +++++++++++++++++++++++++++++++++++++++ 
 

echo ---------------------------------------------------
echo TAT Files  \(SWAN \& HYFLUX \& postProcess \)
echo ---------------------------------------------------
 
tatFile="$input maxValues.txt locations.txt locations.xml locations.kml base.jpg P1_MAXHEIGHT_END.png  P1_MAXHEIGHT_END.jpg coastalImpactMax.png coastalImpactMax.tif   \
 	TIF_MAXHEIGHT_END.tif bathymetry.tif WGFiles.tar WTFiles.tar " 
  if [ "$keepNETCDF" = yes ] ; then
  tatFile="$tatFile NETCDF_H.nc NETCDF_H.nxc  "
  fi
 
for file in $tatFile  ; do
  if [ -e $window/$file ] ; then cp -v $window/$file $finalDir/$window ; fi
done

  if [ "$calcType" = TSUNAMI ] ; then cp -v $window/deformation* $finalDir/$window  ; fi
cp -v $window/coast* $finalDir/$window  

echo
echo ---------------------------------------------------
echo  HYFLUX 
echo ---------------------------------------------------
 
for file in  console.txt parameters.txt monitor.txt logout.txt maxHeight0.png ; do
  if [ -e $window/$file ] ; then cp -v $window/$file $finalDir/$window  ; fi
done

  if [ -e $window/logout.txt ] ; then cp -v $window/logout.txt logout-$window.txt ; fi

# copy also in case that the calculation fail and/or no post-processing are available 


cp -vr $window/work/inundation* $finalDir/$window  
cp -vr $window/work/imageData.sh  $finalDir/$window  
cp -vr $window/work/*kmz   $finalDir/$window  

cp -vr $window/work/watch   $finalDir  
cp -vr $window/work/zsurf*png   $finalDir/watch

workS=" mask.map demSea.map popmap.map distShore.map shoreline.map cellBound.map zminv.map 
zsurf000.000.map wetfrac.map depth.map zsurf.map  slopev.map \
watch.txt summary.txt \
vnmax.map \
tarr.map \
zwmax.map hzwmax.map  tzwmax.map  wzwmax.map dhjv.map "

if [ "$calcType" = CYCLONE ] ; then workS="$workS deprmax.map u10max.map" ; fi
if [ "$calcType" = TSUNAMI ] ; then workS="$workS fault.map" ; fi

for map in $workS ; do
cp -v $window/work/$map*  $finalDir/$window
done
 
echo Done copy of $window
 
echo
echo -------------------------------------------------
echo  merge \(or copy\) H maps if lastcc==0  \(nested calculations that do not fail\)
echo -------------------------------------------------

if [ $lastcc -eq 0 ] ; then
  while read line ; do
    if [ "$line" = "" ] ; then continue ; fi
  c=`echo $line  | cut -b 1`         
    if [ "$c" = "*" ] ; then continue ; fi
  
  file=`echo $line  | cut -d' ' -f 2 `         
  
  large=$finalDir/$file 
  small=$window/$file   
    if [ -e $small ] ; then
      if [ -e $large ] ; then
      NVSexecute mergeMap -l $large -s $small  -o $large 
      else 
      cp -v $small $large
      fi
    fi  
  done  < $finalDir/listfiles.txt

echo done merge maps

else
echo NOT done merge maps
fi

echo
echo -------------------------------------------------
echo  tmpClean: clean the working directories
echo -------------------------------------------------
if [ "$tmpClean" = yes ] ; then
  if [ -e  $window/work ] ; then 
  rm -rf $window/work 
  	if [ -e $finalDir/$window/inundation.txt ] ; then 
  	mkdir $window/work
  	cp -v $finalDir/$window/inundation.txt $window/work
        fi
  fi
fi

#-------------------------------------------------
# windowToClean: clean the entire directoriy (nested window yet processed)
#-------------------------------------------------
if [ -d $windowToClean ] ; then NVSexecute rm -rf $windowToClean ; fi
