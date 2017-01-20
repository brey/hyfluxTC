#!/bin/bash 
#-------------------------------------------------
. $procDir/functions.sh
export cmdChain=$cmdChain/`CMD $0`
#-------------------------------------------------

calculation="$*"

#-----------------------------------------------------
#   copy results to destPath
#-----------------------------------------------------
cd $hurDir

if [ -e $hurDir/noPublishWeb.txt ] ; then 
echo \# NOT published on $destPath/$toBul  $calculation because exist file $hurDir/noPublishWeb.txt >> $workStatus
exit 0 ; fi
 
#............................................... 
pubDir=$destPath/final
pubDirNew=$destPath/final-new
 
if [ ! -d $pubDirNew ] ; then NVSexecute mkdir -p $pubDirNew ; fi
dir=$pubDirNew
  if [ `check_mkdir $dir` -ne 1 ] ; then echo Problems in mkdir $dir ; fi

cp -v $finalDir/* $pubDirNew
cp -rv $advDir $pubDirNew

rm -f $pubDirNew/TIF_H_* 
rm -rf $pubDir

NVSexecute mv $pubDirNew $pubDir
dir=$pubDir
  if [ `check_mkdir $dir` -ne 1 ] ; then echo Problems in mv $pubDirNew $pubDir ; fi

#............................................... 
dir=$destPath/$toBul/final
if [ -d $dir ] ; then rm -rf $dir ; fi
NVSexecute mkdir -p $dir 
  if [ `check_mkdir $dir` -ne 1 ] ; then echo Problems in mkdir $dir ; fi
cp -rvf $pubDir $destPath/$toBul
NVSexecute rm -rf $dir/NETCDF* $dir/nestedFolders.zip

#............................................... 

exclude="*/demSea* */zwmax* */vnmax* */u10max* */deprmax*"
unzip -nq $destPath/final/locations.kmz -d $destPath/final -x $exclude 

#..............................................
cd $destPath/final
if [ -f $hurDir/detailedCalc.txt ] ; then
NVSexecute unzip -nq nestedFolders.zip

else
for window in $windowS ; do
  for file in P1_MAXHEIGHT_END.jpg  inundationDepth.tif  inundationDist.tif  inundationFrac.tif  inundationHeight.tif  inundationPop.tif  inundationVel.tif  ; do
  NVSexecute unzip -nq nestedFolders.zip $window/$file
  done
done
fi

#creation of a light kmz
echoS creating locationsLight.kmz
echoS cp $destPath/final/locations.kmz $destPath/final/locationsLight.kmz
cp $destPath/final/locations.kmz $destPath/final/locationsLight.kmz

echoS NVSexecute zip -q --delete $destPath/final/locationsLight.kmz "watch/*"
NVSexecute zip -q --delete $destPath/final/locationsLight.kmz "watch/*"



cd $hurDir
#..............................................
echoS published bulletin $toBul on $destPath/final: elaps from start [hh:mm:ss] `elaps.sh `  $calculation
echo PUBLISHED date=\"`date`\" runID=$runID   $calculation toBul=$toBul > $runStatus
echo PUBLISHED date=\"`date`\" runID=$runID   $calculation > $destStatus


cp $workStatus $destPath

