source /home/critechuser/cycloneSurge/bash_profile
#-----------------------------------------------------
#if [ "$MPL_HOME" = "" ] ; then . ~/operational/setoper.sh ; fi
source $procDir/functions.sh
source $procDir/functionsLod.sh
#-----------------------------------------------------
function MRexecute ()
{
echo
echo ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
echo `pwd`\> ${@:1:$#-1} \&\> `htmlOut ${@:$#}`
echo ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
${@:1:$#-1} &> ${@:$#}
ccexec=$?
echo done  $cmdChain/`CMD ${@:1:$#-1} ` 
echo ..............................................................
echo
echoStep  "${@:1:$#-1} " ${@:$#}
return $ccexec
}

function echoS ()
{
echo $* 
echo \# $*  >> $workStatus
}
export -f echoS
#-----------------------------------------------------
function echoT () 
{
echo \	$* 
}
#-----------------------------------------------------
function checkExitCode ()
{
if [ $tmpFolder = yes ] ; then NVSexecute cp -r $workDir $finalDir $hurDir ; fi
if [ $tmpAdvDir = yes ] ; then NVSexecute cp -r $advDir $hurDir ; fi
if [ "$finalClean" = yes -a "$tmpFolder" = yes ] ; then NVSexecute rm -rf $calcDir ; fi
#.........................
if   [ $1 -gt 0 ] ; 	then flag=FAILED; cc=1
elif [ $1 -eq 0 ] ; 	then flag=COMPLETED; cc=0
elif [ $1 -lt 0 ] ; 	then flag=IGNORED; cc=0
fi
echo $flag on date=\"`date`\"  $2
echo $flag on date=\"`date`\"   $2 > $destStatus
echo $flag on date=\"`date`\"   $2 > $runStatus.$flag
echo export STATUS=$flag \#   $2 >> $workStatus
echoS .......................................................
echoS
cp $workStatus $destPath
exit $cc
}

function elaps ()
{

t1=`date --date="$date1" +%s` 
t2=`date --date="$date2" +%s` 
let dt=t2-t1
let hh=dt/3600
mm=`echo " ( $dt - $hh * 3600 ) / 60 " | bc `
ss=`echo " ( $dt - $hh * 3600 - $mm * 60 )  " | bc `
if [ $mm -le 9 ] ; then mm="0$mm" ; fi
if [ $ss -le 9 ] ; then ss="0$ss" ; fi

echo ${hh}:${mm}:${ss}
}

function restFile ()
{
#-------------------------------------------------
#-------------------------------------------------

win=$1
lastwin=$2
clonetif=$3
clonemap=$4


# env variable:

# in: path of the previous run 
# out: path of the current run

in=$lastWorkDir/$lastwin
out=$win

declare -p win lastwin clenetif clonemap in out maskSea

if [ ! -d $out/work/watch ] ; then mkdir -p $out/work/watch ; fi

if [ "$maskSea" = yes ] ; then
export batinp=$in/bathymetry.tif
export batout=$out/bathymetry.tif
export maskmap=$out/masksea.tif
mask="-mask yes"

NVSexecute resmap -clone $batout -i $batinp -o $maskmap -int 0

#-------------------------------------------------
python $pythonDir/maskmap.py $batout $maskmap
#vpl << EOF &> /dev/null

#getmap out $batout
#getmap mask $maskmap
#landin = (.not.eqmv(mask).and.mask.lt.0)
#landout = (.not.eqmv(out).and.out.lt.0)
#%m=(.not.landin.and.landout).or.(.not.landout.and.landin)
#%m=landin.or.landout
#m=landout

#if (sum(m).gt.0) then
#l=getm(m,do(size(m)))
#mask_l = MV
#endif

#putmap mask $maskmap
#EOF

#-------------------------------------------------

if [ $? -ne 0 ] ; then echo error from vpl ; exit 1 ; fi

else
export maskmap=$out/bathymetry.tif
mask="-mask no"
fi

if [ "$in" != "$out" ] ; then
for file in cpus.txt listfiles.txt monitor.txt ; do
NVSexecute cp $in/$file $out
done
fi

#---------------------------------------------

let time=0
ext="00000000"
#while [ $time -lt $timestart ] ; do
while (( $(bc <<< "$time < $timestart") == 1 )) ; do
map=TIF_H_$ext.tif
if [ -f $in/$map  ] ; then 
NVSexecute resmap -clone $maskmap $mask -cloneFmt $clonetif -setdata 0 -i $in/$map -o $out/$map -int 0 
fi
if [ $? -ne 0 ] ; then echo error on resampling $map ; exit 1 ; fi
#let time=time+dtout
time=`echo "$time + $dtout" | bc -l`
ext0=`echo "$time + 100000000 " | bc -l `
ext=`echo $ext0 | cut -b 2-9`
done

#---------------------------------------------
echo timestart=$timestart ext=$ext

for map in  TIF_H_$ext.tif TIF_U_$ext.tif  TIF_V_$ext.tif ; do
if [ -f $in/$map  ] ; then 
NVSexecute resmap -clone  $maskmap $mask -cloneFmt $clonetif  -setdata 0 -i $in/$map -o $out/$map -int 0 
if [ $? -ne 0 ] ; then echo error on resampling $map ; exit 1 ; fi
fi
done


in=$in/work
out=$out/work

if [ $in != $out ] ; then
cp  -R $in/watch  $out
rm $out/watch/*statistics.txt
fi

for file in deprmax hzwmax tarr tzwmax u10max vnmax wzwmax zwmax ; do
map=$file$ext.map
if [ -f $in/$map ] ; then 
NVSexecute resmap -clone  $clonemap -i $in/$map -o $out/$map -int 0
fi
if [ $? -ne 0 ] ; then echo error on resampling $map ; exit 1 ; fi
done 


}


function mergeloc ()
{
#-------------------------------------------------
#-------------------------------------------------

# imported variables

declare -p nested procDir windowS
if [ $? -ne 0 ] ; then echo Exit because some variables are not defined ; exit 1 ; fi

#-------------------------------------------------
# merge locations
#-------------------------------------------------
cd $finalDir

inundationS=
locationS=

rm -f inundation.txt

for window in $windowS ; do	
if [ -e $window/inundation.txt ] ; then inundationS=" $inundationS $window/inundation.txt "  ; fi
if [ -e $window/locations.txt ] ; then locationS=" $locationS $window/locations.txt " ; fi 
done

#-------------------------------------


imgFile=imgFile.txt

postMaps="P1_MAXHEIGHT_END coastalImpactMax"

if [ "$calcType" = TSUNAMI ] ; then calcMaps="deformation.tif" 
elif [ "$calcType" = CYCLONE ] ; then calcMaps="deprmax.map u10max.map"
else
calcMaps=
fi
 
mapS="$postMaps \
shoreline.map \
$calcMaps  \
inundationFrac.tif inundationPop.tif inundationDist.tif inundationHeight.tif \
popmap.map demSea.map zwmax.map vnmax.map "

declare -p STATUS postMaps calcMaps mapS  windowS
echo --------------------------------------


let nw=`nitem $windowS`

echo "nameFolder	endFolder	nameImage	north	south	west	east	minLod	maxLod \
	nameRasterMap	outRasterMap	description	visibility " > $imgFile


#-----------------------------------
# loop over maps

let imap=0
for map in $mapS ; do
let imap=imap+1
if [ $imap -eq 1 ] ; then visibility=1 
		else visibility=0 ; fi


#-----------------------------------
#loop done only to evaluate first and last window map

let iw=0
let nlast=0
let nfirst=0
for window in $windowS ; do
let iw=iw+1
what=`item "$STATUS" $iw`
if [ "$what" = DONE ] ; then

post=no
for postMap in $postMaps  deformation.tif  ; do
	if [ $map = $postMap ] ; then post=yes ; fi
done 

if [ $iw -eq 1 -a $post = yes ] ; then nameRasterMap=$map.png 
else  nameRasterMap=$window/$map.png ; fi 

if [ -e $nameRasterMap ] ; then let nlast=iw  
if [ $nfirst -eq 0 ] ; then let nfirst=iw ; fi
fi

fi

done

#-----------------------------------
# loop over windows


let iw=0
for window in $windowS ; do
let iw=iw+1
export code=`item "$codeS" $iw`
what=`item "$STATUS" $iw`

if [ "$what" = DONE ] ; then

post=no
for postMap in $postMaps  deformation.tif ; do
	if [ $map = $postMap ] ; then post=yes ; fi
done 

if [ $iw -eq 1 -a $post = yes ] ; then nameRasterMap=$map.png 
else  nameRasterMap=$window/$map.png ; fi 


export batgrid=`item "$batgridS" $iw`
export finergrid=`item "$finergridS" $iw`

if [ ! -e $window/imageData.sh ] ; then
NVSexecute mergeLoc -i $window/$input \
      -iProc $procDir/imageData.sh \
      -oProc $window/imageData.sh
fi

. $window/imageData.sh


post=no
for postMap in $postMaps deformation.tif ; do
	if [ $map = $postMap ] ; then post=yes ; fi
done 

if [ $post = no -a $iw -eq 1 ] ; then export west=$westl ; fi


nameImage=$window

if [ -e $nameRasterMap ] ; then

if [ $iw -eq $nfirst ] ; then nameFolder=$map
else nameFolder=NaN ; fi

if [ $iw -eq $nlast ] ; then endFolder='</Folder>'
else endFolder=NaN ; fi

minLod=`minLodFun`; maxLod=`maxLodFun`

echo "$nameFolder	$endFolder	$nameImage	$north	$south	$west	$east	$minLod	$maxLod \
	$nameRasterMap	none	NaN	$visibility " >> $imgFile

elif [ $code = HYFLUX -a $map != 'P1_MAXHEIGHT_END' ] ; then
echo map $nameRasterMap not exist
fi

fi

done  # windowS loop

done  # maps loop 

#-------------------------------------
#  images.kml , locations.xml
#-------------------------------------

if [ $nested = yes ] ; 	then large="locations_merge.txt" 
			else large="locations.txt"  ; fi

if [ "$calcType" = TSUNAMI ] ; then
NVSexecute mergeLoc -i $input -l $large -s $locationS -w watch \
      -img $imgFile \
      -iProc $procDir/bodyImages.kml  $procDir/bodyLoc.xml \
      -oProc locations.kml locations.xml  \
      -sortedby tMaxHei  -maxHeightmin 0.1 \
      -o locations.txt 
elif [ "$calcType" = CYCLONE ] ; then
NVSexecute mergeLoc -i $input -l $large -s $locationS -w watch \
      -img $imgFile \
      -iProc $cycloneDir/bodyImages.kml  $cycloneDir/bodyLoc.xml \
      -oProc locations.kml locations.xml  \
      -sortedby -MaxHei  -maxHeightmin 0.1 \
      -o locations.txt 
fi

maxzmax=`getField -i locations.xml -f maxzmax`
if [ "$maxzmax" = "" ] ; then let maxzmax=0 ; fi
echo maxzmax=$maxzmax

#-------------------------------------
#  interval.kml 
#-------------------------------------

maxHeightminS="4   3 2 1  0.1 0.01"
maxHeightmaxS="100 4 3 2  1   0.1"

for interval in 1 2 3 4 5 6 ; do
export maxHeightmin=`item "$maxHeightminS" $interval` 
export maxHeightmax=`item "$maxHeightmaxS" $interval` 

cond=$(echo "$maxzmax >= $maxHeightmin " | bc -q 2>/dev/null)
echo interval $maxHeightmin $maxHeightmax cond=$cond

if [ $cond -eq 1 ] ; then 

if [ $interval -eq 1 ] ; then
export intLabel=" > $maxHeightmin "
else 
export intLabel="$maxHeightmin to $maxHeightmax"
fi

if [ $interval -gt 4 ] ; then
export visibility=0
else
export visibility=1
fi

NVSexecute mergeLoc -i $input -l locations.txt -w watch \
      -sortedby -MaxHei \
      -maxHeightmin $maxHeightmin -maxHeightmax $maxHeightmax \
      -iProc $procDir/bodyLocations.kml  \
      -oProc interval.kml \
      -o interval_${maxHeightmin}_${maxHeightmax}.txt
cat interval.kml >> locations.kml
fi
done

#-------------------------------------
#  inundations.kml 
#-------------------------------------
if [ "$inundationS" != "" ] ; then
NVSexecute mergeLoc -i $input -l locations.txt -s $inundationS -w watch \
      -sortedby -simDistRunup \
      -iProc $procDir/bodyInundations.kml   \
      -oProc inundations.kml \
      -o inundation.txt
cat inundations.kml >> locations.kml 
fi


if [ "$calcType" = TSUNAMI ] ; then
cat $procDir/bodyArrivals.kml >> locations.kml 
elif [ "$calcType" = CYCLONE ] ; then
cat $cycloneDir/bodyArrivals.kml >> locations.kml 
fi

if [ -e locations.kmz ] ; then rm locations.kmz ; fi

zip -ru locations.kmz locations.kml *png */*png Tarrival*.kml 1> /dev/null

if [ "$finalClean" = yes ] ; then rm -rf *.kml interval*  ; fi
 
}

function postTimeRanges ()
{
#-------------------------------------------------
#-------------------------------------------------

# env. variables
declare -p timeShift FinTime hgrey input code prg
if [ $? -ne 0 ] ; then exit 1 ; fi
#-----------------------------------------------------------------
# post-processing of selected time ranges. output:  locations_$ext
#-------------------------------------------------

time=$timeShift
# while [ $time -le $FinTime ] ; do
  while (( $(bc <<< "$time <= $FinTime") == 1 )) ; do
# let timeh=time-timeShift
  timeh=`echo "$time-$timeShift" | bc -l`
  # if [ $time -gt 0 ] ; then
    if (( $(bc <<< "$time > 0") == 1 )) ; then
    time1=`echo " ( $time - 6 ) * 3600  " | bc -l `
    time2=`echo " $time * 3600  " | bc -l `   
    # if [ $timeh -lt 100 ] ; then
      if (( $(bc <<< "$timeh < 100") == 1 )) ; then
      ext0=`echo "$timeh + 100 " | bc -l `
      ext=`echo $ext0 | cut -b 2-3`
      else
      ext=$timeh
      fi
NVSexecute $prg/postProcessing -i $input -o .  -opt rel  -code $code -inundation true -time1 $time1 -time2 $time2 -locFileName locations_$ext -onlyLocations true  -tifuv2netcdf false 
      if [ $? -ne 0 ] ; then exit 1 ; fi
      if [ -e locations_$ext.kml ] ; then rm locations_$ext.kml ; fi
    fi
# let time=time+6
  time=`echo "$time+6" | bc -l`
  done
}

function publishWeb ()
{
#-------------------------------------------------
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

# copy the run folders from calc
for f in $workDir/* ; do
   if [[ -d  $f ]] ; then  
     cp -rv $f $pubDirNew
   fi
done

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
echoS published bulletin $toBul on $destPath/final: elaps from start [hh:mm:ss] `elaps `  $calculation
echo PUBLISHED date=\"`date`\" runID=$runID   $calculation toBul=$toBul > $runStatus
echo PUBLISHED date=\"`date`\" runID=$runID   $calculation > $destStatus


cp $workStatus $destPath

}

function finalPostProcessing ()
{

#-------------------------------------------------
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
  TSexecute postTimeRanges
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
SSexecute publishWeb "final postProcessing"
}


function merge ()
{

#-------------------------------------------------
#-------------------------------------------------

nestedWin="$*"

echo
echo +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
echo merge locations of selected time ranges: ouput  locations_\$ext.xml 
echo +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

let maxzmax=0
time=$timeShift

#while [ $time -le $FinTime ] ; do
while (( $(bc <<< "$time <= $FinTime") == 1 )) ; do

#let timeh=time-timeShift
timeh=`echo "$time-$timeShift" | bc -l`
#if [ $time -gt 0 ] ; then
if (( $(bc <<< "$time > 0") == 1 )) ; then
ext0=`echo "$timeh + 100 " | bc -l `
ext=`echo $ext0 | cut -b 2-3`


if [ "$nestedWin" != "" ] ; then
smallLoc="-s"
	for win in $nestedWin ; do
	smallLoc="$smallLoc $workDir/$win/locations_$ext.txt"
	done
else
smallLoc=""
fi

NVSexecute mergeLoc   -i $input -l $workDir/$firstWin/locations_$ext.txt $smallLoc -iProc $cycloneDir/bodyLoc.xml -oProc locations_$ext.xml -o locations_$ext.txt -sortedby tMaxHei -maxHeightmin 0.05 
if [ $? -ne 0 ] ; then exit 1 ; fi

zwmax=`getField -i locations_$ext.xml -f maxzmax ` 
if [ "$zwmax" = "" ] ; then zwmax=0 ; fi
maxzmax=`max $zwmax $maxzmax`

# echo ext=$ext zwmax=$zwmax maxzmax=$maxzmax

# if [ -e locations_$ext.txt ] ; then rm locations_$ext.txt ; fi

fi  # end time test


#let time=time+6
time=`echo "$time+6" | bc -l`
done  # while [ $time -le $FinTime ] 

echo maxzmax=$maxzmax 'int(maxzmax)'=`int $maxzmax`

#-------------------------------------------------
# STATUS
#-------------------------------------------------

STATUS=
for window in $windowS ; do
if [ -e $window/maxValues.txt ] ; then STATUS="$STATUS DONE" ; else STATUS="$STATUS notDONE" ; fi
done
export STATUS


echo
echo +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
echo  merge locations of all time range 
echo +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SSexecute mergeloc 




echo +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
echo  merge tar files 
echo +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if [ "$nestedWin" != "" ] ; then 

prefixS="WG WT"

if [ -d temp ] ; then rm -rf temp ; fi
mkdir temp

# untar

for prefix in $prefixS ; do
tar -xf ${prefix}Files.tar -C temp/
mv ${prefix}Files.tar old_${prefix}Files.tar
done

for window in $windowS ; do
for prefix in  $prefixS ; do
tarfile=$window/${prefix}Files.tar
if [ -e $tarfile ] ; then tar -xf  $tarfile -C temp/ ; fi
done
done

cd temp

# tar again

for prefix in $prefixS ; do
tar -cf $finalDir/${prefix}Files.tar ${prefix}_* -C ./
done

cd $finalDir

rm -rf temp
fi

}


function copy ()
{
#-------------------------------------------------
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

#export win=copy
 
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
#TSexecute vpl -i $procDir/doMap.pr $window/bathymetry.tif $finalDir/$window/cellBound.map 
TSexecute python $pythonDir/doMap.py $window/bathymetry.tif $finalDir/$window/cellBound.map # $window/work/demSea.map
 
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
}

function execr ()
{

#-------------------------------------------------
#set -x
#trap read debug
#-------------------------------------------------

# env variables

# prg cycloneDir

# execr  $input  $outDir $folder $lastWin

input=$1
outDir=$2
folder=$3
lastWin=$4


export win=`basename $outDir`

# env variable:

#  prg: 	path to swan codes
#  hgrey: 	hgrey parameter
#  prefixR: 	if parallel processing (ncore > 1)
#  other_flags & flags: directive to HyFlux2 for nested simulations
# RESTART:	yes/no  (if yes, restFile is done)
# lastWorkDir:	path of the previous run   (used by restFile)

if [ ! -e $outDir/$input ] ; then 
echo Not exist $outDir/$input
exit
fi

######################################################
# pre_processing
######################################################
TSexecute HyFlux2.1 -i $input -w $outDir/work -o $outDir  $other_flags -onlyDem yes
	if [ $? -ne 0 ] ; then exit 1 ; fi
NVSexecute mv $outDir/console.txt $outDir/parameters.txt $outDir/work

######################################################
# resample maps for restart
######################################################
declare -p RESTART lastWorkDir lastWin
  if [ "$RESTART" = yes ] ; then 
    if [ -e $lastWorkDir/$lastWin/bathymetry.tif -a -e $lastWorkDir/$lastWin/logout.txt ] ; then
echo    STATUS=\`getField  -i $lastWorkDir/$lastWin/logout.txt -f STATUS \`
    STATUS=`getField  -i $lastWorkDir/$lastWin/logout.txt -f STATUS `
    echo STATUS=$STATUS
      if [ "$STATUS" = complete ] ; then
      export clonetif=$outDir/bathymetry.tif
      export clonemap=$outDir/work/demSea.map
      TSexecute restFile $win $lastWin $clonetif $clonemap
      	if [ $? -ne 0 ] ; then exit 1 ; fi
      fi 
    fi 
  fi

######################################################
# processing
######################################################
SSexecute ${prefixR}HyFlux2.1 -i $input -w $outDir/work -o $outDir -hollandFile $outDir/hollandData.txt $other_flags $flags 
	if [ $? -ne 0 ] ; then exit $? ; fi
cd $outDir

#####################################################
# postProcessing: full time, coastalImpact 
######################################################
TSexecute postTimeRanges

if [ "$nested" = yes ] ; then
    gifs="-outGIF false -outGifAnimation false"
else
    gifs=""
fi

TSexecute $prg/postProcessing -i $input -o .  -opt rel  -code $code  -inundation true  $gifs -tifuv2netcdf false -outTTT false $hgrey -commentsOn false
#-------------------------------------------------
# coastal impact
#-------------------------------------------------
TSexecute $prg/coastalImpact -i $input -o .  -opt rel   
NVSexecute rm -f coastalImpactMax.bmp.aux.xml  coastalImpact.bmp.aux.xml coastalIndex.tif clipFileCI.tif 

######################################################
# HYFLUX post-processing  (if HYFLUX or exist work/watch)
######################################################
if [ $code = HYFLUX -o -d work/watch ] ; then
NVSexecute tar -xf WTFiles.tar  
#-------------------------------------------
# plots at watch points
#-------------------------------------------
#TSexecute vpl -i $procDir/4-post3.pr  work/watch `pwd` work 
TSexecute python $pythonDir/4-post3.py  work/watch `pwd` work 
#-------------------------------------------
# inundation area 
#-------------------------------------------
#TSexecute vpl -i $procDir/4-mesArea.5.pr work $folder  
TSexecute python  $pythonDir/4-mesArea.5.py work $folder  
#-------------------------------------------
# add fields like place, country etc in inundation.txt file and sort by simDistRunup
#-------------------------------------------
TSexecute mergeLoc -l $places -s work/inundation.txt -o work/inundation.txt  -sortedby -simDistRunup
rm -rf WT_* 
fi

cd $workDir

}



function run ()
{
#-------------------------------------------------
#set -x
#trap read debug
#-------------------------------------------------

# env variables used by run

#  prg  workDir outDir nestDir dxCoarse dxNest ncore RESTART


if [ $ncore -gt 1 ] ; then
export prefixR="mpirun -np $ncore mpi."
else
export prefixR=
fi


export lastWin=none

if [ ! -d $finalDir ] ; then mkdir -p $finalDir ; fi

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# loop over windows
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
let iwin=0
let lastcc=0
let nwin=`nitem $windowS`

while [ $lastcc -eq 0 -a $iwin -lt $nwin ] ; do
let iwin=iwin+1
export win=`item "$windowS" $iwin`
export minDem=`item "$minDemS" $iwin`
export maxDist=`item "$maxDistS" $iwin`
export minDistSea=`item "$minDistSeaS" $iwin`
export code=`item "$codeS" $iwin`
export batgrid=`item "$batgridS" $iwin`
export reqsize=`item "$reqsizeS" $iwin`
  if [ $iwin -le `nitem $lastWindowS` ] ; then 
  last=`item "$lastWindowS" $iwin `
    if [ -e $lastWorkDir/$last/bathymetry.tif -a -e $lastWorkDir/$last/logout.txt ] ; then export lastWin=$last ; fi
  fi
export win

#-----------------
cd $workDir
export outDir=$win
export dxSimulation=$batgrid

#-----------------
if [ $iwin -eq 1 ] ; then 
export nested=no
else 
export nested=yes 
fi


#-------------------------------------------------------
# outDir exist
#-------------------------------------------------------
if [ -e $outDir/logout.txt ] ; then 
STATUS=`getField  -i $outDir/logout.txt -f STATUS `
echoS $outDir yet done  STATUS=$STATUS
  if [ "$STATUS" = complete ] ; then
  lastcc=0
  else
  lastcc=1
  fi

#-------------------------------------------------------
# outDir does NOT exist
#-------------------------------------------------------
else
echoS
echoS Start calculation $outDir
echoS minDem=$minDem maxDist=$maxDist minDistSea=$minDistSea 

echo export STATUS=RUNNING >> $workStatus
echo RUNNING date=\"`date`\" runID=$runID  > $destStatus
echo RUNNING date=\"`date`\" runID=$runID  > $runStatus
echo RUNNING date=\"`date`\" runID=$runID  

mkdir $outDir 
export folder=`basename $advDir`-`basename $outDir`

#-------------------------------------------------
# coarse input deck
#-------------------------------------------------
if [ $iwin -eq 1 ] ; then
nestedWin=''
export other_flags=" -dtstat $dtstat   "
export lastDir=$lastWorkDir/$lastWin

echo variables used in 1-coarseInp.pr
declare -p batgrid input RESTART lastDir workDir dtout  timestart outDir lastWin cycloneDir
#TSexecute vpl -i $cycloneDir/1-coarseInp.pr  
TSexecute python $pythonDir/1-coarseInp.py  
      if [ $? -ne 0 ] ; then exit 1 ; fi

#-------------------------------------------------
# nested  input deck
#-------------------------------------------------
else  # nested 
nestedWin="$nestedWin $win"
export maskmap=$outDir/mask.map
export other_flags=" -boundaryDir $boundaryDir  -minDem $minDem -minDistSea $minDistSea -maxDist $maxDist  -dtstat $dtstat -mask $maskmap  "

# first estimate of demSea 

mkdir work
TSexecute HyFlux2.1 -bathymetry $boundaryDir/bathymetry.tif -onlyDem yes -w work -o work -minDem `expr $minDem-100` -maxDist `expr $maxDist+$batcoarse*10` -minDistSea `expr $minDistSea+$batcoarse*2` 
	if [ $? -ne 0 ] ; then echoS No shoreline: calculation of $outDir is not performed ; let lastcc=2 ; fi

  if [ $lastcc -eq 0 ] ; then
  # mask.map
  fac=`expr \($batgrid/$batcoarse\)^2 `
  nv=`expr $reqsize*$fac`
  nv=`int $nv`
  
  declare -p batgrid batcoarse fac reqsize nv minzmax

#nestWindow="-lonmin $lonminAdv -lonmax $lonmaxAdv  -latmin $latminAdv  -latmax $latmaxAdv"   #   the nested window is restricted to the current bulletin track
nestWindow=""  
  FVSexecute "getMinheight.1 -dem work/demSea.map -height $boundaryDir/TIF_MAXHEIGHT_END.tif -mask $maskmap -nradius 5 -nv $nv  -minHeight $minzmax $nestWindow "   $outDir/out.pr
  	if [ $? -ne 0 ] ; then echoS No values with minHeight near shore \> $minzmax: calculation of $outDir is not performed ; let lastcc=2 ; fi
  rm -rf work
  fi

  if [ $lastcc -eq 0 ] ; then
  declare -p batgrid boundaryDir input outDir maskmap 
# TSexecute vpl -i $cycloneDir/1-nestInp.pr 
  TSexecute python $pythonDir/1-nestInp.py 
  	if [ $? -ne 0 ] ; then echoS Error during preparation of nested input deck : calculation of $outDir is not performed ; let lastcc=2  ; fi
  fi
fi
#-------------------------------------------------
# end input deck preparation
#-------------------------------------------------


#-------------------------------------------------
# copy holland
#-------------------------------------------------
  if [ $lastcc -eq 0 ] ; then
  cp -v outData.txt $outDir/hollandData.txt
  cp -uv outData.txt $finalDir/hollandData.txt
	if [ $? -ne 0 ] ; then echoS Error copying holland data ; let lastcc=1 ; fi
  fi

#-------------------------------------------------
# execute pre & calc $ post
#-------------------------------------------------
  if [ $lastcc -eq 0 ] ; then
  SSexecute execr    $input $outDir $folder $lastWin
  lastcc=$?
  fi

fi  #  [ -e $outDir/logout.txt ]

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# end calculation
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if [ $lastcc -eq 100 ] ; then 
echoS Not done $outDir because nv=0
exit $lastcc
elif [ $lastcc -gt 0 ] ; then 
  if [ $iwin -eq 1 ] ; then 
  echoS Exit with error from `basename $0` because coarse calculation $outDir failed
  exit 1  
  else
  echoS Exit without error from calculations loop because $outDir failed  
  NVSexecute rm -rf $outDir
  fi
fi
if [ $lastcc -eq 0 ] ; then
export date2=`date`
echoS `cat $outDir/logout.txt | cut -d";" -f 3,4,5,7,8,9,10 | sed -e s/\;//g | sed -e "s/  / /g" |  sed -e "s/= /=/g" `
echoS Completed $outDir calculation: elaps from start [hh:mm:ss] `elaps `  
#-------------------------------------------------
# copy results in finalDir
#-------------------------------------------------
MRexecute  copy $win $workDir  $finalDir none $lastcc  $advDir/copy.$win.txt
	if [ $? -ne 0 ] ; then echoS Error from copy results ; exit 1 ; fi

#-------------------------------------------------
# merge locations
#-------------------------------------------------
cd $finalDir
MRexecute  merge $nestedWin  $advDir/merge.$win.txt  
	if [ $? -ne 0 ] ; then echoS Error from merge locations ; exit 1 ; fi


#-------------------------------------------------
# publish
#-------------------------------------------------
export date2=`date "+%d %b %Y %H:%M:%S" `
export pubDate=$date2
export dxSimulation=$dxSimulation
TSexecute publishWeb  dxSimulation=$dxSimulation
	if [ $? -ne 0 ] ; then echoS Error from publish $outDir ; exit 1 ; fi

#-------------------------------------------------
# save informations for nested calculation
#-------------------------------------------------
export boundaryDir=$outDir
export batcoarse=$batgrid
fi

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
done		# end loop over windows
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#-------------------------------------------------
# Post processing of final map and clean
#-------------------------------------------------
MRexecute finalPostProcessing  $advDir/finalPostProcessing.txt

}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  start of runSurgeModel.sh script
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

date >> $HOME/cycloneSurge/logCalls.txt
echo "calling parameters: $0 $* " >> $HOME/cycloneSurge/logCalls.txt


# defaults

export d0=6
export d1=2
export d2=0.5

fineCalc=yes
surgeCalc=automatic
testing=no
fromBul=0
inpPath=$HOME/cycloneSurge/publicVM
calcPath=$HOME/cycloneSurge/storage/tmp
outPath=$inpPath

if [ "$sleepmax" = "" ] ; then export sleepmax=120 ; fi   	#  time in min to wait that the previous calculation is finished


# get arguments

command="$0 $*"

if [ $# -eq 0 ] ; then
echo

echo enter: $0 -hurNo -bulNo [-surgeCalc] [-fromBul] [-testing] [-inpPath]  [-outPath] [-calcPath] [-fineCalc]
echo
echo Folder structure:
echoT bulInp=inpPath/hurNo/input/bulNo
echoT hurDir=calcPath/hurNo
echoT destPath=outPath/hurNo
echoT bulDir=hurDir/bulNo
echo where:
echoT bulInp: folder where the input files inpData.txt, info.pr, bulInfo.txt must exist
echoT hurDir: hurricane folder containing bulletin folders and STATUS.sh file
echoT hurDir/bulNo, hurDir/fromBul-bulNo,  hurDir/fromBul-bulNo-calc, hurDir/fromBul-bulNo-final: folders with the bulletin calculations 
echoT destPath/final: the final results well be stored 
echo
echo Defaults:
echoT testing=$testing
echoT fromBul=$fromBul  \(fromBul has effect if surgeCalc=init, otherwise it is evaluated automatically\)
echoT surgeCalc=$surgeCalc  \(can be no, automatic, force, init\)
echoT fineCalc=$fineCalc  \(can be yes,no \)

echo If testing=no:
echoT inpPath=$inpPath
echoT calcPath=$calcPath
echoT outPath=$outPath

echo If testing=yes:
echoT inpPath=~/cycloneSurge/storage.local/public
echoT calcPath=~/cycloneSurge/storage.local/tmp
echoT outPath=~/cycloneSurge/storage.local/public

echo Note:
echo \	surgeCalc=no:\	\	only wind radii to holland calculation \(WR2H\) is performed 
echo \	surgeCalc=force:\	wind radii to holland calculation is not done: the file outData.txt \(holland parameters\) must exist in bulInp 
echo \	surgeCalc=automatic:\	the calculation starts at bulNo with max wind velocity \> velStart and ends at the bulNo with max velocity \< velEnd
echo \	surgeCalc=init: \	the calculation is done fromBul to bulNo without regards to wind velocity. The WR2H calculation is repeated for each bulletin 
echo
echo \	fineCalc=no:\	\	simulations with bathymetry $d0 $d1
echo \	fineCalc=yes:\	\	simulations with bathymetry $d0 $d1 $d2

exit 0
fi

export startDir=`dirname $0`
  if [ $startDir = "." ] ; then startDir=`pwd` ; fi

while [ $# -gt 0 ] ; do
case $1 in
    -testing) 	export testing=$2;shift;;
    -hurNo) 	export hurNo=$2;shift;;
    -bulNo) 	export bulNo=$2;shift;;
    -surgeCalc)	export surgeCalc=$2;shift;;
    -fromBul)	export fromBul=$2;shift;;
    -inpPath)	export inpPath=$2;shift;;
    -calcPath)	export calcPath=$2;shift;;
    -outPath)	export outPath=$2;shift;;
    -fineCalc)	export fineCalc=$2;shift;;
esac
shift
done


  if [ $testing = yes ] ; then 
  inpPath=~/cycloneSurge/storage.local/public
  calcPath=~/cycloneSurge/storage.local/tmp
  outPath=~/cycloneSurge/storage.local/public
  fi

  if [ "$bulNo" = "" ] ; then echo No defaults for bulNo ; exit ; fi
  if [ "$hurNo" = "" ] ; then echo No defaults for hurNo ; exit ; fi

  export bulInp=$inpPath/$hurNo/input/$bulNo
  export hurDir=$calcPath/$hurNo
  export bulDir=$calcPath/$hurNo/$bulNo
  export destPath=$outPath/$hurNo

export info=info.pr
export infoxml=info.xml
export inpData=inpData.txt
export bulInfo=bulInfo.txt
export inpDataxml=inpData.xml
export bulInfoxml=bulInfo.xml

#------------------------------------------------ 
# initialize stepOut

export stepOut=/tmp/stepOut.${$}.txt
echo > $stepOut
if [ $? -ne 0 ] ; then echo cannot initialize stepOut=$stepOut ; exit ; fi
#------------------------------------------------ 
if [ ! -d $hurDir ] ; then mkdir -p $hurDir ; fi
if [ ! -d $bulDir ] ; then mkdir -p $bulDir ; fi
if [ ! -d $destPath/$bulNo ] ; then mkdir -p $destPath/$bulNo ; fi
if [ "$workStatus" = "" ] ; then export workStatus=$hurDir/STATUS.sh ; fi   #  define another workStatus if debugging

#------------------------------------------------ 
# verify mounted folders (sometimes error !?!?) 
for dir in $bulDir $destPath/$bulNo ; do
  if [ `check_mkdir $dir` -ne 1 ] ; then echo Problems in mkdir $dir ; fi
done
#------------------------------------------------ 
# are input ready ?
#-----------------------------------------------------

let sleeptot=0
while [ ! -e $bulInp/$bulInfo -a $sleeptot -lt $sleepmax ] ; do
echo
echoS CAUTION: I have been waiting $sleeptot min for $bulInp/$bulInfo
let sleeptot=sleeptot+5
sleep 5m
done

if [ ! -e $bulInp/$bulInfo ] ; then checkExitCode -1 "after waiting $sleeptot no input data is available " ; fi

#------------------------------------------------ 

export date1=`date "+%d %b %Y %H:%M:%S" `
export dateStart=$date1

cd $startDir

echo +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
echo start of calculation: $command  
echo +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
echo 
declare -p startDir bulInp destPath bulDir hurDir bulNo hurNo surgeCalc fromBul
echo -----------------------------------------------------


declare -p info inpData bulInfo
echo -----------------------------------------------------

# copy input file in storage directory

for file in $info $inpData $bulInfo $infoxml $inpDataxml $bulInfoxml ; do
  if [ ! -e $bulInp/$file ] ; then echo Not exist file $bulInp/$file ; exit 1 ; fi
cp -uvr $bulInp/$file $bulDir/$file
  if [ ! -e $bulDir/$file ] ; then echo Not copyed file $bulDir/$file ; exit 1 ; fi
done

# -----------------------------------------------------
# set enviroment & functions

export calcType=CYCLONE

export cycloneDir=$startDir/TCHome.4
export pythonDir=$startDir/Python
export prg=$bin
export scripts=$cycloneDir      #  path to 4-project.pr

for dir in $cycloneDir $bin $scripts $procDir $pythonDir; do  
  if [ ! -d $dir ] ; then echo folder $dir does not exist ; exit ; fi
done

export PATH=$PATH:$cycloneDir:$procDir:$pythonDir

export cmdChain=`CMD $0`

declare -p procDir cycloneDir prg calcType scripts pythonDir
echo -----------------------------------------------------

# fixed variables 

export maskSea=yes   						#  yes == initial conditions for restart only from the sea (the past inundation is not considered). Used in restFile
export cleanCalc=no 						#  yes == clean  calc directory of the previous calculation

export dtout=3600
declare -p dtout maskSea cleanCalc 
echo -----------------------------------------------------


# free variables & defaults
#
# run

if [ "$runID" = "" ] ; then export runID=$$ ; fi
if [ "$ncore" = "" ] ; then export ncore=6 ; fi			#  fluentsrv2 has 24 core => 4 runs in the same time. What happens if more than 4 cyclones in the same time ?
# perfom calculation according to vmax 
export velStart=30 # if vmax_1 > velStart => start simulations (58 knots)
export velEnd=18 # if max(vmax) < velEnd => no simulations   (35 knots)
if [ "$minSpace" = "" ] ; then export minSpace=5 ; fi		# minimum free space in temporary folder
if [ "$keepNETCDF" = "" ] ; then export keepNETCDF=no ; fi	#  yes == keep NETCDF of calc windows
if [ "$finalClean" = "" ] ; then export finalClean=yes ; fi   	#  yes == remove calc windows from finalDir
  
if [ "$tmpPublish" = "" ] ; then export tmpPublish=yes ; fi	#  yes == publish calc windows
if [ "$tmpFolder" = "" ] ; then export tmpFolder=no ; fi	#  yes == calculations in temporary folder (/tmp, /srv/scratch) 
if [ "$tmpAdvDir" = "" ] ; then export tmpAdvDir=no ; fi	#  yes == control output in temporary folder
if [ "$tmpFolder" = no ] ; then export tmpAdvDir=no ; fi	#  
if [ "$tmpClean" = "" ] ; then export tmpClean=no ; fi 		# yes == clean the calculation folder after the copy of the data in finalDir (usefull when batch runs in Qsub)
if [ "$minzmax" = "" ] ; then export minzmax=0.05 ; fi		# the nested field domain is defined by the pixels where the Max water heigh simulated in the coarser simulation is > minzmax
declare -p runID ncore keepNETCDF finalClean tmpPublish tmpFolder tmpAdvDir minSpace
echo -----------------------------------------------------

if [ "$timeForecast" = "" ] ; then export timeForecast=96 ; fi
declare -p sleepmax timeForecast
echo -----------------------------------------------------

#------------------------------------------------ 
# hydro data

dist1=`expr $d0*1`
dist2=`expr $d1*1`
declare -p d0 d1 d2 dist1 dist2

export batgridS="$d0 $d1  $d2 "
export windowS=" run${d0}  run${d1}  run${d2} "
export minDistSeaS=" -1  $dist1  $dist2 "
export minDemS=" -10000  -3000  -200"
export maxDistS=" 10000  100  10"
export codeS="  HYFLUX  HYFLUX  HYFLUX"
export finergridS=" no  no  yes"
export currentWindowS=" none  none  none "
export reqsizeS=" -1  50000  50000 "


if [ $fineCalc = no ] ; then
export batgridS=" $d0  $d1 "
export windowS=" run$d0  run$d1 "
export minDistSeaS=" -1  $dist1 "
export minDemS=" -10000  -3000 "
export maxDistS=" 10000  100 "
export codeS="  HYFLUX  HYFLUX "
export finergridS=" no yes"
export currentWindowS=" none 	none "
export reqsizeS=" -1  50000  "
fi

export firstWin=`item "$windowS" 1`
export otherWin=`item "$windowS" 2-`

declare -p fineCalc batgridS windowS minDemS maxDistS finergridS  codeS minDistSeaS firstWin otherWin currentWindowS reqsizeS
echo -----------------------------------------------------

#-----------------------------------------------------
# post processing
#-----------------------------------------------------

export hgrey=""

#-----------------------------------------------------
# define Status files
#-----------------------------------------------------
 
export hurName=`getField -i $bulDir/$info -f hurName`

export destStatus=$destPath/$bulNo/StormSurgeStatus.txt 
export runStatus=$hurDir-$hurName.txt


declare -p destStatus workStatus runStatus stepOut
echo -----------------------------------------------------
 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  process surgeCalc
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
export begin=no

declare -p surgeCalc begin velStart velEnd 
echo -----------------------------------------------------

export timeShift=0
export STATUS=""
export advNo=""
export toBul=0

if [ -e $workStatus -a $begin = no ] ; then
. $workStatus
fi
declare -p timeShift STATUS advNo toBul 
echo -----------------------------------------------------

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  add  new bulNo
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

export lastSTATUS=$STATUS
export lastAdvNo=$advNo
export lastWindowS="$currentWindowS"
 
if [ "$lastSTATUS" = "" -o "$lastAdvNo" = "" ] ; then export begin=yes ; fi

declare -p lastSTATUS lastAdvNo begin lastWindowS
declare -p bulNo bulDir
echo -----------------------------------------------------

echoS    
echoS +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
echoS ----   bulNo=$bulNo   runID=$runID -----   
echo +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
echoS dateStart=$dateStart   
echoS $command
echoS
#-----------------------------------------------------
# control input data
#-----------------------------------------------------

cd $bulDir

lastcc=0
message=
  for file in $info $inpData $bulInfo ; do
    if [ ! -e $file  ] ; then 
    message="$message $file does not exist"
    lastcc=-1
    fi  
  done

if [ $lastcc -ne 0 ] ; then checkExitCode $lastcc "$message" ; fi  


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#if [ ! -e info.sh ] ; then NVSexecute vpl -i $cycloneDir/info2sh.pr ; fi
if [ ! -e info.sh ] ; then NVSexecute python $pythonDir/info2sh.py ; fi
. info.sh
#if [ ! -e info.pr ] ; then NVSexecute vpl -i $cycloneDir/info2pr.sh ; fi
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#-----------------------------------------------------
touch $destStatus
touch $runStatus

#-----------------------------------------------------
# review input data because sometimes errors
#-----------------------------------------------------

cd $bulDir

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
NVSexecute endtable -i $bulInfo -var advNo -log gt -val $bulNo 
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sed -e s/UTC//g -i $bulInfo
sed -e s/UTC//g -i $info

#-----------------------------------------------------
# make logs 
#-----------------------------------------------------

cd $hurDir
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#NVSexecute  vpl -i $cycloneDir/makeLogs.pr $bulNo 
#NVSexecute  python $pythonDir/makeLogs.py $bulNo 
FSexecute  "python $pythonDir/makeLogs.py $bulNo"  mk.log
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
export nvel=2 nwr=4
. logs.sh

echo after makeLogs.pr
declare -p vmax1 maxvmax minvmax nvel nwr 
echo ----------------------------------------------------- 
echoS hurName=$hurName hurId=$hurId basin=$basin 
echoS bulNo=$bulNo bulDate=$bulDate
echoS velStart=$velStart  velEnd=$velEnd begin=$begin  
echoS ncore=$ncore minzmax=$minzmax timeForecast=$timeForecast   
echoS vmax1=$vmax1 maxvmax=$maxvmax minvmax=$minvmax nvel=$nvel nwr=$nwr  
echoS bulDir=$bulDir 
echoS calcDir=$calcDir

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  WR2H calculation
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if [ $surgeCalc = force ] ; then
echoS WR2H not done because surgeCalc=force
  if [ ! -e $bulDir/outData.txt ] ; then checkExitCode -1 "not exist $bulDir/outData.txt " ; fi

else
#-----------------------------------------------------
#  Bulletin  processing: from wind radii to hollands parameters
#-----------------------------------------------------

if [ -e  $bulDir/outData.txt ] ; then if [ $bulDir/inpData.txt -nt $bulDir/outData.txt ] ; then 
NVSexecute rm $bulDir/outData.txt 
echoS outData.txt is removed because older than inpData.txt
fi ; fi

# WR2H repeted if init

  if [  $surgeCalc = init ] ; then
  noBul=""
  echoS repeat WR2H because surgeCalc=init
  let bul=$fromBul
    while [ $bul -le $bulNo ] ; do
      if [ ! -d $hurDir/$bul ] ; then
      mkdir -p $hurDir/$bul
        for file in $info $inpData $bulInfo inpData.xml bulInfo.xml; do
        hurInp=$inpPath/$hurNo/input
          if [ ! -e $hurInp/$bul/$file ] ; then echo Not exist file $hurInp/$bul/$file ; exit 1 ; fi
        cp -uvr $hurInp/$bul/$file $hurDir/$bul/$file
          if [ ! -e $hurDir/$bul/$file ] ; then echo Not copyed file $hurDir/$bul/$file ; fi
        done
      fi
    
      if [ -f $hurDir/$bul/$inpData ] ; then    
      cd $hurDir/$bul
      echoS prePROCESSING bul=$bul 
      
      # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     #FSexecute "vpl -i $cycloneDir/WR2H.2.pr $info $inpData outData.txt  plot"  $hurDir/$bul/WR2H.txt
      FSexecute "python $pythonDir/WR2H.py $infoxml $inpDataxml outData.txt  plot"  $hurDir/$bul/WR2H.txt
      # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      else
      echoS not done prePROCESSING of bul=$bul because not exist  $hurDir/$bul/$inpData
      noBul="$noBul $bul"
      fi
    
    let  bul=bul+1
    done
  fi


#-----------------------------------------------------
# holland parameters calculations 
#-----------------------------------------------------


if [ ! -e $bulDir/outData.txt ] ; then
cd $bulDir
echo export STATUS=prePROCESSING >> $workStatus
echo prePROCESSING date=\"`date`\" runID=$runID > $destStatus
echo prePROCESSING date=\"`date`\" runID=$runID > $runStatus
echo prePROCESSING date=\"`date`\" runID=$runID
 
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#FSexecute "vpl -i $cycloneDir/WR2H.2.pr $info $inpData outData.txt  plot " $bulDir/WR2H.txt
FSexecute "python $pythonDir/WR2H.py $infoxml $inpDataxml outData.txt  plot"  WR2H.txt
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#cp /mnt/web/cycloneSurgeVM/$hurNo/$bulNo/outData.txt $bulDir/
#cp /mnt/web/cycloneSurgeVM/$hurNo/$bulNo/WR2H.txt $bulDir/ 
#cp /mnt/web/cycloneSurgeVM/$hurNo/$bulNo/vtcal.txt $bulDir/ 

 
echo export STATUS=DONE >> $workStatus
echo  DONE prePROCESSING date=\"`date`\" runID=$runID > $destStatus
echo  DONE prePROCESSING date=\"`date`\" runID=$runID > $runStatus
echo  DONE prePROCESSING date=\"`date`\" runID=$runID
 
cp -R $bulDir $destPath
fi

fi

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  exit from calculation according to surgeCalc, control files, lastSTATUS  
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
declare -p surgeCalc lastSTATUS

#-----------------------------------------------------
#  exit if surgeCalc=no
#-----------------------------------------------------
if [ $surgeCalc = no ] ; then echoS Exit because surgeCalc=no ; cat $stepOut ; sleep 5s ; exit 0 ; fi

#-----------------------------------------------------
#  exit if control files exist
#-----------------------------------------------------
if [ -e $startDir/noSurgeCalc.txt ] ; then checkExitCode 0 "exist $startDir/noSurgeCalc.txt" ; fi
if [ -e $hurDir/noSurgeCalc.txt ] ; then checkExitCode 0 "exist $hurDir/noSurgeCalc.txt" ; fi

#-----------------------------------------------------
#  lastSTATUS control
#-----------------------------------------------------

if [ "$lastSTATUS" = FAILED ] ; then
checkExitCode 1 "previous STATUS=$lastSTATUS"
fi
 
if [ "$lastSTATUS" = RUNNING ] ; then
checkExitCode -1 "previous  STATUS=$lastSTATUS" 
fi
 
if [ "$lastSTATUS" = prePROCESSING ] ; then
checkExitCode -1 "previous  STATUS=$lastSTATUS"
fi

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  exit from calculation according to begin maxvmax velStart velEnd surgeCalc nvel nwr
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
declare -p begin maxvmax velStart velEnd surgeCalc nvel nwr

if [ $begin = yes  ] ; then
#-----------------------------------------------------
# IGNORE bulletin ? (only if begin=yes)
#-----------------------------------------------------
	if [ $maxvmax -lt $velStart ] ; then
	checkExitCode -1 "maxvmax -lt velStart"
	fi
 
echo export STATUS=INIT >> $workStatus
echo export fromBul=$fromBul >> $workStatus
echo export timeShift=$timeShift >> $workStatus
echo    >> $workStatus
 
. $workStatus
export lastSTATUS=INIT
 
fi #

#-----------------------------------------------------
# timeShift is the time of last analysis, i.e., time of start of the forecast, i.e., time 0 of last bulettin
# timestart of next calculation is timeShift of the previous (COMPLETD) calculation
#-----------------------------------------------------
 
export InTime=$timeShift
export timestart=`echo "$InTime * 3600 "| bc -l `
 
#-----------------------------------------------------
#  IGNORE bulletin ? (not for force or init ) 
#-----------------------------------------------------

if [ $velEnd -gt 0 ] ; then

# IGNORE bulletin
if [ $maxvmax -lt $velEnd  ] ; then
checkExitCode -1 "maxvmax -lt velEnd"
fi
 
# IGNORE bulletin
if [  "$nvel" -le 1 -o "$nwr" -lt 2 ] ; then
checkExitCode -1 "nvel -le 1 -o nwr -lt 2"
fi
 
fi

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# combine bulletins and update STATUS.sh  using bulAdv.pr
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cd $hurDir

export lastWorkDir=$hurDir/$lastAdvNo-calc
 
if [ "$lastAdvNo" != "" -a -d $lastWorkDir ] ; then 
export RESTART=yes
export velStart=0 velEnd=0
else
export RESTART=no
export lastWorkDir=""
fi

declare -p RESTART lastAdvNo lastWorkDir velStart velEnd

# bulAdv.pr create advNo folder and files info.pr, info.sh, bulInfo.txt, outData.txt 

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#FSexecute "vpl -i $cycloneDir/bulAdv.pr $fromBul $bulNo " bulAdvOut.txt
FSexecute "python $pythonDir/bulAdv.py $fromBul $bulNo " bulAdvOut.txt
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
maxcc=$?

if [ $maxcc -ne 0 ] ; then 
checkExitCode -1 " error from bulAdv.pr" 
fi

. $workStatus

declare -p advNo fromBul toBul InTime timeShift FinTime 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# export env variables used in bodyLoc.xml and bodyLoc.kml
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

. $advNo/info.sh

#-----------------------------------------------------
# move logs in advNo
#-----------------------------------------------------
cd $hurDir

mv bulAdvOut.txt $advNo

cp $stepOut $advNo
rm -rf $stepOut
export stepOut=$advNo/`basename $stepOut`

chmod +w -R $advNo

export dtstat=`echo "3600*6 " | bc -l `


echo +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
echo Variables necessary to execute run.sh
echo +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

declare -p destPath 
declare -p scripts procDir cycloneDir ncore runID
declare -p destStatus workStatus runStatus stepOut
declare -p dateStart hurDir advNo 
declare -p tmpFolder tmpAdvDir 
declare -p dtout dtstat  timestart 
declare -p minDemS minDistSeaS maxDistS reqsizeS
echo -----------------------------------------------------------

if [ "$RESTART" = "" ] ; then export RESTART=no ; fi

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# setup calculation folders
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# tmpFolder=yes => calculation in calcDir
# tmpFolder=no => calculation in hurDir
 
if [ $tmpFolder = yes ] ; then
export workDir=$calcDir/${advNo}-calc
export finalDir=$calcDir/${advNo}-final
else
export workDir=$hurDir/${advNo}-calc
export finalDir=$hurDir/${advNo}-final
fi

if [ $tmpAdvDir = yes ] ; then
cp -R $hurDir/$advNo $calcDir 
export advDir=$calcDir/$advNo
else
export advDir=$hurDir/$advNo
fi

export stepOut=$advDir/`basename $stepOut`


export tmpdir=$advDir/tmpdir

if [ ! -d $tmpdir ] ; then mkdir -p $tmpdir ; fi
if [ ! -d $workDir ] ; then mkdir -p $workDir ; fi
if [ ! -d $finalDir ] ; then mkdir -p $finalDir ; fi

for dir in $tmpdir $workDir $finalDir ; do
  if [ `check_mkdir $dir` -ne 1 ] ; then echo Problems in mkdir $dir ; fi
done

if [ $tmpFolder = yes ] ; then 
dir=$calcDir
  if [ `check_mkdir $dir` -ne 1 ] ; then echo Problems in mkdir $dir ; fi
if [ -d $hurDir/${advNo}-calc ] ; then NVSexecute cp -R $hurDir/${advNo}-calc $calcDir ; fi 
if [ -d $hurDir/${advNo}-final ] ; then NVSexecute cp -R $hurDir/${advNo}-final $calcDir ; fi 
if [ -d $hurDir/${advNo} ] ; then NVSexecute cp -R $hurDir/${advNo} $calcDir ; fi 
fi

cd $advDir

files="info.xml bulInfo.txt outData.txt"

echo Files that must be stored in advNo: $files
cp -v $files  $workDir
cp -v $workStatus $destPath

export someError=.some-error.txt

if [ "$RESTART" = "yes" ] ; then declare -p lastWorkDir lastWindowS ; fi
declare -p advDir workDir finalDir 
 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# run
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

export date1=$dateStart    #  date1 is used in elaps and must not be written in STATUS.sh
export input=Calc_input_deck.txt
echo export currentWindowS=\"$windowS\" >> $workStatus

cd $workDir
export runcc=0
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
MRexecute run  $advDir/runOut.txt
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
export runcc=$?


if [ "$cleanCalc" = yes -a "$lastAdvNo" != "$advNo" -a $runcc -eq 0 ] ; then rm -rf $lastWorkDir ; fi

if [ $runcc -eq 100 ] ; then checkExitCode -1 " offshore " ; fi
if [ $runcc -ne 0 ] ; then checkExitCode $runcc " error from run " ; fi
checkExitCode 0 " normally "
echo -----------------------------------------------------------------------------------

