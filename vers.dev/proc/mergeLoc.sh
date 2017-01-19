#!/bin/bash 

#-------------------------------------------------
. $procDir/functions.sh
. $procDir/functionsLod.sh
export cmdChain=$cmdChain/`CMD $0`
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
 
