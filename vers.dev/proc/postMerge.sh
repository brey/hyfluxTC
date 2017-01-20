#!/bin/bash 

#-------------------------------------------------
. $procDir/functions.sh
export cmdChain=$cmdChain/`CMD $0`
#-------------------------------------------------
 
if [ $# -ne 1 ] ; then
echo enter $0  finalDir
echo finalDir: path to the folder of the final products
echo
exit
fi

echo $*
echo external variables:
echo procDir=$procDir finalClean=$finalClean prg=$prg firstWin=$firstWin otherWin=$otherWin windowS=$windowS tmpdir=$tmpdir STATUS=$STATUS

 
if [ "$tmpdir" = "" ] ; then echo tmpdir not defined  ; exit ; fi


export finalDir=$1
 
export win=postMerge
 
echo finalDir=$finalDir
 
#-------------------------------------------------
 

cd $finalDir


#-------------------------------------------------
# merge & remove  bathymetry and deformation 
#-------------------------------------------------

fileS=bathymetry.tif
if [ "$calcType" = TSUNAMI ] ; then fileS="$fileS deformation.tif" ; fi
 
for file in  $fileS ; do
 
large=$firstWin/$file
 
small=
let i=0
for window in $otherWin ; do
if [ -e $window/$file ] ; then small="$small $window/$file" ; let i=i+1 ; fi
done # window
 
if [ $i -gt 0 ] ; then mergeMap -l $large -o $file -s $small 
	if [ "$finalClean" = yes ] ; then NVSexecute rm $large $small   ; fi
fi

done # file

#-------------------------------------------------
#post processing
#-------------------------------------------------
 
if [ -e end-of-stuff.txt ] ; then 
echo Yet done postProcessing

else
if [ "$tttFirstWin" = yes ] ; 	then export outTTT=" -outTTT false -doNotOverwriteMax true " 
				else export outTTT=" -outTTT true $morePostFlags "  ; fi
CSexecute $prg/postProcessing -i $input -o . -opt rel  -code HYFLUX $outTTT -inundation false
 
cp -v locations.txt locations_merge.txt 
cp -v locations.xml locations_merge.xml 
cp -v locations.kml locations_merge.kml 

mergeAgain=no 
echo done post processing
fi

if [ "$finalClean" = yes ] ; then rm -rf TIF_H_*.tif   ; fi



#-------------------------------------------------
# coastal impact
#-------------------------------------------------

CSexecute $prg/coastalImpact -i $input -o .  -opt rel 

rm -f coastalImpactMax.bmp.aux.xml coastalImpact.png coastalImpact.bmp.aux.xml coastalImpact.tif coastalIndex.tif clipFileCI.tif

#-------------------------------------------------
# unzip
#-------------------------------------------------
 

if [ ! -d $firstWin -o ! -d watch ] ; then

if [ -e nestedFolders.zip ] ; then unzip -o nestedFolders.zip */*.txt &> /dev/null ; fi

if [ -e locations.kmz ] ; then unzip -o locations.kmz */*.txt */*.png &> /dev/null ; fi


export nested=yes

mergeAgain=yes 
echo done unzip for merge again

fi

#-------------------------------------------------
# STATUS
#-------------------------------------------------


STATUS=
for window in $windowS ; do
if [ -e $window/maxValues.txt ] ; then STATUS="$STATUS DONE" ; else STATUS="$STATUS notDONE" ; fi
done
export STATUS


#-------------------------------------------------
# calcLimit map
#-------------------------------------------------

if [ ! -e P1_MAXHEIGHT_END_LIMITS.jpg ] ; then

calcLimit=
let i=0
for window in $windowS ; do
if [ -e $window/cellBound.map ] ; then calcLimit="$calcLimit $window/cellBound.map " ; let i=i+1 ; fi
done

if [ $i -gt 0 ] ; then
TSexecute map2image -i $calcLimit -pixsize 1 -o calcLimit.png -doLegend no no no no no
#rm $calcLimit
fi

convert -composite P1_MAXHEIGHT_END.jpg calcLimit.png P1_MAXHEIGHT_END_LIMITS.jpg

fi

#-------------------------------------------------
# merge locations
#-------------------------------------------------
 
SSexecute $procDir/mergeLoc.sh

#-------------------------------------
#  clean arrival time
#-------------------------------------

if [ "$cleanTarr" = "yes" ] ; then
echo $procDir/cleanTarr.2.sh  $finalDir  0.5  
		$procDir/cleanTarr.2.sh  $finalDir  0.5
echo done clean arrival time
fi


#:::::::::::::::::::::::::::::::::::::::
if [ "$mergeAgain" = no ] ; then
#:::::::::::::::::::::::::::::::::::::::

#-------------------------------------------------
# merge tar files 
#-------------------------------------------------

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

#-------------------------------------------------
# compress &  clean
#-------------------------------------------------


NVSexecute zip NETCDF_H.nc.zip NETCDF_H.nc 
if [ "$finalClean" = yes ] ; then NVSexecute rm -rf NETCDF_H.nc ; fi

 
exclude=`ls */NETCDF* 2> $tmpdir/exclude.lst ` 
if [ "$exclude" != "" ] ; then 
echo $exclude > exclude.lst
exclude=" -x@exclude.lst "
fi

# don't include execute before zip
zip -r nestedFolders.zip $exclude $windowS watch/*.txt  1> /dev/null
if [ "$finalClean" = yes ] ; then NVSexecute rm -rf old_*.tar ; fi

fi 
#:::::::::::::::::::::::::::::::::::::::
if [ "$finalClean" = yes ] ; then NVSexecute rm -rf $windowS watch   ; fi

touch end-of-merge-0.txt

echo done postMerge.sh
