#!/bin/bash 


#-------------------------------------------------
. functions.sh
export cmdChain=$cmdChain/`CMD $0`
#-------------------------------------------------

# env variables

# prg cycloneDir

# exec.sh  $input  $outDir $folder $lastWin

input=$1
outDir=$2
folder=$3
lastWin=$4


export win=`basename $outDir`

# env variable:

#  prg: 	path to swan codes
#  hgrey: 	hgrey parameter
#  prefix: 	if parallel processing (ncore > 1)
#  other_flags & flags: directive to HyFlux2 for nested simulations
# RESTART:	yes/no  (if yes, restFile.sh is done)
# lastWorkDir:	path of the previous run   (used by restFile.sh)

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
      TSexecute restFile.sh $win $lastWin $clonetif $clonemap
      	if [ $? -ne 0 ] ; then exit 1 ; fi
      fi 
    fi 
  fi

######################################################
# processing
######################################################
SSexecute ${prefix}HyFlux2.1 -i $input -w $outDir/work -o $outDir -hollandFile $outDir/hollandData.txt $other_flags $flags 
	if [ $? -ne 0 ] ; then exit $? ; fi
cd $outDir

######################################################
# postProcessing: full time, coastalImpact 
######################################################
TSexecute postTimeRanges.sh
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
TSexecute vpl -i $procDir/4-post3.pr  work/watch . work 
#-------------------------------------------
# inundation area 
#-------------------------------------------
TSexecute vpl -i $procDir/4-mesArea.5.pr work $folder  
#-------------------------------------------
# add fields like place, country etc in inundation.txt file and sort by simDistRunup
#-------------------------------------------
TSexecute mergeLoc -l $places -s work/inundation.txt -o work/inundation.txt  -sortedby -simDistRunup
rm -rf WT_* 
fi

