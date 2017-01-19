

#-------------------------------------------------
. $procDir/functions.sh
export cmdChain=$cmdChain/`CMD $0`
#-------------------------------------------------

# exec.sh  input outDir code

input=$1
outDir=$2
code=$3

# env variable:

#  prg: path to swan codes
#  procDir: path to procedures
#  scripts/calcDir: path to 4-project.pr, executed by post-processing
#  hgrey: hgrey parameter
#  prefix: if parallel processing (ncore > 1)
#  other_flags: directive to HyFlux2 for nested simulations
#  flags: additional directive to HyFlux2
#  morePostFlags: additional flags used by swanConsolle, postProcessing 
#  tmpdir: output folder requested by 4-mesArea.5.pr


if [ ! -e $outDir/$input ] ; then 
echo Not exist $outDir/$input
exit
fi

# used by swanConsolle, postProcessing 
postFlags=" -outGIF false -outGifAnimation false -outTTT false $hgrey -commentsOn false "


#-------------------------------------------------

if [ "$scripts" = "" ] ; then export scripts=$calcDir ; fi

######################################################
# pre_processing
######################################################

if [ $code = HYFLUX ] ; then
CSexecute $prg/preProcessing -i $input -o $outDir -opt rel -code $code  -batNod true -commentsOn false 
lastcc=$?
else
CSexecute $prg/preProcessing -i $input -o $outDir -opt rel -code $code  -commentsOn false 
lastcc=$?
fi
if [ $lastcc -ne 0 ] ; then exit  $lastcc ; fi

######################################################
# processing
######################################################

if [ "$code" = HYFLUX ] ; then
inundation="-inundation true -code HYFLUX "
CSexecute ${prefix}HyFlux2.1 -i $input -w $outDir/work -o $outDir -fault $outDir/defNode.tif $other_flags $flags
lastcc=$?
if [ $lastcc -ne 0 ] ; then exit  $lastcc ; fi

else
inundation=" -inundation false "
CSexecute $prg/swanConsolle -i $input -o $outDir -opt rel  $inundation  $postFlags
lastcc=$?
if [ $lastcc -ne 0 ] ; then exit  $lastcc ; fi
fi


######################################################
# deformation  (only if exist defNode.tif or deformation.tif)
######################################################

cd $outDir

if [ "$calcType" = TSUNAMI ] ; then
if [ -e defNode.tif -a -e bathymetry.tif -a ! -e deformation.tif ] ; then 
TSexecute resmap -clone bathymetry.tif -i defNode.tif -o deformation.tif
fi

if [ -e deformation.tif ] ; then
TSexecute map2image -i deformation.tif -pixsize 1 -scale 1 -o deformation.tif.png -rgb $procDir/colDepth.txt
fi
fi


######################################################
# post-processing
######################################################

if [ -e maxValues.txt ] ; then rm maxValues.txt ; fi

CSexecute $prg/postProcessing -i $input -o .  -opt rel   $inundation -code $code $postFlags $morePostFlags
lastcc=$?
if [ $lastcc -ne 0 ] ; then exit  $lastcc ; fi

######################################################
# coastal impact
######################################################

CSexecute $prg/coastalImpact -i $input -o .  -opt rel   
rm -f coastalImpactMax.bmp.aux.xml coastalImpact.png coastalImpact.bmp.aux.xml coastalImpact.tif coastalIndex.tif clipFileCI.tif


#-------------------------------------------
# HYFLUX post-processing  (if HYFLUX or exist work/watch)
#-------------------------------------------

if [ $code = HYFLUX -o -d work/watch ] ; then
tar -xf WTFiles.tar  
CSexecute vpl -i $procDir/4-post3.pr  work/watch . work
CSexecute vpl -i $procDir/4-mesArea.5.pr work  $outDir 
rm -rf WT_* 
fi



