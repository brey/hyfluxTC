#!/bin/bash 

#-------------------------------------------------
. functions.sh
export cmdChain=$cmdChain/`CMD $0`
#-------------------------------------------------

# env variables used by run.sh

#  prg  workDir outDir nestDir dxCoarse dxNest ncore RESTART


if [ $ncore -gt 1 ] ; then
export prefix="mpirun -np $ncore mpi."
else
export prefix=
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
nestedWin=""
export other_flags=" -dtstat $dtstat   "
export lastDir=$lastWorkDir/$lastWin

echo variables used in 1-coarseInp.pr
declare -p batgrid input RESTART lastDir workDir dtout  timestart outDir lastWin cycloneDir
TSexecute vpl -i $cycloneDir/1-coarseInp.pr  
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
  TSexecute vpl -i $cycloneDir/1-nestInp.pr 
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
  SSexecute exec.sh    $input $outDir $folder $lastWin
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
echoS Completed $outDir calculation: elaps from start [hh:mm:ss] `elaps.sh `  
#-------------------------------------------------
# copy results in finalDir
#-------------------------------------------------
FSexecute "copy.sh $win $workDir  $finalDir none $lastcc"   $advDir/copy.$win.txt
	if [ $? -ne 0 ] ; then echoS Error from copy results ; exit 1 ; fi

#-------------------------------------------------
# merge locations
#-------------------------------------------------
cd $finalDir
FSexecute "merge.sh  $nestedWin "  $advDir/merge.$win.txt  
	if [ $? -ne 0 ] ; then echoS Error from merge locations ; exit 1 ; fi

#-------------------------------------------------
# publish
#-------------------------------------------------
export date2=`date "+%d %b %Y %H:%M:%S" `
export pubDate=$date2
export dxSimulation=$dxSimulation
TSexecute publishWeb.sh  dxSimulation=$dxSimulation
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
FSexecute finalPostProcessing.sh  $advDir/finalPostProcessing.txt


