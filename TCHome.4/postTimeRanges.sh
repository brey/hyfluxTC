#!/bin/bash 

#-------------------------------------------------
. functions.sh
export cmdChain=$cmdChain/`CMD $0`
#-------------------------------------------------

# env. variables
declare -p timeShift FinTime hgrey input code prg
if [ $? -ne 0 ] ; then exit 1 ; fi
#-----------------------------------------------------------------
# post-processing of selected time ranges. output:  locations_$ext
#-------------------------------------------------
time=$timeShift
  while [ $time -le $FinTime ] ; do
  let timeh=time-timeShift
    if [ $time -gt 0 ] ; then
    let time1=`echo " ( $time - 6 ) * 3600  " | bc -l `
    let time2=`echo " $time * 3600  " | bc -l `   
      if [ $timeh -lt 100 ] ; then
      ext0=`echo "$timeh + 100 " | bc -l `
      ext=`echo $ext0 | cut -b 2-3`
      else
      ext=$timeh
      fi
NVSexecute $prg/postProcessing -i $input -o .  -opt rel  -code $code -inundation true -time1 $time1 -time2 $time2 -locFileName locations_$ext -onlyLocations true  -tifuv2netcdf false 
      if [ $? -ne 0 ] ; then exit 1 ; fi
      if [ -e locations_$ext.kml ] ; then rm locations_$ext.kml ; fi
    fi
  let time=time+6
  done
