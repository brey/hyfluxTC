#!/bin/bash 


#-------------------------------------------------
. $procDir/functions.sh
export cmdChain=$cmdChain/`CMD $0`
#-------------------------------------------------

nestedWin="$*"

echo
echo +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
echo merge locations of selected time ranges: ouput  locations_\$ext.xml 
echo +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

let maxzmax=0
time=$timeShift

while [ $time -le $FinTime ] ; do

let timeh=time-timeShift
if [ $time -gt 0 ] ; then
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


let time=time+6
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
SSexecute $procDir/mergeLoc.sh 




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

