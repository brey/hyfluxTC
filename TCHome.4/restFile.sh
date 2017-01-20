#!/bin/bash 

#-------------------------------------------------
. functions.sh
export cmdChain=$cmdChain/`CMD $0`
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
vpl << EOF &> /dev/null

getmap out $batout
getmap mask $maskmap
landin = (.not.eqmv(mask).and.mask.lt.0)
landout = (.not.eqmv(out).and.out.lt.0)
%m=(.not.landin.and.landout).or.(.not.landout.and.landin)
%m=landin.or.landout
m=landout

if (sum(m).gt.0) then
l=getm(m,do(size(m)))
mask_l = MV
endif

putmap mask $maskmap
EOF

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
while [ $time -lt $timestart ] ; do
map=TIF_H_$ext.tif
if [ -f $in/$map  ] ; then 
NVSexecute resmap -clone $maskmap $mask -cloneFmt $clonetif -setdata 0 -i $in/$map -o $out/$map -int 0 
fi
if [ $? -ne 0 ] ; then echo error on resampling $map ; exit 1 ; fi
let time=time+dtout
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


